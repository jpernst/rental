extern crate proc_macro;
#[macro_use]
extern crate procedural_masquerade;
extern crate syn;
extern crate quote;

use std::collections::HashSet;
use quote::ToTokens;


define_proc_macros! {
	#[doc(hidden)]
	#[allow(non_snake_case)]
	pub fn rental__impl(input: &str) -> String {
		let mut tokens = quote::Tokens::new();

		for item in syn::parse_items(input).expect("Failed to parse items in module body.") {
			match item.node {
				syn::ItemKind::Use(..) => {
					item.to_tokens(&mut tokens);
				},
				syn::ItemKind::Struct(..) => {
					write_rental_struct_and_impls(&mut tokens, &item);
				},
				_ => panic!("Illegal item kind (only `use` and `struct` allowed)."),
			}
		}

		tokens.to_string()
	}
}


fn write_rental_struct_and_impls(mut tokens: &mut quote::Tokens, item: &syn::Item) {
	let (data, gen) = if let syn::ItemKind::Struct(ref data, ref gen) = item.node {
		(data, gen)
	} else {
		panic!("Item is not a struct.");
	};

	let mut rattrs = item.attrs.clone();
	let is_mut = if let Some(rental_pos) = rattrs.iter().position(|a| if let syn::MetaItem::Word(ref ident) = a.value { ident == "rental" } else { false }) {
		rattrs.remove(rental_pos);
		false
	} else if let Some(rental_mut_pos) = rattrs.iter().position(|a| if let syn::MetaItem::Word(ref ident) = a.value { ident == "rental_mut" } else { false }) {
		rattrs.remove(rental_mut_pos);
		true
	} else {
		panic!("Struct must have `rental` or `rental_mut` attribute");
	};

	let (is_tup, fields) = match *data {
		syn::VariantData::Struct(ref fields) => (false, fields),
		syn::VariantData::Tuple(ref fields) => (true, fields),
		_ => panic!("Struct must have fields."),
	};

	let mut rfields = Vec::with_capacity(fields.len());
	for (field_idx, field) in fields.iter().enumerate() {
		if field.vis != syn::Visibility::Inherited {
			panic!("Fields must be private.");
		}

		let mut rfattrs = field.attrs.clone();
		let mut subrental_len = None;
		if let Some(subrental_pos) = rfattrs.iter().position(|a| match a.value {
			syn::MetaItem::Word(ref ident) => {
				if ident == "subrental" {
					panic!("`subrental` attribute must have a single integer argument.");
				}

				false
			},
			syn::MetaItem::List(ref ident, ref nested) => {
				if ident == "subrental" {
					if let (Some(&syn::NestedMetaItem::Literal(syn::Lit::Int(sr_len, ..))), None) = (nested.get(0), nested.get(1)) {
						subrental_len = Some(sr_len as usize);
						true
					} else {
						panic!("`subrental` attribute must have a single integer argument.");
					}
				} else { false }
			}
			_ => false,
		}) {
			rfattrs.remove(subrental_pos);
		}

		let mut rlt_args = HashSet::new();
		let rty = {
			let mut eraser = RentalLifetimeEraser{
				rfields: &rfields,
				rlt_args: &mut rlt_args,
			};

			syn::fold::noop_fold_ty(&mut eraser, field.ty.clone())
		};

		rfields.push(RentalField{
			field: syn::Field{
				ident: field.ident.clone(),
				vis: field.vis.clone(),
				attrs: rfattrs,
				ty: rty,
			},
			subrental_len: subrental_len,
			rlt_args: rlt_args,
		});
	}

	let rstruct = syn::Item{
		ident: item.ident.clone(),
		vis: item.vis.clone(),
		attrs: rattrs,
		node: syn::ItemKind::Struct(
			if is_tup {
				syn::VariantData::Tuple(rfields.iter().map(|rf| rf.field.clone()).collect())
			} else {
				syn::VariantData::Struct(rfields.iter().map(|rf| rf.field.clone()).collect())
			},
			gen.clone()
		),
	};

	rstruct.to_tokens(&mut tokens);
}


struct RentalField {
	pub field: syn::Field,
	pub subrental_len: Option<usize>,
	pub rlt_args: HashSet<syn::Lifetime>,
}


struct RentalLifetimeEraser<'a, 'b> {
	pub rfields: &'a [RentalField],
	pub rlt_args: &'b mut HashSet<syn::Lifetime>,
}


impl<'a, 'b> syn::fold::Folder for RentalLifetimeEraser<'a, 'b> {
	fn fold_lifetime(&mut self, lifetime: syn::Lifetime) -> syn::Lifetime {
		let mut rlt_arg = None;
		'find_rlt_arg: for (field_idx, rfield) in self.rfields.iter().enumerate() {
			if let Some(ref field_ident) = rfield.field.ident {
				if let Some(subrental_len) = rfield.subrental_len {
					for subrental_idx in 0 .. subrental_len {
						let rlt = syn::Lifetime::new(format!("'{}_{}", field_ident, subrental_idx));
						if lifetime.ident == rlt.ident {
							rlt_arg = Some(rlt);
							break 'find_rlt_arg;
						}
					}
				} else {
					let rlt = syn::Lifetime::new(format!("'{}", field_ident));
					if lifetime.ident == rlt.ident {
						rlt_arg = Some(rlt);
						break 'find_rlt_arg;
					}
				}
			} else {
				if let Some(subrental_len) = rfield.subrental_len {
					for subrental_idx in 0 .. subrental_len {
						let rlt = syn::Lifetime::new(format!("'_{}_{}", field_idx, subrental_idx));
						if lifetime.ident == rlt.ident {
							rlt_arg = Some(rlt);
							break 'find_rlt_arg;
						}
					}
				} else {
					let rlt = syn::Lifetime::new(format!("'_{}", field_idx));
					if lifetime.ident == rlt.ident {
						rlt_arg = Some(rlt);
						break 'find_rlt_arg;
					}
				}
			}
		}

		if let Some(rlt_arg) = rlt_arg {
			self.rlt_args.insert(rlt_arg);
			syn::Lifetime::new("'static")
		} else {
			lifetime
		}
	}
}
