extern crate proc_macro;
#[macro_use]
extern crate procedural_masquerade;
extern crate syn;
#[macro_use]
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
		panic!("Item `{}` is not a struct.", item.ident);
	};

	let mut rattrs = item.attrs.clone();
	let is_mut = if let Some(rental_pos) = rattrs.iter().position(|a| if let syn::MetaItem::Word(ref ident) = a.value { ident == "rental" } else { false }) {
		rattrs.remove(rental_pos);
		false
	} else if let Some(rental_mut_pos) = rattrs.iter().position(|a| if let syn::MetaItem::Word(ref ident) = a.value { ident == "rental_mut" } else { false }) {
		rattrs.remove(rental_mut_pos);
		true
	} else {
		panic!("Struct `{}` must have `rental` or `rental_mut` attribute.", item.ident);
	};

	if !is_mut && item.ident.as_ref().ends_with("_Borrows") {
		panic!("Immutable rental struct `{}` name must not end with `_Borrows`.", item.ident);
	}

	let (is_tup, fields) = match *data {
		syn::VariantData::Struct(ref fields) => (false, fields),
		syn::VariantData::Tuple(ref fields) => (true, fields),
		_ => panic!("Struct `{}` must have fields.", item.ident),
	};

	let mut rfields = Vec::with_capacity(fields.len());
	for (field_idx, field) in fields.iter().enumerate() {
		if field.vis != syn::Visibility::Inherited {
			panic!(
				"Struct `{}` field `{}` must be private.",
				item.ident,
				field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
			);
		}

		let mut rfattrs = field.attrs.clone();
		let mut rlt_args = HashSet::new();
		let mut subrental_len = None;
		if let Some(subrental_pos) = rfattrs.iter().position(|a| match a.value {
			syn::MetaItem::Word(ref ident) => {
				if ident == "subrental" {
					panic!(
						"`subrental` attribute on struct `{}` field `{}` must have a single integer argument.",
						item.ident,
						field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
					);
				}

				false
			},
			syn::MetaItem::List(ref ident, ref nested) => {
				if ident == "subrental" {
					if let (Some(&syn::NestedMetaItem::Literal(syn::Lit::Int(sr_len, ..))), None) = (nested.get(0), nested.get(1)) {
						subrental_len = Some(sr_len as usize);
						true
					} else {
						panic!(
							"`subrental` attribute on struct `{}` field `{}` must have a single integer argument.",
							item.ident,
							field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
						);
					}
				} else { false }
			}
			_ => false,
		}) {
			rfattrs.remove(subrental_pos);
		}

		if let Some(subrental_len) = subrental_len {
			if let Some(ref field_ident) = field.ident {
				for subrental_idx in 0 .. subrental_len {
					rlt_args.insert(syn::Lifetime::new(format!("'{}_{}", field_ident, subrental_idx)));
				}
			} else {
				for subrental_idx in 0 .. subrental_len {
					rlt_args.insert(syn::Lifetime::new(format!("'_{}_{}", field_idx, subrental_idx)));
				}
			}
		} else {
			if let Some(ref field_ident) = field.ident {
				rlt_args.insert(syn::Lifetime::new(format!("'{}", field_ident)));
			} else {
				rlt_args.insert(syn::Lifetime::new(format!("'_{}", field_idx)));
			}
		}

		let rty = {
			let mut eraser = RentalLifetimeEraser{
				rfields: &rfields,
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

	let rlt_args = rfields.iter().fold(HashSet::new(), |mut rlt_args, rfield| { rlt_args.extend(rfield.rlt_args.iter().map(|lt| lt.clone())); rlt_args });
	if let Some(collide) = rlt_args.iter().find(|rlt_arg| gen.lifetimes.iter().any(|lt_def| lt_def.lifetime == **rlt_arg)) {
		panic!("Struct `{}` lifetime parameter `{}` collides with rental lifetime.", item.ident, collide.ident);
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

	rstruct.to_tokens(tokens);

	let item_ident = &item.ident;
	let item_vis = &item.vis;
	if !is_mut {
		let borrows_ident = syn::Ident::new(item_ident.to_string() + "_Borrows");
		let borrow_tys = fields.iter().map(|field| &field.ty);

		if is_tup {
			(quote!{
				#item_vis struct #borrows_ident(
					//#(& #borrow_tys),*
				);
			}).to_tokens(tokens);
		} else {
			let field_names = fields.iter().map(|field| field.ident.as_ref().unwrap());

			(quote!{
				#item_vis struct #borrows_ident {
					//#(#field_names: & #borrow_tys),*
				}
			}).to_tokens(tokens);
		}

		(quote!{
			impl #item_ident {
			}
		}).to_tokens(tokens);
	} else {
	}
}


struct RentalField {
	pub field: syn::Field,
	pub subrental_len: Option<usize>,
	pub rlt_args: HashSet<syn::Lifetime>,
}


struct RentalLifetimeEraser<'a> {
	pub rfields: &'a [RentalField],
}


impl<'a> syn::fold::Folder for RentalLifetimeEraser<'a> {
	fn fold_lifetime(&mut self, lifetime: syn::Lifetime) -> syn::Lifetime {
		if self.rfields.iter().any(|rfield| rfield.rlt_args.contains(&lifetime)) {
			syn::Lifetime::new("'static")
		} else {
			lifetime
		}
	}
}
