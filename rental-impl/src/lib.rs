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
	let mut is_mut = false;
	let mut borrows_ident = syn::Ident::new(item.ident.to_string() + "_Borrows");
	if let Some(rental_pos) = rattrs.iter().filter(|attr| !attr.is_sugared_doc).position(|attr| match attr.value {
		syn::MetaItem::Word(ref attr_ident) => {
			is_mut = match attr_ident.as_ref() {
				"rental" => false,
				"rental_mut" => true,
				_ => return false,
			};

			true
		},
		syn::MetaItem::List(ref attr_ident, ref nested) => {
			is_mut = match attr_ident.as_ref() {
				"rental" => false,
				"rental_mut" => true,
				_ => return false,
			};

			match (nested.get(0), nested.get(1)) {
				(
					Some(&syn::NestedMetaItem::MetaItem(syn::MetaItem::NameValue(ref borrows_str_name, syn::Lit::Str(ref borrows_str, ..)))),
					None
				) if borrows_str_name == "borrows" => {
					borrows_ident = syn::Ident::new(borrows_str.to_string());
				},
				_ => panic!(
					"`{}` attribute on struct `{}` expects `borrows = \"ident\"`.",
					if !is_mut { "rental" } else { "rental_mut" },
					item.ident
				),
			}

			true
		},
		_ => false,
	}) {
		rattrs.remove(rental_pos);
	} else {
		panic!("Struct `{}` must have a `rental` or `rental_mut` attribute.", item.ident);
	}

	if rattrs.iter().any(|attr| !attr.is_sugared_doc) {
		panic!("Struct `{}` must not have attributes other than `rental` or `rental_mut`.", item.ident);
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
		let mut subrental = None;
		if let Some(subrental_pos) = rfattrs.iter().position(|a| match a.value {
			syn::MetaItem::Word(ref ident) => {
				if ident == "subrental" {
					panic!(
						"`subrental` attribute on struct `{}` field `{}` expects `arity = int` and optionally `borrows = \"ty\"`.",
						item.ident,
						field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
					);
				} else { false }
			},
			syn::MetaItem::List(ref attr_ident, ref nested) => {
				if attr_ident == "subrental" {
					match (nested.get(0), nested.get(1), nested.get(2)) {
						(
							Some(&syn::NestedMetaItem::MetaItem(syn::MetaItem::NameValue(ref sr_arity_name, syn::Lit::Int(sr_arity, ..)))),
							None,
							None
						) if sr_arity_name == "arity" => {
							subrental = Some((sr_arity as usize, None));
							true
						},
						(
							Some(&syn::NestedMetaItem::MetaItem(syn::MetaItem::NameValue(ref sr_arity_name, syn::Lit::Int(sr_arity, ..)))),
							Some(&syn::NestedMetaItem::MetaItem(syn::MetaItem::NameValue(ref sr_borrows_name, syn::Lit::Str(ref sr_borrows, ..)))),
							None
						) | (
							Some(&syn::NestedMetaItem::MetaItem(syn::MetaItem::NameValue(ref sr_borrows_name, syn::Lit::Str(ref sr_borrows, ..)))),
							Some(&syn::NestedMetaItem::MetaItem(syn::MetaItem::NameValue(ref sr_arity_name, syn::Lit::Int(sr_arity, ..)))),
							None
						) if sr_arity_name == "arity" && sr_borrows_name == "borrows" => {
							if let Ok(sr_borrows) = syn::parse_path(sr_borrows) {
								subrental = Some((sr_arity as usize, Some(sr_borrows)));
								true
							} else {
								panic!(
									"`borrows` value of `subrental` attribute on struct `{}` field `{}` must be a type path.",
									item.ident,
									field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
								);
							}
						},
						_ => panic!(
							"`subrental` attribute on struct `{}` field `{}` expects `arity = int` and optionally `borrows = \"ty\"`.",
							item.ident,
							field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
						),
					}
				} else { false }
			},
			_ => false,
		}) {
			rfattrs.remove(subrental_pos);
		}

		if rfattrs.iter().any(|attr| !attr.is_sugared_doc) {
			panic!(
				"Struct `{}` field `{}` must not have attributes other than `subrental`.",
				item.ident,
				field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
			);
		}

		let mut rlt_args = HashSet::new();
		if let Some((subrental_arity, ..)) = subrental {
			if let Some(ref field_ident) = field.ident {
				for subrental_idx in 0 .. subrental_arity {
					rlt_args.insert(syn::Lifetime::new(format!("'{}_{}", field_ident, subrental_idx)));
				}
			} else {
				for subrental_idx in 0 .. subrental_arity {
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
			subrental: subrental,
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
			{
				let mut reverse_rfields: Vec<_> = rfields.iter().map(|rf| rf.field.clone()).collect();
				reverse_rfields.reverse();
				if is_tup {
					syn::VariantData::Tuple(reverse_rfields)
				} else {
					syn::VariantData::Struct(reverse_rfields)
				}
			},
			gen.clone()
		),
	};

	rstruct.to_tokens(tokens);

	let item_ident = &item.ident;
	let item_vis = &item.vis;
	if !is_mut {
		let borrow_tys = rfields.iter().map(|rfield| {
		});

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
	pub subrental: Option<(usize, Option<syn::Path>)>,
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
