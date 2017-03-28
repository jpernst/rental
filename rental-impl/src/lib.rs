#![recursion_limit = "256"]

extern crate proc_macro;
#[macro_use]
extern crate procedural_masquerade;
extern crate syn;
#[macro_use]
extern crate quote;

use std::iter;
use quote::ToTokens;


define_proc_macros! {
	#[doc(hidden)]
	#[allow(non_snake_case)]
	pub fn __rental_impl(input: &str) -> String {
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
	let (struct_data, struct_generics) = if let syn::ItemKind::Struct(ref struct_data, ref struct_generics) = item.node {
		(struct_data, struct_generics)
	} else {
		panic!("Item `{}` is not a struct.", item.ident);
	};

	let mut rattrs = item.attrs.clone();
	let mut is_rental_mut = false;
	if let Some(rental_pos) = rattrs.iter().filter(|attr| !attr.is_sugared_doc).position(|attr| match attr.value {
		syn::MetaItem::Word(ref attr_ident) => {
			is_rental_mut = match attr_ident.as_ref() {
				"rental" => false,
				"rental_mut" => true,
				_ => return false,
			};

			true
		},
		syn::MetaItem::List(ref attr_ident, ref nested) => {
			is_rental_mut = match attr_ident.as_ref() {
				"rental" => false,
				"rental_mut" => true,
				_ => return false,
			};

			if nested.get(0).is_some() {
				panic!(
					"`{}` attribute on struct `{}` must have no arguments.",
					if !is_rental_mut { "rental" } else { "rental_mut" },
					item.ident
				);
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

	let (is_tup, fields) = match *struct_data {
		syn::VariantData::Struct(ref fields) => (false, fields),
		syn::VariantData::Tuple(ref fields) => (true, fields),
		_ => panic!("Struct `{}` must have at least 2 fields.", item.ident),
	};

	if fields.len() < 2 {
		panic!("Struct `{}` must have at least 2 fields.", item.ident);
	}

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
		if let Some(sr_pos) = rfattrs.iter().position(|a| match a.value {
			syn::MetaItem::Word(ref ident) => {
				if ident == "subrental" {
					panic!(
						"`subrental` attribute on struct `{}` field `{}` expects `arity = int`.",
						item.ident,
						field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
					);
				} else { false }
			},
			syn::MetaItem::List(ref attr_ident, ref nested) => {
				if attr_ident == "subrental" {
					match (nested.get(0), nested.get(1)) {
						(
							Some(&syn::NestedMetaItem::MetaItem(syn::MetaItem::NameValue(ref sr_arity_name, syn::Lit::Int(sr_arity, ..)))),
							None
						) if sr_arity_name == "arity" => {
							if let syn::Ty::Path(ref qself, ref sr_borrow) = field.ty {
								let last_idx = sr_borrow.segments.len() - 1;

								let mut sr_borrow = sr_borrow.clone();
								sr_borrow.segments[last_idx].ident = syn::Ident::new(sr_borrow.segments[0].ident.to_string() + "_Borrow");
								sr_borrow.segments[last_idx].parameters = syn::PathParameters::none();

								let mut sr_borrow_mut = sr_borrow.clone();
								sr_borrow_mut.segments[last_idx].ident = syn::Ident::new(sr_borrow.segments[0].ident.to_string() + "_BorrowMut");
								sr_borrow_mut.segments[last_idx].parameters = syn::PathParameters::none();

								subrental = Some((sr_arity as usize, syn::Ty::Path(qself.clone(), sr_borrow), syn::Ty::Path(qself.clone(), sr_borrow_mut)));
							} else {
								panic!(
									"Struct `{}` subrental field `{}` must be a type path.",
									item.ident,
									field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
								);
							}
						},
						_ => panic!(
							"`subrental` attribute on struct `{}` field `{}` expects `arity = int`.",
							item.ident,
							field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
						),
					}

					true
				} else { false }
			},
			_ => false,
		}) {
			rfattrs.remove(sr_pos);
		}

		if rfattrs.iter().any(|attr| !attr.is_sugared_doc) {
			panic!(
				"Struct `{}` field `{}` must not have attributes other than `subrental`.",
				item.ident,
				field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
			);
		}

		let mut self_rlt_args = Vec::new();
		if let Some((sr_arity, ..)) = subrental {
			if let Some(ref field_ident) = field.ident {
				for sr_idx in 0 .. sr_arity {
					self_rlt_args.push(syn::Lifetime::new(format!("'{}_{}", field_ident, sr_idx)));
				}
			} else {
				for sr_idx in 0 .. sr_arity {
					self_rlt_args.push(syn::Lifetime::new(format!("'_{}_{}", field_idx, sr_idx)));
				}
			}
		} else {
			if let Some(ref field_ident) = field.ident {
				self_rlt_args.push(syn::Lifetime::new(format!("'{}", field_ident)));
			} else {
				self_rlt_args.push(syn::Lifetime::new(format!("'_{}", field_idx)));
			}
		}

		let mut used_rlt_args = Vec::new();
		let rty = {
			let mut eraser = RentalLifetimeEraser{
				rfields: &rfields,
				used_rlt_args: &mut used_rlt_args,
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
			self_rlt_args: self_rlt_args,
			used_rlt_args: used_rlt_args,
		});
	}

	let struct_rlt_args = &rfields.iter().fold(Vec::new(), |mut rlt_args, rfield| { rlt_args.extend(rfield.self_rlt_args.iter()); rlt_args });
	if let Some(collide) = struct_rlt_args.iter().find(|rlt_arg| struct_generics.lifetimes.iter().any(|lt_def| lt_def.lifetime == ***rlt_arg)) {
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
			struct_generics.clone()
		),
	};

	rstruct.to_tokens(tokens);

	let item_ident = &item.ident;
	let item_vis = &item.vis;

	let (struct_impl_params, struct_impl_args, struct_where_clause) = struct_generics.split_for_impl();
	let struct_lt_params = &struct_generics.lifetimes;
	let struct_ty_params = &struct_generics.ty_params;
	let struct_lt_args = &struct_lt_params.iter().map(|lt_def| &lt_def.lifetime).collect::<Vec<_>>();
	let struct_ty_args = &struct_ty_params.iter().map(|ty_param| &ty_param.ident).collect::<Vec<_>>();
	let struct_fake_rlt_args = &iter::repeat(syn::Lifetime::new("'__a")).take(struct_rlt_args.len()).collect::<Vec<_>>();
	let struct_tail_rlt_args = &rfields[fields.len() - 1].self_rlt_args.iter().chain(rfields[fields.len() - 1].used_rlt_args.iter()).collect::<Vec<_>>();
	let field_idents = &fields.iter().map(|field| field.ident.as_ref().unwrap()).collect::<Vec<_>>();
	let struct_prefix_field_tys = &rfields.iter().map(|rfield| &rfield.field.ty).take(fields.len() - 1).collect::<Vec<_>>();

	let borrow_ident = syn::Ident::new(item.ident.to_string() + "_Borrow");
	let borrow_mut_ident = syn::Ident::new(item.ident.to_string() + "_BorrowMut");
	let borrow_tys_and_exprs = &make_borrow_tys_and_exprs(item, &rfields, &fields, is_rental_mut, is_tup, false);
	let borrow_tys = &borrow_tys_and_exprs.iter().map(|&(ref ty, _)| ty).collect::<Vec<_>>();
	let borrow_tail_ty = &borrow_tys[fields.len() - 1];
	let borrow_exprs = &borrow_tys_and_exprs.iter().map(|&(_, ref expr)| expr).collect::<Vec<_>>();
	let borrow_tail_expr = &borrow_exprs[fields.len() - 1];
	let borrow_mut_tys_and_exprs = &make_borrow_tys_and_exprs(item, &rfields, &fields, is_rental_mut, is_tup, true);
	let borrow_mut_tys = &borrow_mut_tys_and_exprs.iter().map(|&(ref ty, _)| ty).collect::<Vec<_>>();
	let borrow_mut_tail_ty = &borrow_mut_tys[fields.len() - 1];
	let borrow_mut_exprs = &borrow_mut_tys_and_exprs.iter().map(|&(_, ref expr)| expr).collect::<Vec<_>>();
	let borrow_mut_tail_expr = &borrow_mut_exprs[fields.len() - 1];

	let borrow_lt_params = &struct_lt_params.iter().map(|lt_def| {
		let mut lt_def = (*lt_def).clone();
		lt_def.bounds.push(struct_rlt_args[0].clone());
		lt_def
	}).chain(struct_rlt_args.iter().zip(struct_rlt_args.iter().skip(1)).map(|(rlt_arg, next_rlt_arg)| {
		syn::LifetimeDef {
			attrs: Vec::with_capacity(0),
			lifetime: (**rlt_arg).clone(),
			bounds: vec![(**next_rlt_arg).clone()],
		}
	})).chain(Some(syn::LifetimeDef {
			attrs: Vec::with_capacity(0),
			lifetime: struct_rlt_args[struct_rlt_args.len() - 1].clone(),
			bounds: Vec::with_capacity(0),
	})).collect::<Vec<_>>();

	if !is_rental_mut {
		if is_tup {
			quote!(
				#[allow(non_camel_case_types, non_snake_case, dead_code)]
				#item_vis struct #borrow_ident<#(#borrow_lt_params,)* #(#struct_ty_params),*>(
					#(pub #borrow_tys),*
				) #struct_where_clause;

				#[allow(non_camel_case_types, non_snake_case, dead_code)]
				#item_vis struct #borrow_mut_ident<#(#borrow_lt_params,)* #(#struct_ty_params),*>(
					#(pub #borrow_mut_tys),*
				) #struct_where_clause;
			).to_tokens(tokens);
		} else {
			quote!(
				#[allow(non_camel_case_types, non_snake_case, dead_code)]
				#item_vis struct #borrow_ident<#(#borrow_lt_params,)* #(#struct_ty_params),*> #struct_where_clause {
					#(pub #field_idents: #borrow_tys,)*
				}

				#[allow(non_camel_case_types, non_snake_case, dead_code)]
				#item_vis struct #borrow_mut_ident<#(#borrow_lt_params,)* #(#struct_ty_params),*> #struct_where_clause {
					#(pub #field_idents: #borrow_mut_tys,)*
				}
			).to_tokens(tokens);
		}

		quote!(
			#[allow(dead_code)]
			impl #struct_impl_params #item_ident #struct_impl_args #struct_where_clause {
				pub fn new(
				) where
				{
					#(__rental_prelude::static_assert_stable_deref::<#struct_prefix_field_tys>();)*
				}

				pub fn rent_all<__F, __R>(&self, f: __F) -> __R where
					__F: for<#(#struct_rlt_args,)*> FnOnce(#borrow_ident<#(#struct_lt_args,)* #(#struct_rlt_args,)* #(#struct_ty_args),*>) -> __R,
					__R: #(#struct_lt_args +)*,
				{
					f(unsafe { self.borrow() })
				}

				pub fn rent_all_mut<__F, __R>(&mut self, f: __F) -> __R where
					__F: for<#(#struct_rlt_args,)*> FnOnce(#borrow_mut_ident<#(#struct_lt_args,)* #(#struct_rlt_args,)* #(#struct_ty_args),*>) -> __R,
					__R: #(#struct_lt_args +)*,
				{
					f(unsafe { self.borrow_mut() })
				}
			}
		).to_tokens(tokens);
	} else {
	}

	quote!(
		#[allow(dead_code, unused_unsafe)]
		impl #struct_impl_params #item_ident #struct_impl_args #struct_where_clause {
			pub fn rent<__F, __R>(&self, f: __F) -> __R where
				__F: for<#(#struct_tail_rlt_args,)*> FnOnce(#borrow_tail_ty) -> __R,
				__R: #(#struct_lt_args +)*,
			{
				f(#borrow_tail_expr)
			}

			pub fn rent_mut<__F, __R>(&mut self, f: __F) -> __R where
				__F: for<#(#struct_tail_rlt_args,)*> FnOnce(#borrow_mut_tail_ty) -> __R,
				__R: #(#struct_lt_args +)*,
			{
				f(#borrow_mut_tail_expr)
			}

			pub unsafe fn borrow<'__a>(&'__a self) -> #borrow_ident<#(#struct_lt_args,)* #(#struct_fake_rlt_args,)* #(#struct_ty_args),*> {
				#borrow_ident {
					#(#field_idents: #borrow_exprs,)*
				}
			}

			pub unsafe fn borrow_mut<'__a>(&'__a mut self) -> #borrow_mut_ident<#(#struct_lt_args,)* #(#struct_fake_rlt_args,)* #(#struct_ty_args),*> {
				#borrow_mut_ident {
					#(#field_idents: #borrow_mut_exprs,)*
				}
			}
		}
	).to_tokens(tokens);
}


fn make_borrow_tys_and_exprs(item: &syn::Item, rfields: &[RentalField], fields: &[syn::Field], is_rental_mut: bool, is_tup: bool, is_borrow_mut: bool) -> Vec<(quote::Tokens, quote::Tokens)> {
	rfields.iter().zip(fields.iter()).enumerate().map(|(field_idx, (rfield, field))| {
		if let Some(ref subrental) = rfield.subrental {
			let (rfield_lt_args, rfield_ty_args) = if let syn::Ty::Path(_, ref field_path) = field.ty {
				let last_idx = field_path.segments.len();
				if let syn::PathParameters::AngleBracketed(ref bracket_params) = field_path.segments[last_idx].parameters {
					(&bracket_params.lifetimes, &bracket_params.types)
				} else {
					panic!(
						"Parameters on struct `{}` subrental field `{}` must be angle-bracketed.",
						item.ident,
						field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
					);
				}
			} else {
				panic!(
					"Struct `{}` subrental field `{}` must be a type path.",
					item.ident,
					field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
				);
			};

			let rfield_rlt_args = &rfield.self_rlt_args;
			let sr_borrow = &subrental.1;
			let sr_borrow_mut = &subrental.2;

			if field_idx == fields.len() - 1 && is_borrow_mut {
				(
					quote!(#sr_borrow_mut<#(#rfield_lt_args,)* #(#rfield_rlt_args,)* #(#rfield_ty_args),*>),
					if is_tup {
						quote!(unsafe { self.#field_idx.borrow_mut() })
					} else {
						let field_ident = field.ident.as_ref().unwrap();
						quote!(unsafe { self.#field_ident.borrow_mut() })
					}
				)
			} else if field_idx == fields.len() - 1 || !is_rental_mut {
				(
					quote!(#sr_borrow<#(#rfield_lt_args,)* #(#rfield_rlt_args,)* #(#rfield_ty_args),*>),
					if is_tup {
						quote!(unsafe { self.#field_idx.borrow() })
					} else {
						let field_ident = field.ident.as_ref().unwrap();
						quote!(unsafe { self.#field_ident.borrow() })
					}
				)
			} else {
				(
					quote!(__rental_prelude::PhantomData<#sr_borrow<#(#rfield_lt_args,)* #(#rfield_rlt_args,)* #(#rfield_ty_args),*>>),
					quote!(__rental_prelude::PhantomData)
				)
			}
		} else {
			let rfield_rlt_arg = &rfield.self_rlt_args[0];
			let field_ty = &field.ty;

			if field_idx == fields.len() - 1 && is_borrow_mut {
				(
					quote!(&#rfield_rlt_arg mut #field_ty),
					if is_tup {
						quote!(unsafe { __rental_prelude::transmute(&mut self.#field_idx) })
					} else {
						let field_ident = field.ident.as_ref().unwrap();
						quote!(unsafe { __rental_prelude::transmute(&mut self.#field_ident) })
					}
				)
			} else if field_idx == fields.len() - 1 || !is_rental_mut {
				(
					quote!(&#rfield_rlt_arg #field_ty),
					if is_tup {
						quote!(unsafe { __rental_prelude::transmute(&self.#field_idx) })
					} else {
						let field_ident = field.ident.as_ref().unwrap();
						quote!(unsafe { __rental_prelude::transmute(&self.#field_ident) })
					}
				)
			} else {
				(
					quote!(__rental_prelude::PhantomData<&#rfield_rlt_arg #field_ty>),
					quote!(__rental_prelude::PhantomData)
				)
			}
		}
	}).collect()
}


struct RentalField {
	pub field: syn::Field,
	pub subrental: Option<(usize, syn::Ty, syn::Ty)>,
	pub self_rlt_args: Vec<syn::Lifetime>,
	pub used_rlt_args: Vec<syn::Lifetime>,
}


struct RentalLifetimeEraser<'a> {
	pub rfields: &'a [RentalField],
	pub used_rlt_args: &'a mut Vec<syn::Lifetime>,
}


impl<'a> syn::fold::Folder for RentalLifetimeEraser<'a> {
	fn fold_lifetime(&mut self, lifetime: syn::Lifetime) -> syn::Lifetime {
		if self.rfields.iter().any(|rfield| rfield.self_rlt_args.contains(&lifetime)) {
			if !self.used_rlt_args.contains(&lifetime) {
				self.used_rlt_args.push(lifetime.clone());
			}

			syn::Lifetime::new("'static")
		} else {
			lifetime
		}
	}
}
