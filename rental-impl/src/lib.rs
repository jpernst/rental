#![recursion_limit = "512"]

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
	pub fn __rental_traits(input: &str) -> String {
		let mut tokens = quote::Tokens::new();

		let max_arity = input.parse::<usize>().expect("Input must be an integer literal.");
		write_rental_traits(&mut tokens, max_arity);

		tokens.to_string()
	}


	#[doc(hidden)]
	#[allow(non_snake_case)]
	pub fn __rental_structs_and_impls(input: &str) -> String {
		let mut tokens = quote::Tokens::new();

		for item in syn::parse_items(input).expect("Failed to parse items in module body.") {
			match item.node {
				syn::ItemKind::Use(..) => {
					item.to_tokens(&mut tokens);
				},
				syn::ItemKind::Struct(..) => {
					write_rental_struct_and_impls(&mut tokens, &item);
				},
				_ => panic!("Item must be a `use` or `struct`."),
			}
		}

		tokens.to_string()
	}
}


fn write_rental_traits(tokens: &mut quote::Tokens, max_arity: usize) {
	let mut lt_params = vec![syn::LifetimeDef::new("'a0")];

	for arity in 2 .. max_arity + 1 {
		let trait_ident = &syn::Ident::new(format!("Rental{}", arity));
		let lt_param = syn::LifetimeDef::new(format!("'a{}", arity - 1));
		lt_params[arity - 2].bounds.push(lt_param.lifetime.clone());
		lt_params.push(lt_param);

		let lt_params_iter = &lt_params;
		quote!(
			#[doc(hidden)]
			pub unsafe trait #trait_ident<#(#lt_params_iter),*> {
				type Borrow;
				type BorrowMut;
			}
		).to_tokens(tokens);
	}
}


fn write_rental_struct_and_impls(tokens: &mut quote::Tokens, item: &syn::Item) {
	let (struct_data, struct_generics) = if let syn::ItemKind::Struct(ref struct_data, ref struct_generics) = item.node {
		(struct_data, struct_generics)
	} else {
		panic!("Item `{}` is not a struct.", item.ident);
	};

	if item.vis == syn::Visibility::Inherited {
		panic!("Struct `{}` must be non-private.", item.ident);
	}

	let mut rattrs = item.attrs.clone();
	let mut is_rental_mut = false;
	let mut is_deref_suffix = false;
	let mut is_deref_mut_suffix = false;
	let mut is_debug_borrow = false;
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

			let mut nested = nested.clone();
			nested.retain(|nested| {
				if let syn::NestedMetaItem::MetaItem(ref item) = *nested {
					match *item {
						syn::MetaItem::Word(ref ident) => {
							match ident.as_ref() {
								"debug_borrow" => {
									is_debug_borrow = true;
									false
								},
								"deref_suffix" => {
									is_deref_suffix = true;
									false
								},
								"deref_mut_suffix" => {
									is_deref_suffix = true;
									is_deref_mut_suffix = true;
									false
								},
								_ => true,
							}
						},
						_ => true,
					}
				} else {
					true
				}
			});

			if nested.len() > 0 {
				panic!("Struct `{}` rental attribute takes optional arguments: `debug_borrow`, `deref_suffix`, and `deref_mut_suffix`.", item.ident);
			}

			true
		},
		_ => false,
	}) {
		rattrs.remove(rental_pos);
	} else {
		panic!("Struct `{}` must have a `rental` or `rental_mut` attribute.", item.ident);
	}

	if rattrs.iter().any(|attr| match attr.value { syn::MetaItem::NameValue(ref ident, ..) if ident == "doc" => false, _ => true }) {
		panic!("Struct `{}` must not have attributes other than one `rental` or `rental_mut`.", item.ident);
	}

	let fields = match *struct_data {
		syn::VariantData::Struct(ref fields) => fields,
		syn::VariantData::Tuple(..) => panic!("Struct `{}` must not be a tuple struct.", item.ident),
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
		let mut target_ty_hack = None;
		if let Some(sr_pos) = rfattrs.iter().position(|a| match a.value {
			syn::MetaItem::List(ref attr_ident, ref nested) => {
				if attr_ident == "subrental" {
					match (nested.get(0), nested.get(1)) {
						(
							Some(&syn::NestedMetaItem::MetaItem(syn::MetaItem::NameValue(ref sr_arity_name, syn::Lit::Int(sr_arity, ..)))),
							None
						) if sr_arity_name == "arity" => {
							subrental = Some(Subrental{
								arity: sr_arity as usize, 
								rental_trait_ident: syn::Ident::new(format!("Rental{}", sr_arity).as_str()),
							})
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
			syn::MetaItem::Word(ref ident) | syn::MetaItem::NameValue(ref ident, ..) => {
				if ident == "subrental" {
					panic!(
						"`subrental` attribute on struct `{}` field `{}` expects `arity = int`.",
						item.ident,
						field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
					);
				} else { false }
			},
		}) {
			rfattrs.remove(sr_pos);
		}

		if let Some(tth_pos) = rfattrs.iter().position(|a| match a.value {
			syn::MetaItem::NameValue(ref name, ref value) => {
				if name == "target_ty_hack" {
					if let syn::Lit::Str(ref ty_str, ..) = *value {
						if let Ok(ty) = syn::parse_type(ty_str) {
							target_ty_hack = Some(ty);
						}
					} 

					if target_ty_hack.is_none() {
						panic!(
							"`target_ty_hack` attribute on struct `{}` field `{}` expects a ty string.",
							item.ident,
							field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
						);
					}

					true
				} else { false }
			},
			syn::MetaItem::Word(ref name) | syn::MetaItem::List(ref name, ..) => {
				if name == "target_ty_hack" {
					panic!(
						"`target_ty_hack` attribute on struct `{}` field `{}` expects a ty string.",
						item.ident,
						field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
					);
				} else { false }
			},
		}) {
			rfattrs.remove(tth_pos);
		}

		if rfattrs.iter().any(|attr| match attr.value { syn::MetaItem::NameValue(ref ident, ..) if ident == "doc" => false, _ => true }) {
			panic!(
				"Struct `{}` field `{}` must not have attributes other than one `subrental` and `target_ty_hack`.",
				item.ident,
				field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
			);
		}

		let mut self_rlt_args = Vec::new();
		if let Some(Subrental{arity: sr_arity, ..}) = subrental {
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
				fields: &rfields,
				used_rlt_args: &mut used_rlt_args,
			};

			syn::fold::noop_fold_ty(&mut eraser, field.ty.clone())
		};

		let target_ty_hack = target_ty_hack.as_ref().map(|ty| (*ty).clone()).or_else(|| if field_idx < fields.len() - 1 {
			if let syn::Ty::Path(_, ref ty_path) = field.ty {
				if let syn::PathSegment{ref ident, parameters: syn::PathParameters::AngleBracketed(ref params)} = ty_path.segments[ty_path.segments.len() - 1] {
					if params.types.len() == 1 {
						if ident == "Vec" {
							Some(syn::Ty::Slice(Box::new(params.types[0].clone())))
						} else {
							Some(params.types[0].clone())
						}
					} else if ident == "String" && params.types.len() == 0 {
						Some(syn::parse_type("str").unwrap())
					} else {
						panic!("Field `{}` must have an angle-bracketed type parameter or be `String`.", field.ident.as_ref().unwrap())
					}
				} else {
					panic!("Field `{}` must have an angle-bracketed parameter or be `String`.", field.ident.as_ref().unwrap())
				}
			} else {
				panic!("Field `{}` must be a type path.", field.ident.as_ref().unwrap())
			}
		} else {
			None
		});

		rfields.push(RentalField{
			name: if let Some(ref ident) = field.ident {
				(*ident).clone()
			} else {
				syn::Ident::new(format!("v{}", field_idx.to_string()))
			},
			orig_ty: field.ty.clone(),
			erased: syn::Field{
				ident: field.ident.clone(),
				vis: field.vis.clone(),
				attrs: rfattrs,
				ty: rty,
			},
			subrental: subrental,
			self_rlt_args: self_rlt_args,
			used_rlt_args: used_rlt_args,
			target_ty_hack: target_ty_hack,
		});
	}
	let fields = rfields;

	let struct_rlt_args = &fields.iter().fold(Vec::new(), |mut rlt_args, field| { rlt_args.extend(field.self_rlt_args.iter()); rlt_args });
	if let Some(collide) = struct_rlt_args.iter().find(|rlt_arg| struct_generics.lifetimes.iter().any(|lt_def| lt_def.lifetime == ***rlt_arg)) {
		panic!("Struct `{}` lifetime parameter `{}` collides with rental lifetime.", item.ident, collide.ident);
	}
	let last_rlt_arg = &struct_rlt_args[struct_rlt_args.len() - 1];

	let item_ident = &item.ident;
	let item_vis = &item.vis;

	let (struct_impl_params, struct_impl_args, struct_where_clause) = struct_generics.split_for_impl();
	let struct_lt_params = &struct_generics.lifetimes;
	let struct_ty_params = &struct_generics.ty_params;
	let struct_lt_args = &struct_lt_params.iter().map(|lt_def| &lt_def.lifetime).collect::<Vec<_>>();
	let struct_ty_args = &struct_ty_params.iter().map(|ty_param| &ty_param.ident).collect::<Vec<_>>();
	let struct_fake_rlt_arg = &syn::Lifetime::new("'__a");
	let struct_fake_rlt_args = &iter::repeat(struct_fake_rlt_arg).take(struct_rlt_args.len()).collect::<Vec<_>>();

	let rental_trait_ident = syn::Ident::new(format!("Rental{}", struct_rlt_args.len()));
	let field_idents = &fields.iter().map(|field| &field.name).collect::<Vec<_>>();
	let field_idents_reverse = &field_idents.iter().rev().collect::<Vec<_>>();;
	let local_idents = field_idents;

	let borrow_ident = syn::Ident::new(item.ident.to_string() + "_Borrow");
	let borrow_mut_ident = syn::Ident::new(item.ident.to_string() + "_BorrowMut");
	let borrow_quotes = &make_borrow_quotes(&fields, is_rental_mut);
	let borrow_tys = &borrow_quotes.iter().map(|&BorrowQuotes{ref ty, ..}| ty).collect::<Vec<_>>();
	let borrow_ty_hacks = &borrow_quotes.iter().map(|&BorrowQuotes{ref ty_hack, ..}| ty_hack).collect::<Vec<_>>();
	let borrow_suffix_ty = &borrow_tys[fields.len() - 1];
	let borrow_exprs = &borrow_quotes.iter().map(|&BorrowQuotes{ref expr, ..}| expr).collect::<Vec<_>>();
	let borrow_suffix_expr = &borrow_exprs[fields.len() - 1];
	let borrow_mut_tys = &borrow_quotes.iter().map(|&BorrowQuotes{ref mut_ty, ..}| mut_ty).collect::<Vec<_>>();
	let borrow_mut_ty_hacks = &borrow_quotes.iter().map(|&BorrowQuotes{ref mut_ty_hack, ..}| mut_ty_hack).collect::<Vec<_>>();
	let borrow_mut_suffix_ty = &borrow_mut_tys[fields.len() - 1];
	let borrow_mut_exprs = &borrow_quotes.iter().map(|&BorrowQuotes{ref mut_expr, ..}| mut_expr).collect::<Vec<_>>();
	let borrow_mut_suffix_expr = &borrow_mut_exprs[fields.len() - 1];

	let struct_rlt_params = &struct_rlt_args.iter().zip(struct_rlt_args.iter().skip(1)).map(|(rlt_arg, next_rlt_arg)| {
		syn::LifetimeDef {
			attrs: Vec::with_capacity(0),
			lifetime: (**rlt_arg).clone(),
			bounds: vec![(**next_rlt_arg).clone()],
		}
	}).chain(Some(syn::LifetimeDef {
			attrs: Vec::with_capacity(0),
			lifetime: struct_rlt_args[struct_rlt_args.len() - 1].clone(),
			bounds: Vec::with_capacity(0),
	})).collect::<Vec<_>>();
	let borrow_lt_params = &struct_lt_params.iter().map(|lt_def| {
		let mut lt_def = (*lt_def).clone();
		lt_def.bounds.push(struct_rlt_args[0].clone());
		lt_def
	}).chain(struct_rlt_params.iter().cloned()).collect::<Vec<_>>();

	let head_ident = &local_idents[0];
	let head_ident_rep = &iter::repeat(&head_ident).take(fields.len() - 1).collect::<Vec<_>>();
	let head_ty = &fields[0].orig_ty;
	let prefix_field_tys = &fields.iter().map(|field| &field.erased.ty).take(fields.len() - 1).collect::<Vec<_>>();
	let tail_field_tys = &fields.iter().map(|field| &field.erased.ty).skip(1).collect::<Vec<_>>();
	let tail_field_idents_reverse = &field_idents.iter().skip(1).rev().collect::<Vec<_>>();
	let tail_idents = &local_idents.iter().skip(1).collect::<Vec<_>>();
	let tail_closure_tys = &fields.iter().skip(1).map(|field| syn::Ident::new(format!("__F{}", field.name))).collect::<Vec<_>>();
	let tail_closure_quotes = make_tail_closure_quotes(&fields, borrow_quotes, is_rental_mut);
	let tail_closure_bounds = &tail_closure_quotes.iter().map(|&ClosureQuotes{ref bound, ..}| bound).collect::<Vec<_>>();
	let tail_closure_exprs = &tail_closure_quotes.iter().map(|&ClosureQuotes{ref expr, ..}| expr).collect::<Vec<_>>();
	let tail_try_closure_bounds = &tail_closure_quotes.iter().map(|&ClosureQuotes{ref try_bound, ..}| try_bound).collect::<Vec<_>>();
	let tail_try_closure_exprs = &tail_closure_quotes.iter().map(|&ClosureQuotes{ref try_expr, ..}| try_expr).collect::<Vec<_>>();
	let suffix_field_ident = &field_idents[fields.len() - 1];
	let suffix_field_ty = &fields[fields.len() - 1].erased.ty;
	let suffix_rlt_args = &fields[fields.len() - 1].self_rlt_args.iter().chain(fields[fields.len() - 1].used_rlt_args.iter()).collect::<Vec<_>>();

	let rstruct = syn::Item{
		ident: item.ident.clone(),
		vis: item.vis.clone(),
		attrs: rattrs,
		node: syn::ItemKind::Struct(
			syn::VariantData::Struct(fields.iter().map(|field| {
				let field_erased_ty = &field.erased.ty;
				let mut field_erased = field.erased.clone();
				field_erased.ty = syn::parse_type(&quote!(__rental_prelude::Option<#field_erased_ty>).to_string()).unwrap();
				field_erased
			}).collect()),
			struct_generics.clone()
		),
	};

	let borrow_struct = syn::Item{
		ident: borrow_ident.clone(),
		vis: item.vis.clone(),
		attrs: Vec::with_capacity(0),
		node: syn::ItemKind::Struct(
			syn::VariantData::Struct(fields.iter().zip(borrow_tys).enumerate().map(|(idx, (field, borrow_ty))| {
				let mut field = field.erased.clone();
				field.vis = if !is_rental_mut || idx == fields.len() - 1 { (*item_vis).clone() } else { syn::Visibility::Inherited };
				field.ty = syn::parse_type(&borrow_ty.to_string()).unwrap();
				field
			}).collect()),
			{
				let mut gen = struct_generics.clone();
				gen.lifetimes = borrow_lt_params.clone();
				gen
			}
		),
	};

	let borrow_mut_struct = syn::Item{
		ident: borrow_mut_ident.clone(),
		vis: item.vis.clone(),
		attrs: Vec::with_capacity(0),
		node: syn::ItemKind::Struct(
			syn::VariantData::Struct(fields.iter().zip(borrow_mut_tys).enumerate().map(|(idx, (field, borrow_mut_ty))| {
				let mut field = field.erased.clone();
				field.vis = if idx == fields.len() - 1 || !is_rental_mut { (*item_vis).clone() } else { syn::Visibility::Inherited };
				field.ty = syn::parse_type(&borrow_mut_ty.to_string()).unwrap();
				field
			}).collect()),
			{
				let mut gen = struct_generics.clone();
				gen.lifetimes = borrow_lt_params.clone();
				gen
			}
		),
	};

	let derives = if is_debug_borrow {
		quote!(#[derive(Debug)])
	} else {
		quote!()
	};

	quote!(
		#rstruct

		/// Shared borrow of a rental struct.
		#[allow(non_camel_case_types, non_snake_case, dead_code)]
		#derives
		#borrow_struct

		/// Mutable borrow of a rental struct.
		#[allow(non_camel_case_types, non_snake_case, dead_code)]
		#derives
		#borrow_mut_struct

		#[allow(dead_code)]
		impl<#(#borrow_lt_params,)* #(#struct_ty_params),*> #borrow_ident<#(#struct_lt_args,)* #(#struct_rlt_args,)* #(#struct_ty_args),*> #struct_where_clause {
			fn unify_hack_tys(#(#local_idents: #borrow_ty_hacks),*) -> #borrow_ident<#(#struct_lt_args,)* #(#struct_rlt_args,)* #(#struct_ty_args),*> {
				#borrow_ident {
					#(#field_idents: #local_idents,)*
				}
			}
		}

		#[allow(dead_code)]
		impl<#(#borrow_lt_params,)* #(#struct_ty_params),*> #borrow_mut_ident<#(#struct_lt_args,)* #(#struct_rlt_args,)* #(#struct_ty_args),*> #struct_where_clause {
			fn unify_hack_tys(#(#local_idents: #borrow_mut_ty_hacks),*) -> #borrow_mut_ident<#(#struct_lt_args,)* #(#struct_rlt_args,)* #(#struct_ty_args),*> {
				#borrow_mut_ident {
					#(#field_idents: #local_idents,)*
				}
			}
		}

		unsafe impl<#(#borrow_lt_params,)* #(#struct_ty_params),*> __rental_prelude::#rental_trait_ident<#(#struct_rlt_args),*> for #item_ident #struct_impl_args #struct_where_clause {
			type Borrow = #borrow_ident<#(#struct_lt_args,)* #(#struct_rlt_args,)* #(#struct_ty_args),*>;
			type BorrowMut = #borrow_mut_ident<#(#struct_lt_args,)* #(#struct_rlt_args,)* #(#struct_ty_args),*>;
		}

		#[allow(dead_code, unused_mut, unused_unsafe)]
		impl #struct_impl_params #item_ident #struct_impl_args #struct_where_clause {
			/// Create a new instance of the rental struct.
			///
			/// The first argument provided is the head, followed by a series of closures, one for each tail field. Each of these closures will receive, as its arguments, a borrow of the previous field, followed by borrows of the remaining prefix fields if the struct is a shared rental. If the struct is a mutable rental, only the immediately preceding field is passed.
			pub fn new<#(#tail_closure_tys),*>(
				mut #head_ident: #head_ty,
				#(#tail_idents: #tail_closure_tys),*
			) -> Self where #(#tail_closure_tys: #tail_closure_bounds),*
			{
				#(__rental_prelude::static_assert_stable_deref::<#prefix_field_tys>();)*

				#(let mut #tail_idents = unsafe { __rental_prelude::transmute::<_, #tail_field_tys>(#tail_closure_exprs) };)*

				#item_ident {
					#(#field_idents: Some(#local_idents),)*
				}
			}

			/// Attempt to create a new instance of the rental struct.
			///
			/// As `new`, but each closure returns a `Result`. If the result is an error, execution is short-circuited and the error is returned to you, along with the original head value.
			pub fn try_new<#(#tail_closure_tys,)* __E>(
				mut #head_ident: #head_ty,
				#(#tail_idents: #tail_closure_tys),*
			) -> __rental_prelude::TryNewResult<Self, __E, #head_ty> where
				#(#tail_closure_tys: #tail_try_closure_bounds,)*
			{
				#(__rental_prelude::static_assert_stable_deref::<#prefix_field_tys>();)*

				#(let mut #tail_idents = {
					let temp = #tail_try_closure_exprs.map(|t| unsafe { __rental_prelude::transmute::<_, #tail_field_tys>(t) });
					match temp {
						Ok(t) => t,
						Err(e) => return Err(__rental_prelude::TryNewError(e.into(), #head_ident_rep)),
					}
				};)*

				Ok(#item_ident {
					#(#field_idents: Some(#local_idents),)*
				})
			}

			/// Return direct shared borrows of the fields of the struct.
			///
			/// This is unsafe because the erased lifetimes are exposed. Use this only if absolutely necessary and be very mindful of what the true lifetimes are.
			pub unsafe fn borrow<#struct_fake_rlt_arg>(&#struct_fake_rlt_arg self) -> <Self as __rental_prelude::#rental_trait_ident<#(#struct_fake_rlt_args),*>>::Borrow {
				#borrow_ident::unify_hack_tys(#(__rental_prelude::transmute(#borrow_exprs),)*)
			}

			/// Return a direct mutable borrow of the suffix of the struct.
			///
			/// This is unsafe because the erased lifetimes are exposed. Use this only if absolutely necessary and be very mindful of what the true lifetimes are.
			pub unsafe fn borrow_mut<#struct_fake_rlt_arg>(&#struct_fake_rlt_arg mut self) -> <Self as __rental_prelude::#rental_trait_ident<#(#struct_fake_rlt_args),*>>::BorrowMut {
				#borrow_mut_ident::unify_hack_tys(#(__rental_prelude::transmute(#borrow_mut_exprs),)*)
			}

			/// Execute a closure on the shared suffix of the struct.
			///
			/// The closure may return any value not bounded by one of the special rentail lifetimes of the struct.
			pub fn rent<__F, __R>(&self, f: __F) -> __R where
				__F: for<#(#suffix_rlt_args,)*> FnOnce(#borrow_suffix_ty) -> __R,
				__R: #(#struct_lt_args +)*,
			{
				f(#borrow_suffix_expr)
			}

			/// Execute a closure on the mutable suffix of the struct.
			///
			/// The closure may return any value not bounded by one of the special rentail lifetimes of the struct.
			pub fn rent_mut<__F, __R>(&mut self, f: __F) -> __R where
				__F: for<#(#suffix_rlt_args,)*> FnOnce(#borrow_mut_suffix_ty) -> __R,
				__R: #(#struct_lt_args +)*,
			{
				f(#borrow_mut_suffix_expr)
			}

			/// Return a shared reference from the shared suffix of the struct.
			///
			/// This is a subtle variation of `rent` where it is legal to return a reference bounded by a rental lifetime, because that lifetime is reborrowed away before it is returned to you.
			pub fn ref_rent<__F, __R>(&self, f: __F) -> &__R where
				__F: for<#(#suffix_rlt_args,)*> FnOnce(#borrow_suffix_ty) -> &#last_rlt_arg __R,
				__R: 'static + ?Sized //#(#struct_lt_args +)*,
			{
				f(#borrow_suffix_expr)
			}

			/// Return a mutable reference from the mutable suffix of the struct.
			///
			/// This is a subtle variation of `rent_mut` where it is legal to return a reference bounded by a rental lifetime, because that lifetime is reborrowed away before it is returned to you.
			pub fn ref_rent_mut<__F, __R>(&mut self, f: __F) -> &mut __R where
				__F: for<#(#suffix_rlt_args,)*> FnOnce(#borrow_mut_suffix_ty) -> &#last_rlt_arg  mut __R,
				__R: 'static + ?Sized //#(#struct_lt_args +)*,
			{
				f(#borrow_mut_suffix_expr)
			}

			/// Drop the rental struct and return the original head value to you.
			pub fn into_head(mut self) -> #head_ty {
				#(drop(self.#tail_field_idents_reverse.take());)*
				self.#head_ident.take().unwrap()
			}
		}

		impl #struct_impl_params __rental_prelude::Drop for #item_ident #struct_impl_args #struct_where_clause {
			fn drop(&mut self) {
				#(drop(self.#field_idents_reverse.take());)*
			}
		}
	).to_tokens(tokens);

	if !is_rental_mut {
		quote!(
			#[allow(dead_code)]
			impl #struct_impl_params #item_ident #struct_impl_args #struct_where_clause {
				/// Return a shared reference to the head field of the struct.
				pub fn head(&self) -> &<#head_ty as __rental_prelude::Deref>::Target {
					&*self.#head_ident.as_ref().unwrap()
				}

				/// Execute a closure on shared borrows of the fields of the struct.
				///
				/// The closure may return any value not bounded by one of the special rentail lifetimes of the struct.
				pub fn rent_all<__F, __R>(&self, f: __F) -> __R where
					__F: for<#(#struct_rlt_args,)*> FnOnce(#borrow_ident<#(#struct_lt_args,)* #(#struct_rlt_args,)* #(#struct_ty_args),*>) -> __R,
					__R: #(#struct_lt_args +)*,
				{
					f(unsafe { self.borrow() })
				}

				/// Return a shared reference from shared borrows of the fields of the struct.
				///
				/// This is a subtle variation of `rent_all` where it is legal to return a reference bounded by a rental lifetime, because that lifetime is reborrowed away before it is returned to you.
				pub fn ref_rent_all<__F, __R>(&self, f: __F) -> &__R where
					__F: for<#(#struct_rlt_args,)*> FnOnce(#borrow_ident<#(#struct_lt_args,)* #(#struct_rlt_args,)* #(#struct_ty_args),*>) -> &#last_rlt_arg __R,
					__R: 'static + ?Sized //#(#struct_lt_args +)*,
				{
					f(unsafe { self.borrow() })
				}

				/// Execute a closure on shared borrows of the prefix fields and a mutable borrow of the suffix field of the struct.
				///
				/// The closure may return any value not bounded by one of the special rentail lifetimes of the struct.
				pub fn rent_all_mut<__F, __R>(&mut self, f: __F) -> __R where
					__F: for<#(#struct_rlt_args,)*> FnOnce(#borrow_mut_ident<#(#struct_lt_args,)* #(#struct_rlt_args,)* #(#struct_ty_args),*>) -> __R,
					__R: #(#struct_lt_args +)*,
				{
					f(unsafe { self.borrow_mut() })
				}

				/// Return a mutable reference from shared borrows of the prefix fields and a mutable borrow of the suffix field of the struct.
				///
				/// This is a subtle variation of `rent_all_mut` where it is legal to return a reference bounded by a rental lifetime, because that lifetime is reborrowed away before it is returned to you.
				pub fn ref_rent_all_mut<__F, __R>(&mut self, f: __F) -> &__R where
					__F: for<#(#struct_rlt_args,)*> FnOnce(#borrow_mut_ident<#(#struct_lt_args,)* #(#struct_rlt_args,)* #(#struct_ty_args),*>) -> &#last_rlt_arg mut __R,
					__R: 'static + ?Sized //#(#struct_lt_args +)*,
				{
					f(unsafe { self.borrow_mut() })
				}
			}
		).to_tokens(tokens);
	}

	if is_debug_borrow {
		quote!(
			impl #struct_impl_params __rental_prelude::fmt::Debug for #item_ident #struct_impl_args #struct_where_clause {
				fn fmt(&self, f: &mut __rental_prelude::fmt::Formatter) -> __rental_prelude::fmt::Result {
					unsafe { __rental_prelude::fmt::Debug::fmt(&self.borrow(), f) }
				}
			}
		).to_tokens(tokens);
	}

	if fields[fields.len() - 1].subrental.is_some() {
		quote!(
			impl<#(#borrow_lt_params,)* #(#struct_ty_params),*> __rental_prelude::IntoSuffix for #borrow_ident<#(#struct_lt_args,)* #(#struct_rlt_args,)* #(#struct_ty_args),*> #struct_where_clause {
				type Suffix = <#borrow_suffix_ty as IntoSuffix>::Suffix;

				#[allow(non_shorthand_field_patterns)]
				fn into_suffix(self) -> <Self as __rental_prelude::IntoSuffix>::Suffix {
					let #borrow_ident{#suffix_field_ident: suffix, ..};
					suffix.into_suffix()
				}
			}

			impl<#(#borrow_lt_params,)* #(#struct_ty_params),*> __rental_prelude::IntoSuffix for #borrow_mut_ident<#(#struct_lt_args,)* #(#struct_rlt_args,)* #(#struct_ty_args),*> #struct_where_clause {
				type Suffix = <#borrow_mut_suffix_ty as IntoSuffix>::Suffix;

				#[allow(non_shorthand_field_patterns)]
				fn into_suffix(self) -> <Self as __rental_prelude::IntoSuffix>::Suffix {
					let #borrow_mut_ident{#suffix_field_ident: suffix, ..};
					suffix.into_suffix()
				}
			}
		).to_tokens(tokens);

		if is_deref_suffix {
			quote!(
				impl #struct_impl_params __rental_prelude::Deref for #item_ident #struct_impl_args #struct_where_clause {
					type Target = <#suffix_field_ty as __rental_prelude::Deref>::Target;

					fn deref(&self) -> &<Self as __rental_prelude::Deref>::Target {
						self.ref_rent(|suffix| &**suffix.into_suffix())
					}
				}
			).to_tokens(tokens);
		}

		if is_deref_suffix {
			quote!(
				impl #struct_impl_params __rental_prelude::DerefMut for #item_ident #struct_impl_args #struct_where_clause {
					fn deref_mut(&mut self) -> &mut <Self as __rental_prelude::Deref>::Target {
						self.ref_rent_mut(|suffix| &mut **suffix.into_suffix())
					}
				}
			).to_tokens(tokens);
		}
	} else {
		quote!(
			impl<#(#borrow_lt_params,)* #(#struct_ty_params),*> __rental_prelude::IntoSuffix for #borrow_ident<#(#struct_lt_args,)* #(#struct_rlt_args,)* #(#struct_ty_args),*> #struct_where_clause {
				type Suffix = #borrow_suffix_ty;

				#[allow(non_shorthand_field_patterns)]
				fn into_suffix(self) -> <Self as __rental_prelude::IntoSuffix>::Suffix {
					let #borrow_ident{#suffix_field_ident: suffix, ..} = self;
					suffix
				}
			}

			impl<#(#borrow_lt_params,)* #(#struct_ty_params),*> __rental_prelude::IntoSuffix for #borrow_mut_ident<#(#struct_lt_args,)* #(#struct_rlt_args,)* #(#struct_ty_args),*> #struct_where_clause {
				type Suffix = #borrow_mut_suffix_ty;

				#[allow(non_shorthand_field_patterns)]
				fn into_suffix(self) -> <Self as __rental_prelude::IntoSuffix>::Suffix {
					let #borrow_mut_ident{#suffix_field_ident: suffix, ..} = self;
					suffix
				}
			}
		).to_tokens(tokens);

		if is_deref_suffix {
			quote!(
				impl #struct_impl_params __rental_prelude::Deref for #item_ident #struct_impl_args #struct_where_clause {
					type Target = <#suffix_field_ty as __rental_prelude::Deref>::Target;

					fn deref(&self) -> &<Self as __rental_prelude::Deref>::Target {
						self.ref_rent(|suffix| &**suffix)
					}
				}
			).to_tokens(tokens);
		}

		if is_deref_mut_suffix {
			quote!(
				impl #struct_impl_params __rental_prelude::DerefMut for #item_ident #struct_impl_args #struct_where_clause {
					fn deref_mut(&mut self) -> &mut <Self as __rental_prelude::Deref>::Target {
						self.ref_rent_mut(|suffix| &mut **suffix)
					}
				}
			).to_tokens(tokens);
		}
	}

	if is_deref_suffix {
		quote!(
			impl #struct_impl_params __rental_prelude::AsRef<<Self as __rental_prelude::Deref>::Target> for #item_ident #struct_impl_args #struct_where_clause {
				fn as_ref(&self) -> &<Self as __rental_prelude::Deref>::Target {
					&**self
				}
			}

//			impl #struct_impl_params __rental_prelude::Borrow<<Self as __rental_prelude::Deref>::Target> for #item_ident #struct_impl_args #struct_where_clause {
//				fn borrow(&self) -> &<Self as __rental_prelude::Deref>::Target {
//					&**self
//				}
//			}
		).to_tokens(tokens);
	}

	if is_deref_mut_suffix {
		quote!(
			impl #struct_impl_params __rental_prelude::AsMut<<Self as __rental_prelude::Deref>::Target> for #item_ident #struct_impl_args #struct_where_clause {
				fn as_mut(&mut self) -> &mut <Self as __rental_prelude::Deref>::Target {
					&mut **self
				}
			}

//			impl #struct_impl_params __rental_prelude::BorrowMut<<Self as __rental_prelude::Deref>::Target> for #item_ident #struct_impl_args #struct_where_clause {
//				fn borrow_mut(&mut self) -> &mut <Self as __rental_prelude::Deref>::Target {
//					&mut **self
//				}
//			}
		).to_tokens(tokens);
	}
}


fn make_borrow_quotes(fields: &[RentalField], is_rental_mut: bool) -> Vec<BorrowQuotes> {
	(0 .. fields.len()).map(|idx| {
		let (field_ty, deref) = if idx == fields.len() - 1 {
			let orig_ty = &fields[idx].orig_ty;
			(quote!(#orig_ty), quote!())
		} else {
			let orig_ty = &fields[idx].orig_ty;
			(quote!(<#orig_ty as __rental_prelude::Deref>::Target), quote!(*))
		};

		let field_ty_hack = fields[idx].target_ty_hack.as_ref().unwrap_or(&fields[idx].orig_ty);

		if let Some(ref subrental) = fields[idx].subrental {
			let field_ident = &fields[idx].name;
			let rental_trait_ident = &subrental.rental_trait_ident;
			let field_rlt_args = &fields[idx].self_rlt_args;

			let (borrow_ty_hack, borrow_mut_ty_hack, field_lt_args, field_ty_args) = if let syn::Ty::Path(ref qself, ref ty_path) = *field_ty_hack {
				let seg_idx = ty_path.segments.len() - 1;
				if let syn::PathParameters::AngleBracketed(ref params) = ty_path.segments[seg_idx].parameters {
					let ty_name = &ty_path.segments[seg_idx].ident.as_ref();

					let mut borrow_ty_path = ty_path.clone();
					borrow_ty_path.segments[seg_idx].ident = syn::Ident::new(format!("{}_Borrow", ty_name));
					borrow_ty_path.segments[seg_idx].parameters = syn::PathParameters::none();

					let mut borrow_mut_ty_path = ty_path.clone();
					borrow_mut_ty_path.segments[seg_idx].ident = syn::Ident::new(format!("{}_BorrowMut", ty_name));
					borrow_mut_ty_path.segments[seg_idx].parameters = syn::PathParameters::none();

					(
						syn::Ty::Path(qself.clone(), borrow_ty_path),
						syn::Ty::Path(qself.clone(), borrow_mut_ty_path),
						&params.lifetimes,
						&params.types,
					)
				} else {
					panic!("Field `{}` must have angle-bracketed params.", fields[idx].name)
				}
			} else {
				panic!("Field `{}` must be a type path.", fields[idx].name)
			};

			BorrowQuotes {
				ty: if idx == fields.len() - 1 || !is_rental_mut {
					quote!(<#field_ty as __rental_prelude::#rental_trait_ident<#(#field_rlt_args),*>>::Borrow)
				} else {
					quote!(__rental_prelude::PhantomData<<#field_ty as __rental_prelude::#rental_trait_ident<#(#field_rlt_args),*>>::Borrow>)
				},
				ty_hack: if idx == fields.len() - 1 || !is_rental_mut {
					quote!(#borrow_ty_hack<#(#field_lt_args,)* #(#field_rlt_args,)* #(#field_ty_args),*>)
				} else {
					quote!(__rental_prelude::PhantomData<#borrow_ty_hack<#(#field_lt_args,)* #(#field_rlt_args,)* #(#field_ty_args),*>>)
				},
				expr: if idx == fields.len() - 1 || !is_rental_mut {
					//quote!(unsafe { (#deref self.#field_ident).borrow() })
					quote!(unsafe { (#deref *self.#field_ident.as_ref().unwrap()).borrow() })
				} else {
					quote!(__rental_prelude::PhantomData::<()>)
				},

				mut_ty: if idx == fields.len() - 1 {
					quote!(<#field_ty as __rental_prelude::#rental_trait_ident<#(#field_rlt_args),*>>::BorrowMut)
				} else if !is_rental_mut {
					quote!(<#field_ty as __rental_prelude::#rental_trait_ident<#(#field_rlt_args),*>>::Borrow)
				} else {
					quote!(__rental_prelude::PhantomData<<#field_ty as __rental_prelude::#rental_trait_ident<#(#field_rlt_args),*>>::BorrowMut>)
				},
				mut_ty_hack: if idx == fields.len() - 1 {
					quote!(#borrow_mut_ty_hack<#(#field_lt_args,)* #(#field_rlt_args,)* #(#field_ty_args),*>)
				} else if !is_rental_mut {
					quote!(#borrow_ty_hack<#(#field_lt_args,)* #(#field_rlt_args,)* #(#field_ty_args),*>)
				} else {
					quote!(__rental_prelude::PhantomData<#borrow_mut_ty_hack<#(#field_lt_args,)* #(#field_rlt_args,)* #(#field_ty_args),*>>)
				},
				mut_expr: if idx == fields.len() - 1 {
					//quote!(unsafe { (#deref self.#field_ident).borrow_mut() })
					quote!(unsafe { (#deref *self.#field_ident.as_mut().unwrap()).borrow_mut() })
				} else if !is_rental_mut {
					quote!(unsafe { (#deref *self.#field_ident.as_ref().unwrap()).borrow() })
				} else {
					quote!(__rental_prelude::PhantomData::<()>)
				},

				new_ty: if !is_rental_mut  {
					//quote!(<#field_ty as __rental_prelude::#rental_trait_ident<#(#field_rlt_args),*>>::Borrow)
					quote!(#borrow_ty_hack<#(#field_lt_args,)* #(#field_rlt_args,)* #(#field_ty_args),*>)
				} else {
					//quote!(<#field_ty as __rental_prelude::#rental_trait_ident<#(#field_rlt_args),*>>::BorrowMut)
					quote!(#borrow_mut_ty_hack<#(#field_lt_args,)* #(#field_rlt_args,)* #(#field_ty_args),*>)
				},
				new_expr: if !is_rental_mut {
					quote!(unsafe { (#deref #field_ident).borrow() })
				} else {
					quote!(unsafe { (#deref #field_ident).borrow_mut() })
				},
			}
		} else {
			let field_ident = &fields[idx].name;
			let field_rlt_arg = &fields[idx].self_rlt_args[0];

			BorrowQuotes {
				ty: if idx == fields.len() - 1 || !is_rental_mut {
					quote!(&#field_rlt_arg #field_ty)
				} else {
					quote!(__rental_prelude::PhantomData<&#field_rlt_arg #field_ty>)
				},
				ty_hack: if idx == fields.len() - 1 || !is_rental_mut {
					quote!(&#field_rlt_arg #field_ty_hack)
				} else {
					quote!(__rental_prelude::PhantomData<&#field_rlt_arg #field_ty_hack>)
				},
				expr: if idx == fields.len() - 1 || !is_rental_mut {
					//quote!(& #deref self.#field_ident)
					quote!(&#deref *self.#field_ident.as_ref().unwrap())
				} else {
					quote!(__rental_prelude::PhantomData::<()>)
				},

				mut_ty: if idx == fields.len() - 1 {
					quote!(&#field_rlt_arg mut #field_ty)
				} else if !is_rental_mut {
					quote!(&#field_rlt_arg #field_ty)
				} else {
					quote!(__rental_prelude::PhantomData<&#field_rlt_arg mut #field_ty>)
				},
				mut_ty_hack: if idx == fields.len() - 1 {
					quote!(&#field_rlt_arg mut #field_ty_hack)
				} else if !is_rental_mut {
					quote!(&#field_rlt_arg #field_ty_hack)
				} else {
					quote!(__rental_prelude::PhantomData<&#field_rlt_arg mut #field_ty_hack>)
				},
				mut_expr: if idx == fields.len() - 1 {
					//quote!(&mut #deref self.#field_ident)
					quote!(&mut #deref *self.#field_ident.as_mut().unwrap())
				} else if !is_rental_mut {
					quote!(&#deref *self.#field_ident.as_ref().unwrap())
				} else {
					quote!(__rental_prelude::PhantomData::<()>)
				},

				new_ty: if !is_rental_mut {
					//quote!(&#field_rlt_arg #field_ty)
					quote!(&#field_rlt_arg #field_ty_hack)
				} else {
					//quote!(&#field_rlt_arg mut #field_ty)
					quote!(&#field_rlt_arg mut #field_ty_hack)
				},
				new_expr: if !is_rental_mut {
					quote!(& #deref #field_ident)
				} else {
					quote!(&mut #deref #field_ident)
				},
			}
		}
	}).collect()
}


fn make_tail_closure_quotes(fields: &[RentalField], borrows: &[BorrowQuotes], is_rental_mut: bool) -> Vec<ClosureQuotes> {
	(1 .. fields.len()).map(|idx| {
		let local_name = &fields[idx].name;
		let field_ty = &fields[idx].orig_ty;

		if !is_rental_mut {
			let prev_new_tys_reverse = &borrows[0 .. idx].iter().map(|b| &b.new_ty).rev().collect::<Vec<_>>();
			let prev_new_exprs_reverse = &borrows[0 .. idx].iter().map(|b| &b.new_expr).rev().collect::<Vec<_>>();;
			let mut prev_rlt_args = Vec::new();
			for prev_field in &fields[0 .. idx] {
				prev_rlt_args.extend(&prev_field.self_rlt_args);
			}
			let prev_rlt_args = &prev_rlt_args;

			ClosureQuotes {
				bound: quote!(for<#(#prev_rlt_args),*> FnOnce(#(#prev_new_tys_reverse),*) -> #field_ty),
				expr: quote!(#local_name(#(#prev_new_exprs_reverse),*)),
				try_bound: quote!(for<#(#prev_rlt_args),*> FnOnce(#(#prev_new_tys_reverse),*) -> __rental_prelude::Result<#field_ty, __E>),
				try_expr: quote!(#local_name(#(#prev_new_exprs_reverse),*)),
			}
		} else {
			let prev_new_ty = &borrows[idx - 1].new_ty;
			let prev_new_expr = &borrows[idx - 1].new_expr;
			let prev_rlt_args = &fields[idx - 1].self_rlt_args.iter().chain(&fields[idx - 1].used_rlt_args).collect::<Vec<_>>();

			ClosureQuotes {
				bound: quote!(for<#(#prev_rlt_args),*> FnOnce(#prev_new_ty) -> #field_ty),
				expr: quote!(#local_name(#prev_new_expr)),
				try_bound: quote!(for<#(#prev_rlt_args),*> FnOnce(#prev_new_ty) -> __rental_prelude::Result<#field_ty, __E>),
				try_expr: quote!(#local_name(#prev_new_expr)),
			}
		}
	}).collect()
}


struct RentalField {
	pub name: syn::Ident,
	pub orig_ty: syn::Ty,
	pub erased: syn::Field,
	pub subrental: Option<Subrental>,
	pub self_rlt_args: Vec<syn::Lifetime>,
	pub used_rlt_args: Vec<syn::Lifetime>,
	pub target_ty_hack: Option<syn::Ty>,
}


struct Subrental {
	arity: usize,
	rental_trait_ident: syn::Ident,
}


struct BorrowQuotes {
	pub ty: quote::Tokens,
	pub ty_hack: quote::Tokens,
	pub expr: quote::Tokens,
	pub mut_ty: quote::Tokens,
	pub mut_ty_hack: quote::Tokens,
	pub mut_expr: quote::Tokens,
	pub new_ty: quote::Tokens,
	pub new_expr: quote::Tokens,
}


struct ClosureQuotes {
	pub bound: quote::Tokens,
	pub expr: quote::Tokens,
	pub try_bound: quote::Tokens,
	pub try_expr: quote::Tokens,
}


struct RentalLifetimeEraser<'a> {
	pub fields: &'a [RentalField],
	pub used_rlt_args: &'a mut Vec<syn::Lifetime>,
}


impl<'a> syn::fold::Folder for RentalLifetimeEraser<'a> {
	fn fold_lifetime(&mut self, lifetime: syn::Lifetime) -> syn::Lifetime {
		if self.fields.iter().any(|field| field.self_rlt_args.contains(&lifetime)) {
			if !self.used_rlt_args.contains(&lifetime) {
				self.used_rlt_args.push(lifetime.clone());
			}

			syn::Lifetime::new("'static")
		} else {
			lifetime
		}
	}
}


#[cfg(test)]
mod tests {
	use super::*;


	fn test_write(item: quote::Tokens) {
		let mut tokens = quote::Tokens::new();

		if let Ok(item) = syn::parse_item(&item.to_string()) {
			write_rental_struct_and_impls(&mut tokens, &item);
		}
	}


	#[test]
	#[should_panic(expected = "must have a `rental` or `rental_mut` attribute")]
	fn no_rental_attrib() {
		test_write(quote! {
			pub struct Foo {
				a: Box<i32>,
				b: &'a i32,
			}
		});
	}


	#[test]
	#[should_panic(expected = "must be non-private")]
	fn private_struct() {
		test_write(quote! {
			struct Foo {
				a: Box<i32>,
				b: &'a i32,
			}
		});
	}


	#[test]
	#[should_panic(expected = "rental attribute takes optional arguments")]
	fn unknown_rental_attrib() {
		test_write(quote! {
			#[rental(foo)]
			pub struct Foo {
				a: Box<i32>,
				b: &'a i32,
			}
		});
	}


	#[test]
	#[should_panic(expected = "must not have attributes other than")]
	fn struct_other_attrib() {
		test_write(quote! {
			#[rental]
			#[other]
			pub struct Foo {
				a: Box<i32>,
				b: &'a i32,
			}
		});
	}


	#[test]
	#[should_panic(expected = "must have at least 2 fields")]
	fn only_one_field() {
		test_write(quote! {
			#[rental]
			pub struct Foo {
				a: Box<i32>,
			}
		});
	}


	#[test]
	#[should_panic(expected = "must be private")]
	fn pub_field() {
		test_write(quote! {
			#[rental]
			pub struct Foo {
				pub a: Box<i32>,
				pub b: &'a i32,
			}
		});
	}


	#[test]
	#[should_panic(expected = "expects `arity = ")]
	fn no_arity() {
		test_write(quote! {
			#[rental]
			pub struct Foo {
				#[subrental]
				a: Box<i32>,
				b: &'a i32,
			}
		});
	}


	#[test]
	#[should_panic(expected = "must not have attributes other than")]
	fn field_other_attrib() {
		test_write(quote! {
			#[rental]
			pub struct Foo {
				#[other]
				a: Box<i32>,
				b: &'a i32,
			}
		});
	}


	#[test]
	#[should_panic(expected = "collides with rental lifetime")]
	fn rental_lifetime_collide() {
		test_write(quote! {
			#[rental]
			pub struct Foo<'a> {
				a: Box<i32>,
				b: &'a i32,
			}
		});
	}
}
