#![recursion_limit = "512"]

extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate procedural_masquerade;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

use std::iter;
use syn::spanned::Spanned;
use quote::ToTokens;
use proc_macro2::{Span, Term};


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

		for item in syn::parse_str::<syn::File>(input).expect("Failed to parse items in module body.").items.iter() {
			match *item {
				syn::Item::Use(..) => {
					item.to_tokens(&mut tokens);
				},
				syn::Item::Type(..) => {
					item.to_tokens(&mut tokens);
				},
				syn::Item::Struct(ref struct_info) => {
					write_rental_struct_and_impls(&mut tokens, &struct_info);
				},
				_ => panic!("Item must be a `use` or `struct`."),
			}
		}

		tokens.to_string()
	}
}


fn write_rental_traits(tokens: &mut quote::Tokens, max_arity: usize) {
	let mut lt_params = vec![syn::LifetimeDef::new(syn::Lifetime::new(Term::intern("'a0"), Span::call_site()))];

	for arity in 2 .. max_arity + 1 {
		let trait_ident = &syn::Ident::new(&format!("Rental{}", arity), Span::call_site());
		let lt_param = syn::LifetimeDef::new(syn::Lifetime::new(Term::intern(&format!("'a{}", arity - 1)), Span::call_site()));
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


fn write_rental_struct_and_impls(tokens: &mut quote::Tokens, struct_info: &syn::ItemStruct) {
	if let syn::Visibility::Inherited = struct_info.vis {
		panic!("Struct `{}` must be non-private.", struct_info.ident);
	}

	let attribs = get_struct_attribs(struct_info);
	let (fields, fields_brace) = prepare_fields(struct_info);

	let struct_generics = &struct_info.generics;
	let struct_rlt_args = &fields.iter().fold(Vec::new(), |mut rlt_args, field| { rlt_args.extend(field.self_rlt_args.iter()); rlt_args });
	if let Some(collide) = struct_rlt_args.iter().find(|rlt_arg| struct_generics.lifetimes().any(|lt_def| lt_def.lifetime == **rlt_arg)) {
		panic!("Struct `{}` lifetime parameter `{}` collides with rental lifetime.", struct_info.ident, collide);
	}
	let last_rlt_arg = &struct_rlt_args[struct_rlt_args.len() - 1];

	let item_ident = &struct_info.ident;
	let item_vis = &struct_info.vis;

	let (struct_impl_params, struct_impl_args, struct_where_clause) = struct_generics.split_for_impl();
	let struct_lt_params = &struct_generics.lifetimes().collect::<Vec<_>>();
	let struct_nonlt_params = &struct_generics.params.iter().filter(|param| if let syn::GenericParam::Lifetime(..) = **param { false } else { true }).collect::<Vec<_>>();
	let struct_lt_args = &struct_lt_params.iter().map(|lt_def| &lt_def.lifetime).collect::<Vec<_>>();

	let struct_nonlt_args = &struct_nonlt_params.iter().map(|param| match **param {
		syn::GenericParam::Type(ref ty) => &ty.ident,
		syn::GenericParam::Const(ref co) => &co.ident,
		syn::GenericParam::Lifetime(..) => unreachable!(),
	}).collect::<Vec<_>>();

	let rental_trait_ident = syn::Ident::new(&format!("Rental{}", struct_rlt_args.len()), Span::def_site());
	let field_idents = &fields.iter().map(|field| &field.name).collect::<Vec<_>>();
	let local_idents = field_idents;

	let (ref self_ref_param, ref self_mut_param, ref self_move_param, ref self_arg) = if attribs.is_deref_suffix {
		(quote!(_self: &Self), quote!(_self: &mut Self), quote!(_self: Self), quote!(_self))
	} else {
		(quote!(&self), quote!(&mut self), quote!(self), quote!(self))
	};

	let borrow_ident = syn::Ident::new(&(struct_info.ident.to_string() + "_Borrow"), Span::call_site());
	let borrow_mut_ident = syn::Ident::new(&(struct_info.ident.to_string() + "_BorrowMut"), Span::call_site());
	let borrow_quotes = &make_borrow_quotes(self_arg, &fields, attribs.is_rental_mut);
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
			lifetime: (*rlt_arg).clone(),
			bounds: vec![(*next_rlt_arg).clone()].into_iter().collect(),
			colon_token: Default::default(),
		}
	}).chain(Some(syn::LifetimeDef {
			attrs: Vec::with_capacity(0),
			lifetime: struct_rlt_args[struct_rlt_args.len() - 1].clone(),
			bounds: syn::punctuated::Punctuated::new(),
			colon_token: Default::default(),
	})).collect::<Vec<_>>();

	let borrow_lt_params = &struct_rlt_params.iter().cloned()
		.chain( struct_lt_params.iter().map(|lt_def| {
			let mut lt_def = (*lt_def).clone();
			lt_def.bounds.push(struct_rlt_args[0].clone());
			lt_def
		})).collect::<Vec<_>>();

	let head_ident = &local_idents[0];
	let head_ident_rep = &iter::repeat(&head_ident).take(fields.len() - 1).collect::<Vec<_>>();
	let head_ty = &fields[0].orig_ty;
	let tail_field_tys = &fields.iter().map(|field| &field.erased.ty).skip(1).collect::<Vec<_>>();
	let tail_idents = &local_idents.iter().skip(1).collect::<Vec<_>>();
	let tail_closure_tys = &fields.iter().skip(1).map(|field| syn::Ident::new(&format!("__F{}", field.name), Span::call_site())).collect::<Vec<_>>();
	let tail_closure_quotes = make_tail_closure_quotes(&fields, borrow_quotes, attribs.is_rental_mut);
	let tail_closure_bounds = &tail_closure_quotes.iter().map(|&ClosureQuotes{ref bound, ..}| bound).collect::<Vec<_>>();
	let tail_closure_exprs = &tail_closure_quotes.iter().map(|&ClosureQuotes{ref expr, ..}| expr).collect::<Vec<_>>();
	let tail_try_closure_bounds = &tail_closure_quotes.iter().map(|&ClosureQuotes{ref try_bound, ..}| try_bound).collect::<Vec<_>>();
	let tail_try_closure_exprs = &tail_closure_quotes.iter().map(|&ClosureQuotes{ref try_expr, ..}| try_expr).collect::<Vec<_>>();
	let suffix_field_ident = &field_idents[fields.len() - 1];
	let suffix_field_ty = &fields[fields.len() - 1].erased.ty;
	let suffix_rlt_args = &fields[fields.len() - 1].self_rlt_args.iter().chain(fields[fields.len() - 1].used_rlt_args.iter()).collect::<Vec<_>>();

	let borrow_derives = if attribs.is_debug_borrow {
		quote!(#[derive(Debug)])
	} else {
		quote!()
	};

	let rstruct = syn::ItemStruct{
		ident: struct_info.ident.clone(),
		vis: struct_info.vis.clone(),
		attrs: attribs.doc.clone(),
		fields: syn::Fields::Named(syn::FieldsNamed{
			brace_token: fields_brace,
			named: fields.iter().enumerate().map(|(i, field)| {
				let mut field_erased = field.erased.clone();
				if i < fields.len() - 1 {
					field_erased.attrs.push(parse_quote!(#[allow(dead_code)]));
				}
				field_erased
			}).rev().collect(),
		}),
		generics: struct_info.generics.clone(),
		struct_token: struct_info.struct_token,
		semi_token: None,
	};

	let borrow_struct = syn::ItemStruct{
		ident: borrow_ident.clone(),
		vis: item_vis.clone(),
		attrs: Vec::with_capacity(0),
		fields: syn::Fields::Named(syn::FieldsNamed{
			brace_token: Default::default(),
			named: fields.iter().zip(borrow_tys).enumerate().map(|(idx, (field, borrow_ty))| {
				let mut field = field.erased.clone();
				field.vis = if !attribs.is_rental_mut || idx == fields.len() - 1 { item_vis.clone() } else { syn::Visibility::Inherited };
				field.ty = syn::parse::<syn::Type>((**borrow_ty).clone().into()).unwrap();
				field
			}).collect(),
		}),
		generics: {
			let mut gen = struct_generics.clone();
			gen.params = borrow_lt_params.iter().map(|lt| syn::GenericParam::Lifetime(lt.clone()))
				.chain(gen.type_params().map(|p| syn::GenericParam::Type(p.clone())))
				.chain(gen.const_params().map(|p| syn::GenericParam::Const(p.clone())))
				.collect();
			gen
		},
		struct_token: Default::default(),
		semi_token: None,
	};

	let borrow_mut_struct = syn::ItemStruct{
		ident: borrow_mut_ident.clone(),
		vis: struct_info.vis.clone(),
		attrs: Vec::with_capacity(0),
		fields: syn::Fields::Named(syn::FieldsNamed{
			brace_token: Default::default(),
			named: fields.iter().zip(borrow_mut_tys).enumerate().map(|(idx, (field, borrow_mut_ty))| {
				let mut field = field.erased.clone();
				field.vis = if idx == fields.len() - 1 || !attribs.is_rental_mut { (*item_vis).clone() } else { syn::Visibility::Inherited };
				field.ty = syn::parse::<syn::Type>((**borrow_mut_ty).clone().into()).unwrap();
				field
			}).collect(),
		}),
		generics: {
			let mut gen = struct_generics.clone();
			gen.params = borrow_lt_params.iter().map(|lt| syn::GenericParam::Lifetime(lt.clone()))
				.chain(gen.type_params().map(|p| syn::GenericParam::Type(p.clone())))
				.chain(gen.const_params().map(|p| syn::GenericParam::Const(p.clone())))
				.collect();
			gen
		},
		struct_token: Default::default(),
		semi_token: None,
	};

	let static_assert_prefix_stable_derefs = &fields.iter().map(|field| {
		let prefix_field_ty = &field.erased.ty;
		if attribs.is_rental_mut {
			quote_spanned!(field.erased.ty.span().resolved_at(Span::def_site()) => __rental_prelude::static_assert_stable_deref_mut::<#prefix_field_ty>();)
		} else {
			quote_spanned!(field.erased.ty.span().resolved_at(Span::def_site()) => __rental_prelude::static_assert_stable_deref::<#prefix_field_ty>();)
		}
	}).take(fields.len() - 1).collect::<Vec<_>>();

	let struct_span = struct_info.span().resolved_at(Span::def_site());
	let suffix_ty_span = suffix_field_ty.span().resolved_at(Span::def_site());

	quote_spanned!(struct_span =>
		#rstruct

		/// Shared borrow of a rental struct.
		#[allow(non_camel_case_types, non_snake_case, dead_code)]
		#borrow_derives
		#borrow_struct

		/// Mutable borrow of a rental struct.
		#[allow(non_camel_case_types, non_snake_case, dead_code)]
		#borrow_derives
		#borrow_mut_struct
	).to_tokens(tokens);

	quote_spanned!(struct_span =>
		#[allow(dead_code)]
		impl<#(#borrow_lt_params,)* #(#struct_nonlt_params),*> #borrow_ident<#(#struct_rlt_args,)* #(#struct_lt_args,)* #(#struct_nonlt_args),*> #struct_where_clause {
			fn unify_hack_tys(#(#local_idents: #borrow_ty_hacks),*) -> #borrow_ident<#(#struct_rlt_args,)* #(#struct_lt_args,)* #(#struct_nonlt_args),*> {
				#borrow_ident {
					#(#field_idents: #local_idents,)*
				}
			}
		}
	).to_tokens(tokens);

	quote_spanned!(struct_span =>
		#[allow(dead_code)]
		impl<#(#borrow_lt_params,)* #(#struct_nonlt_params),*> #borrow_mut_ident<#(#struct_rlt_args,)* #(#struct_lt_args,)* #(#struct_nonlt_args),*> #struct_where_clause {
			fn unify_hack_tys(#(#local_idents: #borrow_mut_ty_hacks),*) -> #borrow_mut_ident<#(#struct_rlt_args,)* #(#struct_lt_args,)* #(#struct_nonlt_args),*> {
				#borrow_mut_ident {
					#(#field_idents: #local_idents,)*
				}
			}
		}
	).to_tokens(tokens);

	quote_spanned!(struct_span =>
		unsafe impl<#(#borrow_lt_params,)* #(#struct_nonlt_params),*> __rental_prelude::#rental_trait_ident<#(#struct_rlt_args),*> for #item_ident #struct_impl_args #struct_where_clause {
			type Borrow = #borrow_ident<#(#struct_rlt_args,)* #(#struct_lt_args,)* #(#struct_nonlt_args),*>;
			type BorrowMut = #borrow_mut_ident<#(#struct_rlt_args,)* #(#struct_lt_args,)* #(#struct_nonlt_args),*>;
		}
	).to_tokens(tokens);

	quote_spanned!(struct_span =>
		#[allow(dead_code, unused_mut, unused_unsafe, non_camel_case_types)]
		impl #struct_impl_params #item_ident #struct_impl_args #struct_where_clause {
			/// Create a new instance of the rental struct.
			///
			/// The first argument provided is the head, followed by a series of closures, one for each tail field. Each of these closures will receive, as its arguments, a borrow of the previous field, followed by borrows of the remaining prefix fields if the struct is a shared rental. If the struct is a mutable rental, only the immediately preceding field is passed.
			pub fn new<#(#tail_closure_tys),*>(
				mut #head_ident: #head_ty,
				#(#tail_idents: #tail_closure_tys),*
			) -> Self where #(#tail_closure_tys: #tail_closure_bounds),*
			{
				#(#static_assert_prefix_stable_derefs)*

				#(let mut #tail_idents = unsafe { __rental_prelude::transmute::<_, #tail_field_tys>(#tail_closure_exprs) };)*

				#item_ident {
					#(#field_idents: #local_idents,)*
				}
			}

			/// Attempt to create a new instance of the rental struct.
			///
			/// As `new`, but each closure returns a `Result`. If one of them fails, execution is short-circuited and a tuple of the error and the original head value is returned to you.
			pub fn try_new<#(#tail_closure_tys,)* __E>(
				mut #head_ident: #head_ty,
				#(#tail_idents: #tail_closure_tys),*
			) -> __rental_prelude::TryNewResult<Self, __E, #head_ty> where
				#(#tail_closure_tys: #tail_try_closure_bounds,)*
			{
				#(#static_assert_prefix_stable_derefs)*

				#(let mut #tail_idents = {
					let temp = #tail_try_closure_exprs.map(|t| unsafe { __rental_prelude::transmute::<_, #tail_field_tys>(t) });
					match temp {
						Ok(t) => t,
						Err(e) => return Err(__rental_prelude::TryNewError(e.into(), #head_ident_rep)),
					}
				};)*

				Ok(#item_ident {
					#(#field_idents: #local_idents,)*
				})
			}

			/// Attempt to create a new instance of the rental struct.
			///
			/// As `try_new`, but only the error value is returned upon failure; the head value is dropped. This method interacts more smoothly with existing error conversions.
			pub fn try_new_or_drop<#(#tail_closure_tys,)* __E>(
				mut #head_ident: #head_ty,
				#(#tail_idents: #tail_closure_tys),*
			) -> __rental_prelude::Result<Self, __E> where
				#(#tail_closure_tys: #tail_try_closure_bounds,)*
			{
				#(#static_assert_prefix_stable_derefs)*

				#(let mut #tail_idents = {
					let temp = #tail_try_closure_exprs.map(|t| unsafe { __rental_prelude::transmute::<_, #tail_field_tys>(t) });
					match temp {
						Ok(t) => t,
						Err(e) => return Err(e.into()),
					}
				};)*

				Ok(#item_ident {
					#(#field_idents: #local_idents,)*
				})
			}

			/// Return lifetime-erased shared borrows of the fields of the struct.
			///
			/// This is unsafe because the erased lifetimes are fake. Use this only if absolutely necessary and be very mindful of what the true lifetimes are.
			pub unsafe fn borrow_erased(#self_ref_param) -> <Self as __rental_prelude::#rental_trait_ident>::Borrow {
				#borrow_ident::unify_hack_tys(#(__rental_prelude::transmute(#borrow_exprs),)*)
			}

			/// Return a lifetime-erased mutable borrow of the suffix of the struct.
			///
			/// This is unsafe because the erased lifetimes are fake. Use this only if absolutely necessary and be very mindful of what the true lifetimes are.
			pub unsafe fn borrow_mut_erased(#self_mut_param) -> <Self as __rental_prelude::#rental_trait_ident>::BorrowMut {
				#borrow_mut_ident::unify_hack_tys(#(__rental_prelude::transmute(#borrow_mut_exprs),)*)
			}

			/// Execute a closure on the shared suffix of the struct.
			///
			/// The closure may return any value not bounded by one of the special rentail lifetimes of the struct.
			pub fn rent<__F, __R>(#self_ref_param, f: __F) -> __R where
				__F: for<#(#suffix_rlt_args,)*> FnOnce(#borrow_suffix_ty) -> __R,
				__R: #(#struct_lt_args +)*,
			{
				f(#borrow_suffix_expr)
			}

			/// Execute a closure on the mutable suffix of the struct.
			///
			/// The closure may return any value not bounded by one of the special rentail lifetimes of the struct.
			pub fn rent_mut<__F, __R>(#self_mut_param, f: __F) -> __R where
				__F: for<#(#suffix_rlt_args,)*> FnOnce(#borrow_mut_suffix_ty) -> __R,
				__R: #(#struct_lt_args +)*,
			{
				f(#borrow_mut_suffix_expr)
			}

			/// Return a shared reference from the shared suffix of the struct.
			///
			/// This is a subtle variation of `rent` where it is legal to return a reference bounded by a rental lifetime, because that lifetime is reborrowed away before it is returned to you.
			pub fn ref_rent<__F, __R>(#self_ref_param, f: __F) -> &__R where
				__F: for<#(#suffix_rlt_args,)*> FnOnce(#borrow_suffix_ty) -> &#last_rlt_arg __R,
				__R: ?Sized //#(#struct_lt_args +)*,
			{
				f(#borrow_suffix_expr)
			}

			/// Optionally return a shared reference from the shared suffix of the struct.
			///
			/// This is a subtle variation of `rent` where it is legal to return a reference bounded by a rental lifetime, because that lifetime is reborrowed away before it is returned to you.
			pub fn maybe_ref_rent<__F, __R>(#self_ref_param, f: __F) -> __rental_prelude::Option<&__R> where
				__F: for<#(#suffix_rlt_args,)*> FnOnce(#borrow_suffix_ty) -> __rental_prelude::Option<&#last_rlt_arg __R>,
				__R: ?Sized //#(#struct_lt_args +)*,
			{
				f(#borrow_suffix_expr)
			}

			/// Try to return a shared reference from the shared suffix of the struct, or an error on failure.
			///
			/// This is a subtle variation of `rent` where it is legal to return a reference bounded by a rental lifetime, because that lifetime is reborrowed away before it is returned to you.
			pub fn try_ref_rent<__F, __R, __E>(#self_ref_param, f: __F) -> __rental_prelude::Result<&__R, __E> where
				__F: for<#(#suffix_rlt_args,)*> FnOnce(#borrow_suffix_ty) -> __rental_prelude::Result<&#last_rlt_arg __R, __E>,
				__R: ?Sized //#(#struct_lt_args +)*,
			{
				f(#borrow_suffix_expr)
			}

			/// Return a mutable reference from the mutable suffix of the struct.
			///
			/// This is a subtle variation of `rent_mut` where it is legal to return a reference bounded by a rental lifetime, because that lifetime is reborrowed away before it is returned to you.
			pub fn ref_rent_mut<__F, __R>(#self_mut_param, f: __F) -> &mut __R where
				__F: for<#(#suffix_rlt_args,)*> FnOnce(#borrow_mut_suffix_ty) -> &#last_rlt_arg  mut __R,
				__R: ?Sized //#(#struct_lt_args +)*,
			{
				f(#borrow_mut_suffix_expr)
			}

			/// Optionally return a mutable reference from the mutable suffix of the struct.
			///
			/// This is a subtle variation of `rent_mut` where it is legal to return a reference bounded by a rental lifetime, because that lifetime is reborrowed away before it is returned to you.
			pub fn maybe_ref_rent_mut<__F, __R>(#self_mut_param, f: __F) -> __rental_prelude::Option<&mut __R> where
				__F: for<#(#suffix_rlt_args,)*> FnOnce(#borrow_mut_suffix_ty) -> __rental_prelude::Option<&#last_rlt_arg  mut __R>,
				__R: ?Sized //#(#struct_lt_args +)*,
			{
				f(#borrow_mut_suffix_expr)
			}

			/// Try to return a mutable reference from the mutable suffix of the struct, or an error on failure.
			///
			/// This is a subtle variation of `rent_mut` where it is legal to return a reference bounded by a rental lifetime, because that lifetime is reborrowed away before it is returned to you.
			pub fn try_ref_rent_mut<__F, __R, __E>(#self_mut_param, f: __F) -> __rental_prelude::Result<&mut __R, __E> where
				__F: for<#(#suffix_rlt_args,)*> FnOnce(#borrow_mut_suffix_ty) -> __rental_prelude::Result<&#last_rlt_arg  mut __R, __E>,
				__R: ?Sized //#(#struct_lt_args +)*,
			{
				f(#borrow_mut_suffix_expr)
			}

			/// Drop the rental struct and return the original head value to you.
			pub fn into_head(#self_move_param) -> #head_ty {
				let Self{#head_ident, ..} = #self_arg;
				#head_ident
			}
		}
	).to_tokens(tokens);

	if !attribs.is_rental_mut {
		quote_spanned!(struct_span =>
			#[allow(dead_code)]
			impl #struct_impl_params #item_ident #struct_impl_args #struct_where_clause {
				/// Return a shared reference to the head field of the struct.
				pub fn head(#self_ref_param) -> &<#head_ty as __rental_prelude::Deref>::Target {
					&*#self_arg.#head_ident
				}

				/// Execute a closure on shared borrows of the fields of the struct.
				///
				/// The closure may return any value not bounded by one of the special rentail lifetimes of the struct.
				pub fn rent_all<__F, __R>(#self_ref_param, f: __F) -> __R where
					__F: for<#(#struct_rlt_args,)*> FnOnce(#borrow_ident<#(#struct_rlt_args,)* #(#struct_lt_args,)* #(#struct_nonlt_args),*>) -> __R,
					__R: #(#struct_lt_args +)*,
				{
					f(unsafe { #item_ident::borrow_erased(#self_arg) })
				}

				/// Return a shared reference from shared borrows of the fields of the struct.
				///
				/// This is a subtle variation of `rent_all` where it is legal to return a reference bounded by a rental lifetime, because that lifetime is reborrowed away before it is returned to you.
				pub fn ref_rent_all<__F, __R>(#self_ref_param, f: __F) -> &__R where
					__F: for<#(#struct_rlt_args,)*> FnOnce(#borrow_ident<#(#struct_rlt_args,)* #(#struct_lt_args,)* #(#struct_nonlt_args),*>) -> &#last_rlt_arg __R,
					__R: ?Sized //#(#struct_lt_args +)*,
				{
					f(unsafe { #item_ident::borrow_erased(#self_arg) })
				}

				/// Optionally return a shared reference from shared borrows of the fields of the struct.
				///
				/// This is a subtle variation of `rent_all` where it is legal to return a reference bounded by a rental lifetime, because that lifetime is reborrowed away before it is returned to you.
				pub fn maybe_ref_rent_all<__F, __R>(#self_ref_param, f: __F) -> __rental_prelude::Option<&__R> where
					__F: for<#(#struct_rlt_args,)*> FnOnce(#borrow_ident<#(#struct_rlt_args,)* #(#struct_lt_args,)* #(#struct_nonlt_args),*>) -> __rental_prelude::Option<&#last_rlt_arg __R>,
					__R: ?Sized //#(#struct_lt_args +)*,
				{
					f(unsafe { #item_ident::borrow_erased(#self_arg) })
				}

				/// Try to return a shared reference from shared borrows of the fields of the struct, or an error on failure.
				///
				/// This is a subtle variation of `rent_all` where it is legal to return a reference bounded by a rental lifetime, because that lifetime is reborrowed away before it is returned to you.
				pub fn try_ref_rent_all<__F, __R, __E>(#self_ref_param, f: __F) -> __rental_prelude::Result<&__R, __E> where
					__F: for<#(#struct_rlt_args,)*> FnOnce(#borrow_ident<#(#struct_rlt_args,)* #(#struct_lt_args,)* #(#struct_nonlt_args),*>) -> __rental_prelude::Result<&#last_rlt_arg __R, __E>,
					__R: ?Sized //#(#struct_lt_args +)*,
				{
					f(unsafe { #item_ident::borrow_erased(#self_arg) })
				}

				/// Execute a closure on shared borrows of the prefix fields and a mutable borrow of the suffix field of the struct.
				///
				/// The closure may return any value not bounded by one of the special rentail lifetimes of the struct.
				pub fn rent_all_mut<__F, __R>(#self_mut_param, f: __F) -> __R where
					__F: for<#(#struct_rlt_args,)*> FnOnce(#borrow_mut_ident<#(#struct_rlt_args,)* #(#struct_lt_args,)* #(#struct_nonlt_args),*>) -> __R,
					__R: #(#struct_lt_args +)*,
				{
					f(unsafe { #item_ident::borrow_mut_erased(#self_arg) })
				}

				/// Return a mutable reference from shared borrows of the prefix fields and a mutable borrow of the suffix field of the struct.
				///
				/// This is a subtle variation of `rent_all_mut` where it is legal to return a reference bounded by a rental lifetime, because that lifetime is reborrowed away before it is returned to you.
				pub fn ref_rent_all_mut<__F, __R>(#self_mut_param, f: __F) -> &mut __R where
					__F: for<#(#struct_rlt_args,)*> FnOnce(#borrow_mut_ident<#(#struct_rlt_args,)* #(#struct_lt_args,)* #(#struct_nonlt_args),*>) -> &#last_rlt_arg mut __R,
					__R: ?Sized //#(#struct_lt_args +)*,
				{
					f(unsafe { #item_ident::borrow_mut_erased(#self_arg) })
				}

				/// Optionally return a mutable reference from shared borrows of the prefix fields and a mutable borrow of the suffix field of the struct.
				///
				/// This is a subtle variation of `rent_all_mut` where it is legal to return a reference bounded by a rental lifetime, because that lifetime is reborrowed away before it is returned to you.
				pub fn maybe_ref_rent_all_mut<__F, __R>(#self_mut_param, f: __F) -> __rental_prelude::Option<&mut __R> where
					__F: for<#(#struct_rlt_args,)*> FnOnce(#borrow_mut_ident<#(#struct_rlt_args,)* #(#struct_lt_args,)* #(#struct_nonlt_args),*>) -> __rental_prelude::Option<&#last_rlt_arg mut __R>,
					__R: ?Sized //#(#struct_lt_args +)*,
				{
					f(unsafe { #item_ident::borrow_mut_erased(#self_arg) })
				}

				/// Try to return a mutable reference from shared borrows of the prefix fields and a mutable borrow of the suffix field of the struct, or an error on failure.
				///
				/// This is a subtle variation of `rent_all_mut` where it is legal to return a reference bounded by a rental lifetime, because that lifetime is reborrowed away before it is returned to you.
				pub fn try_ref_rent_all_mut<__F, __R, __E>(#self_mut_param, f: __F) -> __rental_prelude::Result<&mut __R, __E> where
					__F: for<#(#struct_rlt_args,)*> FnOnce(#borrow_mut_ident<#(#struct_rlt_args,)* #(#struct_lt_args,)* #(#struct_nonlt_args),*>) -> __rental_prelude::Result<&#last_rlt_arg mut __R, __E>,
					__R: ?Sized //#(#struct_lt_args +)*,
				{
					f(unsafe { #item_ident::borrow_mut_erased(#self_arg) })
				}
			}
		).to_tokens(tokens);
	}

	if attribs.is_debug_borrow {
		quote_spanned!(struct_info.ident.span.resolved_at(Span::def_site()) =>
			impl #struct_impl_params __rental_prelude::fmt::Debug for #item_ident #struct_impl_args #struct_where_clause {
				fn fmt(&self, f: &mut __rental_prelude::fmt::Formatter) -> __rental_prelude::fmt::Result {
					unsafe { __rental_prelude::fmt::Debug::fmt(&#item_ident::borrow_erased(self), f) }
				}
			}
		).to_tokens(tokens);
	}

	if fields[fields.len() - 1].subrental.is_some() {
		quote_spanned!(struct_span =>
			impl<#(#borrow_lt_params,)* #(#struct_nonlt_params),*> __rental_prelude::IntoSuffix for #borrow_ident<#(#struct_rlt_args,)* #(#struct_lt_args,)* #(#struct_nonlt_args),*> #struct_where_clause {
				type Suffix = <#borrow_suffix_ty as IntoSuffix>::Suffix;

				#[allow(non_shorthand_field_patterns)]
				fn into_suffix(self) -> <Self as __rental_prelude::IntoSuffix>::Suffix {
					let #borrow_ident{#suffix_field_ident: suffix, ..};
					suffix.into_suffix()
				}
			}
		).to_tokens(tokens);

		quote_spanned!(struct_span =>
			impl<#(#borrow_lt_params,)* #(#struct_nonlt_params),*> __rental_prelude::IntoSuffix for #borrow_mut_ident<#(#struct_rlt_args,)* #(#struct_lt_args,)* #(#struct_nonlt_args),*> #struct_where_clause {
				type Suffix = <#borrow_mut_suffix_ty as IntoSuffix>::Suffix;

				#[allow(non_shorthand_field_patterns)]
				fn into_suffix(self) -> <Self as __rental_prelude::IntoSuffix>::Suffix {
					let #borrow_mut_ident{#suffix_field_ident: suffix, ..};
					suffix.into_suffix()
				}
			}
		).to_tokens(tokens);

		if attribs.is_deref_suffix {
			quote_spanned!(suffix_ty_span =>
				impl #struct_impl_params __rental_prelude::Deref for #item_ident #struct_impl_args #struct_where_clause {
					type Target = <#suffix_field_ty as __rental_prelude::Deref>::Target;

					fn deref(&self) -> &<Self as __rental_prelude::Deref>::Target {
						#item_ident::ref_rent(self, |suffix| &**suffix.into_suffix())
					}
				}
			).to_tokens(tokens);
		}

		if attribs.is_deref_mut_suffix {
			quote_spanned!(suffix_ty_span =>
				impl #struct_impl_params __rental_prelude::DerefMut for #item_ident #struct_impl_args #struct_where_clause {
					fn deref_mut(&mut self) -> &mut <Self as __rental_prelude::Deref>::Target {
						#item_ident.ref_rent_mut(self, |suffix| &mut **suffix.into_suffix())
					}
				}
			).to_tokens(tokens);
		}
	} else {
		quote_spanned!(struct_span =>
			impl<#(#borrow_lt_params,)* #(#struct_nonlt_params),*> __rental_prelude::IntoSuffix for #borrow_ident<#(#struct_rlt_args,)* #(#struct_lt_args,)* #(#struct_nonlt_args),*> #struct_where_clause {
				type Suffix = #borrow_suffix_ty;

				#[allow(non_shorthand_field_patterns)]
				fn into_suffix(self) -> <Self as __rental_prelude::IntoSuffix>::Suffix {
					let #borrow_ident{#suffix_field_ident: suffix, ..} = self;
					suffix
				}
			}
		).to_tokens(tokens);

		quote_spanned!(struct_span =>
			impl<#(#borrow_lt_params,)* #(#struct_nonlt_params),*> __rental_prelude::IntoSuffix for #borrow_mut_ident<#(#struct_rlt_args,)* #(#struct_lt_args,)* #(#struct_nonlt_args),*> #struct_where_clause {
				type Suffix = #borrow_mut_suffix_ty;

				#[allow(non_shorthand_field_patterns)]
				fn into_suffix(self) -> <Self as __rental_prelude::IntoSuffix>::Suffix {
					let #borrow_mut_ident{#suffix_field_ident: suffix, ..} = self;
					suffix
				}
			}
		).to_tokens(tokens);

		if attribs.is_deref_suffix {
			quote_spanned!(suffix_ty_span =>
				impl #struct_impl_params __rental_prelude::Deref for #item_ident #struct_impl_args #struct_where_clause {
					type Target = <#suffix_field_ty as __rental_prelude::Deref>::Target;

					fn deref(&self) -> &<Self as __rental_prelude::Deref>::Target {
						#item_ident::ref_rent(self, |suffix| &**suffix)
					}
				}
			).to_tokens(tokens);
		}

		if attribs.is_deref_mut_suffix {
			quote_spanned!(suffix_ty_span =>
				impl #struct_impl_params __rental_prelude::DerefMut for #item_ident #struct_impl_args #struct_where_clause {
					fn deref_mut(&mut self) -> &mut <Self as __rental_prelude::Deref>::Target {
						#item_ident::ref_rent_mut(self, |suffix| &mut **suffix)
					}
				}
			).to_tokens(tokens);
		}
	}

	if attribs.is_deref_suffix {
		quote_spanned!(suffix_ty_span =>
			impl #struct_impl_params __rental_prelude::AsRef<<Self as __rental_prelude::Deref>::Target> for #item_ident #struct_impl_args #struct_where_clause {
				fn as_ref(&self) -> &<Self as __rental_prelude::Deref>::Target {
					&**self
				}
			}
		).to_tokens(tokens);
	}

	if attribs.is_deref_mut_suffix {
		quote_spanned!(suffix_ty_span =>
			impl #struct_impl_params __rental_prelude::AsMut<<Self as __rental_prelude::Deref>::Target> for #item_ident #struct_impl_args #struct_where_clause {
				fn as_mut(&mut self) -> &mut <Self as __rental_prelude::Deref>::Target {
					&mut **self
				}
			}
		).to_tokens(tokens);
	}
}


fn get_struct_attribs(struct_info: &syn::ItemStruct) -> RentalStructAttribs
{
	let mut rattrs = struct_info.attrs.clone();

	let mut is_rental_mut = false;
	let mut is_deref_suffix = false;
	let mut is_deref_mut_suffix = false;
	let mut is_debug_borrow = false;

	if let Some(rental_pos) = rattrs.iter().filter(|attr| !attr.is_sugared_doc).position(|attr| match attr.interpret_meta().expect(&format!("Struct `{}` Attribute `{}` is not properly formatted.", struct_info.ident, attr.path.clone().into_tokens())) {
		syn::Meta::Word(ref attr_ident) => {
			is_rental_mut = match attr_ident.as_ref() {
				"rental" => false,
				"rental_mut" => true,
				_ => return false,
			};

			true
		},
		syn::Meta::List(ref list) => {
			is_rental_mut = match list.ident.as_ref() {
				"rental" => false,
				"rental_mut" => true,
				_ => return false,
			};

			let mut leftover = list.nested.iter().filter(|nested| {
				if let syn::NestedMeta::Meta(ref meta) = **nested {
					match *meta {
						syn::Meta::Word(ref ident) => {
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
			}).count();

			if leftover > 0 {
				panic!("Struct `{}` rental attribute takes optional arguments: `debug_borrow`, `deref_suffix`, and `deref_mut_suffix`.", struct_info.ident);
			}

			true
		},
		_ => false,
	}) {
		rattrs.remove(rental_pos);
	} else {
		panic!("Struct `{}` must have a `rental` or `rental_mut` attribute.", struct_info.ident);
	}

	if rattrs.iter().any(|attr| attr.path != syn::parse_str::<syn::Path>("doc").unwrap()) {
		panic!("Struct `{}` must not have attributes other than one `rental` or `rental_mut`.", struct_info.ident);
	}

	RentalStructAttribs{
		is_rental_mut: is_rental_mut,
		is_deref_suffix: is_deref_suffix,
		is_deref_mut_suffix: is_deref_mut_suffix,
		is_debug_borrow: is_debug_borrow,
		doc: rattrs,
	}
}


fn prepare_fields(struct_info: &syn::ItemStruct) -> (Vec<RentalField>, syn::token::Brace) {
	let (fields, fields_brace) = match struct_info.fields {
		syn::Fields::Named(ref fields) => (&fields.named, fields.brace_token),
		syn::Fields::Unnamed(..) => panic!("Struct `{}` must not be a tuple struct.", struct_info.ident),
		_ => panic!("Struct `{}` must have at least 2 fields.", struct_info.ident),
	};

	if fields.len() < 2 {
		panic!("Struct `{}` must have at least 2 fields.", struct_info.ident);
	}

	let mut rfields = Vec::with_capacity(fields.len());
	for (field_idx, field) in fields.iter().enumerate() {
		if field.vis != syn::Visibility::Inherited {
			panic!(
				"Struct `{}` field `{}` must be private.",
				struct_info.ident,
				field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
			);
		}

		let mut rfattrs = field.attrs.clone();
		let mut subrental = None;
		let mut target_ty_hack = None;

		if let Some(sr_pos) = rfattrs.iter().position(|attr| match attr.interpret_meta() {
			Some(syn::Meta::List(ref list)) if list.ident == "subrental" => {
				if list.nested.len() != 1 {
					panic!(
						"`subrental` attribute on struct `{}` field `{}` expects `arity = int`.",
						struct_info.ident,
						field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
					);
				}

				match list.nested[0] {
					syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue{ref ident, lit: syn::Lit::Int(ref arity), ..})) if ident == "arity" => {
						subrental = Some(Subrental{
							arity: arity.value() as usize, 
							rental_trait_ident: syn::Ident::new(&format!("Rental{}", arity.value()), Span::def_site()),
						})
					},
					_ => panic!(
						"`subrental` attribute on struct `{}` field `{}` expects `arity = int`.",
						struct_info.ident,
						field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
					),
				}

				true
			},
			Some(syn::Meta::Word(ref word)) if word == "subrental" => {
				panic!(
					"`subrental` attribute on struct `{}` field `{}` expects `arity = int`.",
					struct_info.ident,
					field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
				);
			},
			Some(syn::Meta::NameValue(ref name_value)) if name_value.ident == "subrental" => {
				panic!(
					"`subrental` attribute on struct `{}` field `{}` expects `arity = int`.",
					struct_info.ident,
					field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
				);
			},
			_ => false,
		}) {
			rfattrs.remove(sr_pos);
		}

		if let Some(tth_pos) = rfattrs.iter().position(|a|
			match a.interpret_meta() {
				Some(syn::Meta::NameValue(syn::MetaNameValue{ref ident, lit: syn::Lit::Str(ref ty_str), ..})) if ident == "target_ty" => {
					if let Ok(ty) = syn::parse_str::<syn::Type>(&ty_str.value()) {
						target_ty_hack = Some(ty);

						true
					} else {
						panic!(
							"`target_ty_hack` attribute on struct `{}` field `{}` has an invalid ty string.",
							struct_info.ident,
							field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
						);
					}
				},
				_ => false,
			}
		) {
			rfattrs.remove(tth_pos);
		}

		if rfattrs.iter().any(|attr| match attr.interpret_meta() { Some(syn::Meta::NameValue(syn::MetaNameValue{ref ident, ..})) if ident == "doc" => false, _ => true }) {
			panic!(
				"Struct `{}` field `{}` must not have attributes other than one `subrental` and `target_ty_hack`.",
				struct_info.ident,
				field.ident.as_ref().map(|ident| ident.to_string()).unwrap_or_else(|| field_idx.to_string())
			);
		}

		let mut self_rlt_args = Vec::new();
		if let Some(Subrental{arity: sr_arity, ..}) = subrental {
			let field_ident = field.ident.as_ref().unwrap();
			for sr_idx in 0 .. sr_arity {
				self_rlt_args.push(syn::Lifetime::new(Term::intern(&format!("'{}_{}", field_ident, sr_idx)), Span::call_site()));
			}
		} else {
			let field_ident = field.ident.as_ref().unwrap();
			self_rlt_args.push(syn::Lifetime::new(Term::intern(&format!("'{}", field_ident)), Span::call_site()));
		}

		let mut used_rlt_args = Vec::new();
		let rty = {
			let mut eraser = RentalLifetimeEraser{
				fields: &rfields,
				used_rlt_args: &mut used_rlt_args,
			};

			syn::fold::fold_type(&mut eraser, field.ty.clone())
		};

		let target_ty_hack = target_ty_hack.as_ref().map(|ty| (*ty).clone()).or_else(|| if field_idx < fields.len() - 1 {
			if let syn::Type::Path(ref ty_path) = field.ty {
				match ty_path.path.segments[ty_path.path.segments.len() - 1] {
					syn::PathSegment{ref ident, arguments: syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments{ref args, ..})} => {
						if let Some(&syn::GenericArgument::Type(ref ty)) = args.first().map(|p| *p.value()) {
							if ident == "Vec" {
								Some(syn::Type::Slice(syn::TypeSlice{bracket_token: Default::default(), elem: Box::new(ty.clone())}))
							} else {
								Some(ty.clone())
							}
						} else {
							panic!("Field `{}` must be a type path with 1 type param, `String`, or a reference.", field.ident.as_ref().unwrap())
						}
					},
					syn::PathSegment{ref ident, arguments: syn::PathArguments::None} => {
						if ident == "String" {
							Some(parse_quote!(str))
						} else {
							panic!("Field `{}` must be a type path with 1 type param, `String`, or a reference.", field.ident.as_ref().unwrap())
						}
					},
					_ => {
						panic!("Field `{}` must be a type path with 1 type param, `String`, or a reference.", field.ident.as_ref().unwrap())
					},
				}
			} else if let syn::Type::Reference(syn::TypeReference{elem: ref box_ty, ..}) = field.ty {
				Some((**box_ty).clone())
			} else {
				panic!("Field `{}` must be a type path with 0 type param, `String`, or a reference.", field.ident.as_ref().unwrap())
			}
		} else {
			None
		});

		let target_ty_hack_erased = target_ty_hack.as_ref().map(|tth| {
			let mut eraser = RentalLifetimeEraser{
				fields: &rfields,
				used_rlt_args: &mut Vec::new(),
			};

			syn::fold::fold_type(&mut eraser, tth.clone())
		});

		rfields.push(RentalField{
			name: field.ident.unwrap().clone(),
			orig_ty: field.ty.clone(),
			erased: syn::Field{
				colon_token: field.colon_token,
				ident: field.ident.clone(),
				vis: field.vis.clone(),
				attrs: rfattrs,
				ty: rty,
			},
			subrental: subrental,
			self_rlt_args: self_rlt_args,
			used_rlt_args: used_rlt_args,
			target_ty_hack: target_ty_hack,
			target_ty_hack_erased: target_ty_hack_erased,
		});
	}

	(rfields, fields_brace)
}


fn make_borrow_quotes(self_arg: &quote::Tokens, fields: &[RentalField], is_rental_mut: bool) -> Vec<BorrowQuotes> {
	(0 .. fields.len()).map(|idx| {
		let (field_ty, deref) = if idx == fields.len() - 1 {
			let orig_ty = &fields[idx].orig_ty;
			(
				quote!(#orig_ty),
				quote!()
			)
		} else {
			let orig_ty = &fields[idx].orig_ty;
			(
				quote!(<#orig_ty as __rental_prelude::Deref>::Target),
				quote!(*)
			)
		};

		let field_ty_hack = fields[idx].target_ty_hack.as_ref().unwrap_or(&fields[idx].orig_ty);
		let field_ty_hack_erased = fields[idx].target_ty_hack_erased.as_ref().unwrap_or(&fields[idx].erased.ty);

		if let Some(ref subrental) = fields[idx].subrental {
			let field_ident = &fields[idx].name;
			let rental_trait_ident = &subrental.rental_trait_ident;
			let field_rlt_args = &fields[idx].self_rlt_args;

			let (ref borrow_ty_hack, ref borrow_mut_ty_hack, ref field_args) = if let syn::Type::Path(syn::TypePath{ref qself, path: ref ty_path}) = *field_ty_hack {
				let seg_idx = ty_path.segments.len() - 1;
				let ty_name = &ty_path.segments[seg_idx].ident.as_ref();

				let mut borrow_ty_path = ty_path.clone();
				borrow_ty_path.segments[seg_idx].ident = syn::Ident::new(&format!("{}_Borrow", ty_name), Span::call_site());
				borrow_ty_path.segments[seg_idx].arguments = syn::PathArguments::None;

				let mut borrow_mut_ty_path = ty_path.clone();
				borrow_mut_ty_path.segments[seg_idx].ident = syn::Ident::new(&format!("{}_BorrowMut", ty_name), Span::call_site());
				borrow_mut_ty_path.segments[seg_idx].arguments = syn::PathArguments::None;

				match ty_path.segments[seg_idx].arguments {
					syn::PathArguments::AngleBracketed(ref args) => {
						(
							syn::Type::Path(syn::TypePath{qself: qself.clone(), path: borrow_ty_path}),
							syn::Type::Path(syn::TypePath{qself: qself.clone(), path: borrow_mut_ty_path}),
							args.args.iter().collect::<Vec<_>>(),
						)
					},
					syn::PathArguments::None => {
						(
							syn::Type::Path(syn::TypePath{qself: qself.clone(), path: borrow_ty_path}),
							syn::Type::Path(syn::TypePath{qself: qself.clone(), path: borrow_mut_ty_path}),
							Vec::with_capacity(0),
						)
					},
					_ => panic!("Field `{}` must have angle-bracketed args.", fields[idx].name),
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
					quote!(#borrow_ty_hack<#(#field_rlt_args,)* #(#field_args),*>)
				} else {
					quote!(__rental_prelude::PhantomData<#borrow_ty_hack<#(#field_rlt_args,)* #(#field_args),*>>)
				},
				expr: if idx == fields.len() - 1 || !is_rental_mut {
					quote!(unsafe { <#field_ty_hack_erased>::borrow_erased(&#deref #self_arg.#field_ident) })
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
					quote!(#borrow_mut_ty_hack<#(#field_rlt_args,)* #(#field_args),*>)
				} else if !is_rental_mut {
					quote!(#borrow_ty_hack<#(#field_rlt_args,)* #(#field_args),*>)
				} else {
					quote!(__rental_prelude::PhantomData<#borrow_mut_ty_hack<#(#field_rlt_args,)* #(#field_args),*>>)
				},
				mut_expr: if idx == fields.len() - 1 {
					quote!(unsafe { <#field_ty_hack_erased>::borrow_mut_erased(&mut #deref #self_arg.#field_ident) })
				} else if !is_rental_mut {
					quote!(unsafe { <#field_ty_hack_erased>::borrow_erased(&#deref #self_arg.#field_ident) })
				} else {
					quote!(__rental_prelude::PhantomData::<()>)
				},

				new_ty: if !is_rental_mut  {
					//quote!(<#field_ty as __rental_prelude::#rental_trait_ident<#(#field_rlt_args),*>>::Borrow)
					quote!(#borrow_ty_hack<#(#field_rlt_args,)* #(#field_args),*>)
				} else {
					//quote!(<#field_ty as __rental_prelude::#rental_trait_ident<#(#field_rlt_args),*>>::BorrowMut)
					quote!(#borrow_mut_ty_hack<#(#field_rlt_args,)* #(#field_args),*>)
				},
				new_expr: if !is_rental_mut {
					quote!(unsafe { <#field_ty_hack_erased>::borrow_erased(&#deref #field_ident) })
				} else {
					quote!(unsafe { <#field_ty_hack_erased>::borrow_mut_erased(&mut #deref #field_ident) })
				},
			}
		} else {
			let field_ident = &fields[idx].name;
			let field_rlt_arg = &fields[idx].self_rlt_args[0];

			BorrowQuotes {
				ty: if idx == fields.len() - 1 || !is_rental_mut {
					quote!(&#field_rlt_arg (#field_ty))
				} else {
					quote!(__rental_prelude::PhantomData<&#field_rlt_arg #field_ty>)
				},
				ty_hack: if idx == fields.len() - 1 || !is_rental_mut {
					quote!(&#field_rlt_arg (#field_ty_hack))
				} else {
					quote!(__rental_prelude::PhantomData<&#field_rlt_arg #field_ty_hack>)
				},
				expr: if idx == fields.len() - 1 || !is_rental_mut {
					//quote!(& #deref #self_arg.#field_ident)
					quote!(&#deref #self_arg.#field_ident)
				} else {
					quote!(__rental_prelude::PhantomData::<()>)
				},

				mut_ty: if idx == fields.len() - 1 {
					quote!(&#field_rlt_arg mut (#field_ty))
				} else if !is_rental_mut {
					quote!(&#field_rlt_arg (#field_ty))
				} else {
					quote!(__rental_prelude::PhantomData<&#field_rlt_arg mut #field_ty>)
				},
				mut_ty_hack: if idx == fields.len() - 1 {
					quote!(&#field_rlt_arg mut (#field_ty_hack))
				} else if !is_rental_mut {
					quote!(&#field_rlt_arg (#field_ty_hack))
				} else {
					quote!(__rental_prelude::PhantomData<&#field_rlt_arg mut #field_ty_hack>)
				},
				mut_expr: if idx == fields.len() - 1 {
					//quote!(&mut #deref #self_arg.#field_ident)
					quote!(&mut #deref #self_arg.#field_ident)
				} else if !is_rental_mut {
					quote!(&#deref #self_arg.#field_ident)
				} else {
					quote!(__rental_prelude::PhantomData::<()>)
				},

				new_ty: if !is_rental_mut {
					//quote!(&#field_rlt_arg #field_ty)
					quote!(&#field_rlt_arg (#field_ty_hack))
				} else {
					//quote!(&#field_rlt_arg mut #field_ty)
					quote!(&#field_rlt_arg mut (#field_ty_hack))
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
			let mut prev_rlt_args = Vec::<syn::Lifetime>::new();
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


struct RentalStructAttribs {
	pub is_rental_mut: bool,
	pub is_deref_suffix: bool,
	pub is_deref_mut_suffix: bool,
	pub is_debug_borrow: bool,
	pub doc: Vec<syn::Attribute>,
}


struct RentalField {
	pub name: syn::Ident,
	pub orig_ty: syn::Type,
	pub erased: syn::Field,
	pub subrental: Option<Subrental>,
	pub self_rlt_args: Vec<syn::Lifetime>,
	pub used_rlt_args: Vec<syn::Lifetime>,
	pub target_ty_hack: Option<syn::Type>,
	pub target_ty_hack_erased: Option<syn::Type>,
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


impl<'a> syn::fold::Fold for RentalLifetimeEraser<'a> {
	fn fold_lifetime(&mut self, lifetime: syn::Lifetime) -> syn::Lifetime {
		if self.fields.iter().any(|field| field.self_rlt_args.contains(&lifetime)) {
			if !self.used_rlt_args.contains(&lifetime) {
				self.used_rlt_args.push(lifetime.clone());
			}

			syn::Lifetime::new(Term::intern("'static"), Span::def_site())
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

		if let Ok(item) = syn::parse::<syn::ItemStruct>(item.into()) {
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
