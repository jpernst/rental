extern crate proc_macro;
#[macro_use]
extern crate procedural_masquerade;
extern crate syn;
extern crate quote;

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
	if let Some(rattr_pos) = rattrs.iter().position(|a| if let syn::MetaItem::Word(ref ident) = a.value { ident == "rental" } else { false }) {
		rattrs.remove(rattr_pos);
	} else {
		panic!("Struct must have `rental` attribute");
	}

	let (is_tup, fields) = match *data {
		syn::VariantData::Struct(ref fields) => (false, fields),
		syn::VariantData::Tuple(ref fields) => (true, fields),
		_ => panic!("Struct must have fields."),
	};

	let mut rfields = Vec::with_capacity(fields.len());
	for (i, f) in fields.iter().enumerate() {
		if f.vis != syn::Visibility::Inherited {
			panic!("Fields must be private.");
		}

		let mut rlt_args = Vec::new();
		let rty = {
			let mut eraser = RentalLifetimeEraser{
				rfields: &rfields,
				rlt_args: &mut rlt_args,
			};

			syn::fold::noop_fold_ty(&mut eraser, f.ty.clone())
		};

		rfields.push(RentalField{
			field: syn::Field{
				ident: f.ident.clone(),
				vis: f.vis.clone(),
				attrs: f.attrs.clone(),
				ty: rty,
			},
			nested_lts: 0,
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
	pub nested_lts: usize,
	pub rlt_args: Vec<RentalLifetimeArg>,
}


struct RentalLifetimeArg {
	pub field_idx: usize,
	pub nested_idx: Option<usize>,
}


struct RentalLifetimeEraser<'a, 'b> {
	pub rfields: &'a [RentalField],
	pub rlt_args: &'b mut Vec<RentalLifetimeArg>,
}


impl<'a, 'b> syn::fold::Folder for RentalLifetimeEraser<'a, 'b> {
	fn fold_lifetime(&mut self, lifetime: syn::Lifetime) -> syn::Lifetime {
		if let Some(idx) = self.rfields.iter().enumerate().position(|(i, rf)| {
			if let Some(ref ident) = rf.field.ident {
				lifetime.ident == ident
			} else {
				lifetime.ident == syn::Ident::new(format!("'_{}", i))
			}
		}) {
			syn::Lifetime::new("'static")
		} else {
			lifetime
		}
	}
}
