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
	let mut rstruct = item.clone();

	if let Some(rattr_pos) = item.attrs.iter().position(|a| if let syn::MetaItem::Word(ref ident) = a.value { ident == "rental" } else { false }) {
		rstruct.attrs.remove(rattr_pos);
	} else {
		panic!("Struct must have `rental` attribute");
	}

	if let syn::ItemKind::Struct(ref mut data, ref gen) = rstruct.node {
		let mut fields = match *data {
			syn::VariantData::Struct(ref mut fields) => panic!("Only tuple structs are currently supported."),
			syn::VariantData::Tuple(ref mut fields) => fields,
			_ => panic!("Struct must have fields."),
		};

		let mut rfields = Vec::with_capacity(fields.len());
		for (idx, mut field) in fields.iter_mut().enumerate() {
			if field.vis != syn::Visibility::Inherited {
				panic!("Fields must be private.");
			}

			let mut rlt_params = Vec::new();

			rfields.push(RentalField{idx: idx, rlt_params: rlt_params});
		}
	} else {
		panic!("Item is not a struct.");
	}

	rstruct.to_tokens(tokens);


}


struct RentalField {
	idx: usize,
	rlt_params: Vec<String>,
}
