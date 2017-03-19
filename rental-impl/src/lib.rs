extern crate proc_macro;
#[macro_use]
extern crate procedural_masquerade;
extern crate syn;
extern crate quote;

use quote::ToTokens;


define_proc_macros! {
	#[doc(hidden)]
    #[allow(non_snake_case)]
    pub fn rental__define_struct(input: &str) -> String {
		let mut tokens = quote::Tokens::new();

		for item in syn::parse_items(input).expect("Failed to parse items in module body!") {
			match item.node {
				syn::ItemKind::Use(..) => {
					item.to_tokens(&mut tokens);
				},
				syn::ItemKind::Struct(ref data, ref gen) => {
					write_rental_struct_and_impls(&mut tokens, &item, data, gen);
				},
				_ => panic!("Illegal item kind (only `use` and `struct` allowed)!"),
			}
		}

		tokens.to_string()
    }
}


fn write_rental_struct_and_impls(mut tokens: &mut quote::Tokens, item: &syn::Item, data: &syn::VariantData, gen: &syn::Generics) {
	item.to_tokens(&mut tokens);
}
