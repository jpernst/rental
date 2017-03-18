extern crate proc_macro;
#[macro_use]
extern crate procedural_masquerade;
extern crate syn;
#[macro_use]
extern crate quote;


define_proc_macros! {
    #[allow(non_snake_case)]
    pub fn rental__define_struct(input: &str) -> String {
		let mut tokens = quote::Tokens::new();

		for item in syn::parse_items(input).expect("Failed to parse module body!") {
			match item.node {
				syn::ItemKind::Use(ref vp) => { },
				syn::ItemKind::Struct(ref vd, ref g) => { },
				_ => panic!("Unsupported item kind (only `use` and `struct` allowed)!"),
			}
		}

		input.to_string()
    }
}
