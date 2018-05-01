#[macro_use]
extern crate rental;


#[derive(Debug)]
pub struct Foo {
	i: i32,
}


rental! {
	mod rentals {
		use super::*;

		#[rental(derive_debug, deref_suffix)]
		pub struct SimpleRef {
			foo: Box<Foo>,
			iref: &'foo i32,
		}
	}
}


#[test]
fn print() {
	let foo = Foo { i: 5 };
	let sr = rentals::SimpleRef::new(Box::new(foo), |foo| &foo.i);

	println!("{:?}", sr);
}
