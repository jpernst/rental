#[macro_use]
extern crate rental;


pub struct Foo {
	i: i32,
}

impl Foo {
	fn try_borrow(&self) -> Result<&i32, ()> { Ok(&self.i) }
	fn fail_borrow(&self) -> Result<&i32, ()> { Err(()) }
}


rental! {
	mod rentals {
		use super::*;

		#[rental(deref_suffix)]
		pub struct SimpleRef {
			foo: Box<Foo>,
			iref: &'foo i32,
		}
	}
}


#[test]
fn new() {
	let foo = Foo { i: 5 };
	let _ = rentals::SimpleRef::new(Box::new(foo), |foo| &foo.i);

	let foo = Foo { i: 5 };
	let sr = rentals::SimpleRef::try_new(Box::new(foo), |foo| foo.try_borrow());
	assert!(sr.is_ok());

	let foo = Foo { i: 5 };
	let sr = rentals::SimpleRef::try_new(Box::new(foo), |foo| foo.fail_borrow());
	assert!(sr.is_err());
}


#[test]
fn read() {
	let foo = Foo { i: 5 };

	let mut sr = rentals::SimpleRef::new(Box::new(foo), |foo| &foo.i);

	{
		let i: i32 = sr.rent(|iref| **iref);
		assert_eq!(i, 5);
	}

	{
		let iref: &i32 = sr.ref_rent(|iref| *iref);
		assert_eq!(*iref, 5);
	}

	assert_eq!(*sr, 5);

	assert_eq!(sr.rent_all_mut(|borrows| borrows.foo.i), 5);
}
