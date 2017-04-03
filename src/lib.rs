#![feature(manually_drop)]
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
extern crate core;
#[macro_use]
extern crate rental_impl;
extern crate stable_deref_trait;

pub use rental_impl::*;

#[doc(hidden)]
pub mod __rental_prelude {
	pub use core::marker::PhantomData;
	pub use core::ops::{Deref, DerefMut};
	pub use core::convert::Into;
	pub use core::mem::transmute;
	pub use core::result::Result;
	pub use stable_deref_trait::StableDeref;


	pub struct TryNewError<E, H> (pub E, pub H);
	pub type TryNewResult<T, E, H> = Result<T, TryNewError<E, H>>;

	pub trait Rental2<'a0: 'a1, 'a1> { type Borrow; type BorrowMut; type Prefix; }
	pub trait Rental3<'a0: 'a1, 'a1: 'a2, 'a2> { type Borrow; type BorrowMut; type Prefix; }
	pub trait Rental4<'a0: 'a1, 'a1: 'a2, 'a2: 'a3, 'a3> { type Borrow; type BorrowMut; type Prefix; }
	pub trait Rental5<'a0: 'a1, 'a1: 'a2, 'a2: 'a3, 'a3: 'a4, 'a4> { type Borrow; type BorrowMut; type Prefix; }
	pub trait Rental6<'a0: 'a1, 'a1: 'a2, 'a2: 'a3, 'a3: 'a4, 'a4: 'a5, 'a5> { type Borrow; type BorrowMut; type Prefix; }
	pub trait Rental7<'a0: 'a1, 'a1: 'a2, 'a2: 'a3, 'a3: 'a4, 'a4: 'a5, 'a5: 'a6, 'a6> { type Borrow; type BorrowMut; type Prefix; }
	pub trait Rental8<'a0: 'a1, 'a1: 'a2, 'a2: 'a3, 'a3: 'a4, 'a4: 'a5, 'a5: 'a6, 'a6: 'a7, 'a7> { type Borrow; type BorrowMut; type Prefix; }
	pub trait Rental9<
		'a0: 'a1, 'a1: 'a2, 'a2: 'a3, 'a3: 'a4, 'a4: 'a5, 'a5: 'a6, 'a6: 'a7, 'a7: 'a8,
		'a8
	> { type Borrow; type BorrowMut; type Prefix; }
	pub trait Rental10<
		'a0: 'a1, 'a1: 'a2, 'a2: 'a3, 'a3: 'a4, 'a4: 'a5, 'a5: 'a6, 'a6: 'a7, 'a7: 'a8,
		'a8: 'a9, 'a9
	> { type Borrow; type BorrowMut; type Prefix; }
	pub trait Rental11<
		'a0: 'a1, 'a1: 'a2, 'a2: 'a3, 'a3: 'a4, 'a4: 'a5, 'a5: 'a6, 'a6: 'a7, 'a7: 'a8,
		'a8: 'a9, 'a9: 'a10, 'a10
	> { type Borrow; type BorrowMut; type Prefix; }
	pub trait Rental12<
		'a0: 'a1, 'a1: 'a2, 'a2: 'a3, 'a3: 'a4, 'a4: 'a5, 'a5: 'a6, 'a6: 'a7, 'a7: 'a8,
		'a8: 'a9, 'a9: 'a10, 'a10: 'a11, 'a11
	> { type Borrow; type BorrowMut; type Prefix; }
	pub trait Rental13<
		'a0: 'a1, 'a1: 'a2, 'a2: 'a3, 'a3: 'a4, 'a4: 'a5, 'a5: 'a6, 'a6: 'a7, 'a7: 'a8,
		'a8: 'a9, 'a9: 'a10, 'a10: 'a11, 'a11: 'a12, 'a12
	> { type Borrow; type BorrowMut; type Prefix; }
	pub trait Rental14<
		'a0: 'a1, 'a1: 'a2, 'a2: 'a3, 'a3: 'a4, 'a4: 'a5, 'a5: 'a6, 'a6: 'a7, 'a7: 'a8,
		'a8: 'a9, 'a9: 'a10, 'a10: 'a11, 'a11: 'a12, 'a12: 'a13, 'a13
	> { type Borrow; type BorrowMut; type Prefix; }
	pub trait Rental15<
		'a0: 'a1, 'a1: 'a2, 'a2: 'a3, 'a3: 'a4, 'a4: 'a5, 'a5: 'a6, 'a6: 'a7, 'a7: 'a8,
		'a8: 'a9, 'a9: 'a10, 'a10: 'a11, 'a11: 'a12, 'a12: 'a13, 'a13: 'a14, 'a14
	> { type Borrow; type BorrowMut; type Prefix; }
	pub trait Rental16<
		'a0: 'a1, 'a1: 'a2, 'a2: 'a3, 'a3: 'a4, 'a4: 'a5, 'a5: 'a6, 'a6: 'a7, 'a7: 'a8,
		'a8: 'a9, 'a9: 'a10, 'a10: 'a11, 'a11: 'a12, 'a12: 'a13, 'a13: 'a14, 'a14: 'a15, 'a15
	> { type Borrow; type BorrowMut; type Prefix; }


	#[inline(always)]
	pub fn static_assert_stable_deref<T: StableDeref>() { }
}


#[macro_export]
macro_rules! rental {
	{
		$(#[$attr:meta])*
		mod $rental_mod:ident {
			$($body:tt)*
		}
	} => {
		$(#[$attr])*
		mod $rental_mod {
			#[allow(unused_imports)]
			use $crate::__rental_prelude;

			#[allow(unused)]
			#[derive(__rental_impl)]
			enum ProceduralMasqueradeDummyType {
				Input = (0, stringify!($($body)*)).0
			}
		}
	};
	{
		$(#[$attr:meta])*
		pub mod $rental_mod:ident {
			$($body:tt)*
		}
	} => {
		$(#[$attr])*
		pub mod $rental_mod {
			#[allow(unused_imports)]
			use $crate::__rental_prelude;

			#[allow(unused)]
			#[derive(__rental_impl)]
			enum ProceduralMasqueradeDummyType {
				Input = (0, stringify!($($body)*)).0
			}
		}
	};
	{
		$(#[$attr:meta])*
		pub($($vis:tt)*) mod $rental_mod:ident {
			$($body:tt)*
		}
	} => {
		$(#[$attr])*
		pub($($vis)*) mod $rental_mod {
			#[allow(unused_imports)]
			use $crate::__rental_prelude;

			#[allow(unused)]
			#[derive(__rental_impl)]
			enum ProceduralMasqueradeDummyType {
				Input = (0, stringify!($($body)*)).0
			}
		}
	};
}


pub struct A {
	i: i32,
}
pub struct B<'a> {
	a: &'a A,
}
pub struct C<'a: 'b, 'b> {
	b: &'b B<'a>,
}

impl A {
	pub fn borrow(&self) -> B {
		B { a: self }
	}
}

impl Drop for A {
	fn drop(&mut self) { }
}
impl<'a> Drop for B<'a> {
	fn drop(&mut self) { }
}
impl<'a, 'b> Drop for C<'a, 'b> {
	fn drop(&mut self) { }
}

impl<'a> B<'a> {
	pub fn borrow_again<'b>(&'b self) -> C<'a, 'b> {
		C { b: self }
	}
}

pub fn test() {
	use rental_mod::{Foo, Bar};

	let a = A { i: 5 };
	let foo = Foo::new(Box::new(a), |a| a.borrow());

	let bar = Bar::new(Box::new(foo), |fb| fb.b.borrow_again());
}



rental!{
	pub mod rental_mod {
		use super::{A, B, C};

		#[rental]
		pub struct Foo {
			a: Box<A>,
			b: B<'a>,
			//b: Box<B<'a>>,
			//c: C<'a, 'b>,
		}

		#[rental]
		pub struct Bar {
			#[subrental(arity = 2)]
			a: Box<Foo>,
			c: C<'a_0, 'a_1>,
		}
	}
}
