#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
extern crate core;
#[macro_use]
extern crate rental_impl;
extern crate stable_deref_trait;

pub use rental_impl::*;


macro_rules! define_rental_traits {
	($max_arity:expr) => {
		#[allow(unused)]
		#[derive(__rental_traits)]
		enum ProceduralMasqueradeDummyType {
			Input = (0, stringify!($max_arity)).0
		}
	};
}


#[doc(hidden)]
pub mod __rental_prelude {
	pub use core::marker::PhantomData;
	pub use core::ops::{Deref, DerefMut, Drop};
	pub use core::convert::{AsRef, AsMut, Into};
	pub use core::borrow::{Borrow, BorrowMut};
	pub use core::mem::transmute;
	pub use core::result::Result;
	pub use core::option::Option;

	pub use stable_deref_trait::StableDeref;


	pub struct TryNewError<E, H> (pub E, pub H);
	pub type TryNewResult<T, E, H> = Result<T, TryNewError<E, H>>;


	pub trait IntoSuffix {
		type Suffix;

		fn into_suffix(self) -> <Self as IntoSuffix>::Suffix;
	}


	define_rental_traits!(32);


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
			#[derive(__rental_structs_and_impls)]
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
			#[derive(__rental_structs_and_impls)]
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
			#[derive(__rental_structs_and_impls)]
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
	x: i32
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
		C { b: self, x: 12 }
	}
}

impl<'a: 'b, 'b> ::std::ops::Deref for C<'a, 'b> {
	type Target = i32;

	fn deref(&self) -> &i32 { &self.x }
}
impl<'a: 'b, 'b> ::std::ops::DerefMut for C<'a, 'b> {
	fn deref_mut(&mut self) -> &mut i32 { &mut self.x }
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
		}

		#[rental(deref_mut_suffix)]
		pub struct Bar {
			#[subrental(arity = 2)]
			a: Box<Foo>,
			c: C<'a_0, 'a_1>,
		}
	}
}
