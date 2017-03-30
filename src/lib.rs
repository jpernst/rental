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
	pub use core::mem::transmute;
	pub use core::result::Result;
	pub use stable_deref_trait::StableDeref;

	pub use super::{TryNewError, TryNewResult};


	#[inline(always)]
	pub fn static_assert_stable_deref<T: StableDeref>() { }
}


pub struct TryNewError<E, H> (pub E, pub H);
pub type TryNewResult<T, E, H> = Result<T, TryNewError<E, H>>;


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


pub struct A { }
pub struct B<'a> {
	_a: &'a A,
}

rental!{
	mod rental_mod {
		use super::{A, B};

		#[rental]
		pub struct Foo {
			a: Box<A>,
			b: B<'a>,
		}

	}
}
