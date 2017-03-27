#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
extern crate core;
#[macro_use]
extern crate rental_impl;

pub use rental_impl::*;


#[doc(hidden)]
pub mod __rental_prelude {
	pub use core;
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
			use $crate::__rental_prelude;

			#[derive(__rental_impl)]
			#[allow(unused)]
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
			use $crate::__rental_prelude;

			#[derive(__rental_impl)]
			#[allow(unused)]
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
			use $crate::__rental_prelude;

			#[derive(__rental_impl)]
			#[allow(unused)]
			enum ProceduralMasqueradeDummyType {
				Input = (0, stringify!($($body)*)).0
			}
		}
	};
}


pub struct A { }
pub struct B<'a> {
	a: &'a A,
}

rental!{
	mod rental_mod {
		use super::{A, B};

		#[rental]
		pub struct Foo {
			a: A,
			b: B<'a>,
		}
	}
}
