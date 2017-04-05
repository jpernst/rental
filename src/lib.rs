#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
extern crate core;
#[macro_use]
extern crate rental_impl;
extern crate stable_deref_trait;

pub use rental_impl::*;


pub trait IntoSuffix {
	type Suffix;

	fn into_suffix(self) -> <Self as IntoSuffix>::Suffix;
}


pub struct TryNewError<E, H> (pub E, pub H);
pub type TryNewResult<T, E, H> = Result<T, TryNewError<E, H>>;


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

	pub use super::{IntoSuffix, TryNewError, TryNewResult};


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
