//! A macro to generate self-borrowing structs.
//! 
//! # Overview
//! It can sometimes occur in the course of designing an API that it would be convenient, or even necessary, to allow fields within a struct to hold references to other fields within that same struct. Rust's concept of ownership and borrowing is quite flexible, but can't quite express such a scenario yet.
//!
//! Creating such a struct manually requires unsafe code to erase lifetime parameters from the field types. Accessing such a field directly would be completely unsafe as a result. This library addresses the issue by allowing access to the internal fields only under carefully controlled circumstances, through closures that are bounded by generic lifetimes to prevent infiltration or exfiltration of any data with an incorrect lifetime.
//! 
//! The API of this crate consists of the [`rental`](macro.rental.html) macro, which generates self-referential structs, and a few example instantiations to demonstrate the API provided by such structs (see [`RentRef`](struct.RentRef.html), [`RentMut`](struct.RentMut.html), [`RentRefMap`](struct.RentRefMap.html), and [`RentMutMap`](struct.RentMutMap.html)).
//! 
//! # Use Cases
//! One instance where this crate is useful is when working with `libloading`. That crate provides a `Library` struct that defines methods to borrow `Symbol`s from it. These symbols are bounded by the lifetime of the library, and are thus considered a borrow. Under normal circumstances, one would be unable to store both the library and the symbols within a single struct, but the macro defined in this crate allows you to define a struct that is capable of storing both simultaneously, like so:
//! 
//! ```rust,ignore
//! rental! {
//!     pub mod rent_libloading {
//!         use libloading;
//! 
//!         #[rental(deref_suffix)] // This struct will deref to the target of Symbol.
//!         pub struct RentSymbol<S: 'static> {
//!             lib: Box<libloading::Library>, // Library is boxed for stable deref.
//!             sym: libloading::Symbol<'lib, S>, // The 'lib lifetime borrows lib.
//!         }
//!     }
//! }
//! 
//! fn main() {
//!     let lib = libloading::Library::new("my_lib.so").unwrap(); // Open our dylib.
//!     if let Ok(rs) = rent_libloading::RentSymbol::try_new(
//!         Box::new(lib),
//!         |lib| unsafe { lib.get::<extern "C" fn()>(b"my_symbol") }) // Loading Symbols is unsafe.
//!     {
//!         (*rs)(); // Call our function
//!     };
//! }
//! ```
//! 
//! In this way we can store both the `Library` and the `Symbol` that borrows it in a single struct. We can even tell our struct to deref to the function pointer itself so we can easily call it. This is legal because the function pointer does not contain any of the special lifetimes introduced by the rental struct in its type signature, which means reborrowing will not expose them to the outside world. As an aside, the `unsafe` block for loading the symbol is necessary because the act of loading a symbol from a dylib is unsafe, and is unrelated to rental.
//! 
//! For a more advanced case, let's take a look at the `alto` OpenAL bindings. Alto defines an `Alto` struct that represents the API, from which we can borrow an output `Device`, from which we can borrow a device `Context`. This is a 3-level self-referential relationship, but rental can handle it just fine:
//! 
//! ```rust,ignore
//! rental! {
//!     pub mod rent_alto {
//!         use alto;
//! 
//!         #[rental]
//!         pub struct RentContext {
//!             alto: Box<alto::Alto>,
//!             dev: Box<alto::Device<'alto>>,
//!             ctx: alto::Context<'dev>,
//!         }
//!     }
//! }
//! 
//! fn main() {
//!     let alto = alto::Alto::load_default().unwrap(); // Load the default OpenAL impl.
//!     if let Ok(rent_ctx) = rent_alto::RentContext::try_new(
//!         Box::new(alto),
//!         |alto| alto.open(None).map(|dev| Box::new(dev)), // Open the default device.
//!         |dev, _alto| dev.new_context(None), // Create a new context for our device.
//!     ) {
//!         rent_ctx.rent(|ctx| {
//!             // Do stuff with our context
//!         });
//!     };
//! }
//! ```
//! 
//! This approach can be extended to as many fields as you like, up to a current implementation defined maximum. Read below for more information on current limitations.
//! 
//! # Limitations
//! There are a few limitations with the current implementation. These limitations aren't fundamental, but rather the result of bugs or pending features in rust itself, and will be lifted once the underlying language allows it.
//! 
//! * Currently, the rental struct itself cannot take lifetime parameters, and any type parameters it takes must be `'static`. In most situations this is fine, since most of the use cases for this library involve erasing all of the lifetimes anyway, but there's no reason why the head element of a rental struct shouldn't be able to be non-`'static`. This is currently impossible to implement due to lack of an `'unsafe` lifetime or equivalent feature.
//! * Prefix fields must be of the form `Foo<T>` where `Foo` is some `StableDeref` container. This requirement is syntactic as well as semantic, meaning that if you use some type alias `MyAlias = Foo<T>` compilation will fail because it cannot parse the type information it needs. Just use the literal type and everything should be fine. This limitation can be lifted once rust gets ATC or possibly after current bugs surrounding HRTB associated item unification are fixed. The requirement for `StableDeref` at all can possibly be lifted if rust gains a notion of immovable types.
//! * Rental structs can only have a maximum of 32 rental lifetimes, including transitive rental lifetimes from subrentals. This limitation is the result of needing to implement a new trait for each rental arity. More traits can easily be defined though. Lifting this limitation entirely would require some kind of variadic lifetime generics.


#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
extern crate core;
#[macro_use]
extern crate rental_impl;
extern crate stable_deref_trait;

#[doc(hidden)]
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



rental! {
	pub mod examples {
		#[rental(deref_suffix)]
		pub struct RentRef<T: 'static, U: 'static> {
			head: Box<T>,
			suffix: &'head U,
		}

		#[rental_mut(deref_mut_suffix)]
		pub struct RentMut<T: 'static, U: 'static> {
			head: Box<T>,
			suffix: &'head mut U,
		}

		#[rental(deref_suffix)]
		pub struct RentRefMap<T: 'static, U: 'static, V: 'static> {
			#[subrental(arity = 2)]
			head: Box<RentRef<T, U>>,
			suffix: &'head_1 V,
		}

		#[rental_mut(deref_mut_suffix)]
		pub struct RentMutMap<T: 'static, U: 'static, V: 'static> {
			#[subrental(arity = 2)]
			head: Box<RentMut<T, U>>,
			suffix: &'head_1 mut V,
		}
	}
}
