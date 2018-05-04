//! A macro to generate safe self-referntial structs.
//! 
//! # Overview
//! It can sometimes occur in the course of designing an API that it would be convenient, or even necessary, to allow fields within a struct to hold references to other fields within that same struct. Rust's concept of ownership and borrowing is powerful, but can't express such a scenario yet.
//!
//! Creating such a struct manually would require unsafe code to erase lifetime parameters from the field types. Accessing the fields directly would be completely unsafe as a result. This library addresses that issue by allowing access to the internal fields only under carefully controlled circumstances, through closures that are bounded by generic lifetimes to prevent infiltration or exfiltration of any data with an incorrect lifetime. In short, while the struct internally uses unsafe code to store the fields, the interface exposed to the consumer of the struct is completely safe. The implementation of this interface is subtle and verbose, hence the macro to automate the process.
//! 
//! The API of this crate consists of the [`rental`](macro.rental.html) macro, which generates safe self-referential structs, and a few example instantiations to demonstrate the API provided by such structs (see [`RentRef`](examples/struct.RentRef.html), [`RentMut`](examples/struct.RentMut.html), [`RentRefMap`](examples/struct.RentRefMap.html), and [`RentMutMap`](examples/struct.RentMutMap.html)).
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
//! # Limitations
//! There are a few limitations with the current implementation. These limitations aren't fundamental, but rather the result of bugs or pending features in rust itself, and will be lifted once the underlying language allows it.
//! 
//! * Currently, the rental struct itself can only take lifetime parameters under certain conditions. These conditions are difficult to fully describe, but in general, a lifetime param of the rental struct itself must appear "outside" of any special rental lifetimes in the type signatures of the struct fields. To put it another way, replacing the rental lifetimes with `'static` must still produce legal types, otherwise it will not compile. In most situations this is fine, since most of the use cases for this library involve erasing all of the lifetimes anyway, but there's no reason why the head element of a rental struct shouldn't be able to take arbitrary lifetime params. This is currently impossible to fully support due to lack of an `'unsafe` lifetime or equivalent feature.
//! * Prefix fields must be of the form `Foo<T>` where `Foo` is some `StableDeref` container, or rental will not be able to correctly guess the `Deref::Target` of the type. If you are using a custom type that does not fit this pattern, you can use the `target_ty_hack` attribute on the field to manually specify the target type. This limitation can be lifted once rust gets ATC or possibly after current bugs surrounding HRTB associated item unification are fixed. The requirement for `StableDeref` at all can possibly be lifted if rust gains a notion of immovable types.
//! * Rental structs can only have a maximum of 32 rental lifetimes, including transitive rental lifetimes from subrentals. This limitation is the result of needing to implement a new trait for each rental arity. More traits can easily be defined though. Lifting this limitation entirely would require some kind of variadic lifetime generics.
//! * The references received in the constructor closures don't currently have their lifetime relationship expressed in bounds. This is currently not possible until the trait system refactor lands. This is not a soundness hole, but it does prevent some otherwise valid uses from compiling.


#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
extern crate core;
#[macro_use]
extern crate rental_impl;
extern crate stable_deref_trait;

#[doc(hidden)]
pub use rental_impl::*;


/// This trait converts any `*_Borrow` or `*_BorrowMut` structs generated by the [`rental`](macro.rental.html) macro into their suffix (most dependent) field.
///
/// When you own a borrow struct, such as in the body of the closure provided to the `rent_all` or `ref_rent_all` methods of a rental struct, you can call `into_suffix()` to discard the borrow struct and obtain the suffix field if you don't need any of the other fields.
pub trait IntoSuffix {
	/// Type of the transitive suffix of the borrow struct.
	///
	/// If the suffix field of the borrow struct is itself a borrow struct of a subrental, then this type is the suffix of that nested borrow struct, recursively.
	type Suffix;

	/// Discard the borrow struct and return the transitive suffix field.
	///
	/// If the suffix field of the borrow struct is itself a borrow struct of a subrental, then this function will return the nested suffix of that borrow struct, recursively.
	fn into_suffix(self) -> <Self as IntoSuffix>::Suffix;
}


/// An error wrapper returned by the `try_new` method of a rental struct.
///
/// This will contain the first error returned by the closure chain, as well as the original head value you passed in so you can do something else with it.
pub struct RentalError<E, H> (pub E, pub H);

pub type RentalResult<T, E, H> = Result<T, RentalError<E, H>>;


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
	pub use core::clone::Clone;
	pub use core::ops::{FnOnce, Deref, DerefMut, Drop};
	pub use core::convert::{AsRef, AsMut, Into};
	pub use core::borrow::{Borrow, BorrowMut};
	pub use core::mem::transmute;
	pub use core::result::Result;
	pub use core::option::Option;
	pub use core::fmt;
	pub use stable_deref_trait::{StableDeref, CloneStableDeref};

	pub use super::{IntoSuffix, RentalError, RentalResult};


	define_rental_traits!(32);


	#[inline(always)]
	pub fn static_assert_stable_deref<T: StableDeref>() { }
	#[inline(always)]
	pub fn static_assert_mut_stable_deref<T: DerefMut + StableDeref>() { }
}


/// The core of this crate, this macro will generate self-referential structs.
/// 
/// This macro is invoked in item position. The body parses as valid rust and contains no special syntax. Only certain constructs are allowed, and a few special attributes and lifetimes are recognized.
/// 
/// To start, the the top level item of the macro invocation must be a single module. This module will contain all items that the macro generates and export them to you. Within the module, only three types of items are accepted: `use` statements, type aliases, and struct definitions. The `use` statements and type aliases are passed directly through with no special consideration; the primary concern is the struct definitions.
/// 
/// First, all struct definitions must have a `#[rental]` or `#[rental_mut]` attribute to indicate that they are self-referential. The `mut` variant indicates that the struct mutably borrows itself, while the normal attribute assumes shared borrows. These attributes may optionally take the arguments `debug_borrow`, `deref_suffix`, and `deref_mut_suffix`, discussed below.
/// 
/// Next, the structs must have named fields (no tuple structs) and they must have at least 2 fields, since a struct with 1 field can't meaningfully reference itself anyway.
/// 
/// The order of the fields is significant, as they are declared in order of least to most dependent. The first field, also referred to as the "head" of the struct, contains no self-references, the second field may borrow the first, the third field may borrow the second or first, and so on. The chain of fields after the head is called the "tail".
/// 
/// Because rental structs are self-referential, special care is taken to ensure that moving the struct will not invalidate any internal references. This is accomplished by requiring all fields but the last one, collectively known as the "prefix" of the struct, to implement [`StableDeref`](https://crates.io/crates/stable_deref_trait). This is not required for the final field of the struct, known as the "suffix", since nothing holds a reference to it.
///
/// NOTE: Because of a workaround for a compiler bug, rental might not always correctly determine the `Deref::Target` type of your prefix fields. If you receive type errors when compiling, you can try using the `target_ty_hack` attribute on the field of the struct. Set this attribute equal to a string that names the correct target type (e.g. `#[target_ty_hack = "[u8]"]` for `Vec<u8>`.
/// 
/// Each field that you declare creates a special lifetime of the same name that can be used by later fields to borrow it. This is how the referential relationships are established in the struct definition.
/// 
/// This is a all a bit to chew on so far, so let's stop and take a look at an example:
///
/// ```rust
/// # #[macro_use] extern crate rental;
/// pub struct Foo { i: i32 }
/// pub struct Bar<'a> { foo: &'a Foo }
/// pub struct Qux<'a: 'b, 'b> { bar: &'b Bar<'a> }
/// 
/// rental! {
///     mod my_rentals {
///         use super::*;
///
///         #[rental]
///         pub struct MyRental {
///             foo: Box<Foo>,
///             bar: Box<Bar<'foo>>,
///             qux: Qux<'foo, 'bar>,
///         }
///     }
/// }
/// # fn main () { }
/// ```
/// 
/// Here we see each field use the special lifetimes of the previous fields to establish the borrowing chain.
/// 
/// In addition to the rental struct itself, two other structs are generated, with `_Borrow` and `_BorrowMut` appended to the original struct name (e.g. `MyRental_Borrow` and `MyRental_BorrowMut`). These structs contain the same fields as the original struct, but are borrows of the originals. These structs are passed into certain closures that you provide to the [`rent_all`](examples/struct.RentRef.html#method.rent_all) suite of methods to allow you access to the struct's fields. For mutable rentals, these structs will only contain a borrow of the suffix; the other fields will be erased with `PhantomData`.
/// 
/// If all the fields of your struct implement `Debug` then you can use the `debug_borrow` option on the rental attribute to gain a `Debug` impl on the struct itself. Also, if the suffix field of the struct implements `Deref` or `DerefMut`, you can add a `deref_suffix` or `deref_mut_suffix` argument to the `rental` attribute on the struct. This will generate a `Deref` implementation for the rental struct itself that will deref through the suffix and return the borrow to you, for convenience. Note, however, that this will only be legal if none of the special rental lifetimes appear in the type signature of the deref target. If they do, exposing them to the outside world could result in unsafety, so this is not allowed and such a scenario will not compile.
/// 
/// Finally, there is one other capability to discuss. If a rental struct has been defined elsewhere, either in our own crate or in a dependency, we'd like to be able to chain our own rental struct off of it. In this way, we can use another rental struct as a sort of pre-packaged prefix of our own. As a variation on the above example, it would look like this:
///
/// ```rust
/// # #[macro_use] extern crate rental;
/// pub struct Foo { i: i32 }
/// pub struct Bar<'a> { foo: &'a Foo }
/// pub struct Qux<'a: 'b, 'b> { bar: &'b Bar<'a> }
/// 
/// rental! {
///     mod my_rentals {
///         use super::*;
///
///         #[rental]
///         pub struct OtherRental {
///             foo: Box<Foo>,
///             bar: Bar<'foo>,
///         }
///         
///         #[rental]
///         pub struct MyRental {
///             #[subrental = 2]
///             prefix: Box<OtherRental>,
///             qux: Qux<'prefix_0, 'prefix_1>,
///         }
///     }
/// }
/// # fn main () { }
/// ```
/// 
/// The first rental struct is fairly standard, so we'll focus on the second one. The head field is given a `subrental` attribute with an `arity` argument. The arity of a rental struct is the number of special lifetimes it creates. As can be seen above, the first struct has two fields, neither of which is itself a subrental, so it has an arity of 2. The arity of the second struct would be 3, since it includes the two fields of the first rental as well as one new one. In this way, arity is transitive. So if we used our new struct itself as a subrental of yet another struct, we'd need to declare the field with `arity = 3`. The special lifetimes created by a subrental are the field name followed by a `_` and a zero-based index.
/// 
/// This covers the essential capabilities of the macro itself. For details on the API of the structs themselves, see the [`examples`](examples/index.html) module.
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


#[cfg(feature = "std")]
rental! {
	/// Example instantiations that demonstrate the API provided by the types this crate generates.
	pub mod examples {
		/// A simple example that can store an owner and a shared reference in the same struct.
		///
		/// ```rust
		/// # extern crate rental;
		/// # use rental::examples::RentRef;
		/// # fn main() {
		/// let r = RentRef::new(Box::new(5), |i| &*i);
		/// assert_eq!(*r, RentRef::rent(&r, |iref| **iref));
		/// # }
		/// ```
		#[rental(deref_suffix)]
		pub struct RentRef<T: 'static, U: 'static> {
			head: Box<T>,
			suffix: &'head U,
		}

		/// A simple example that can store an owner and a mutable reference in the same struct.
		///
		/// ```rust
		/// # extern crate rental;
		/// # use rental::examples::RentMut;
		/// # fn main() {
		/// let mut r = RentMut::new(Box::new(5), |i| &mut *i);
		/// *r = 12;
		/// assert_eq!(12, RentMut::rent(&mut r, |iref| **iref));
		/// # }
		/// ```
		#[rental_mut(deref_mut_suffix)]
		pub struct RentMut<T: 'static, U: 'static> {
			head: Box<T>,
			suffix: &'head mut U,
		}

		/// An example that borrows off of another rental struct.
		///
		/// ```rust
		/// # extern crate rental;
		/// # use rental::examples::{RentRef, RentRefMap};
		/// # fn main() {
		/// let r = RentRef::new(Box::new(5), |i| &*i);
		/// let rm = RentRefMap::new(Box::new(r), |r| &**r.suffix);
		/// assert_eq!(*rm, RentRefMap::rent(&rm, |iref| **iref));
		/// # }
		/// ```
		#[rental(deref_suffix)]
		pub struct RentRefMap<T: 'static, U: 'static, V: 'static> {
			#[subrental = 2]
			head: Box<RentRef<T, U>>,
			suffix: &'head_1 V,
		}

		/// An example that borrows mutably off of another rental struct.
		///
		/// ```rust
		/// # extern crate rental;
		/// # use rental::examples::{RentMut, RentMutMap};
		/// # fn main() {
		/// let mut r = RentMut::new(Box::new(5), |i| &mut *i);
		/// let mut rm = RentMutMap::new(Box::new(r), |r| &mut **r.suffix);
		/// *rm = 12;
		/// assert_eq!(12, RentMutMap::rent(&mut rm, |iref| **iref));
		/// # }
		/// ```
		#[rental_mut(deref_mut_suffix)]
		pub struct RentMutMap<T: 'static, U: 'static, V: 'static> {
			#[subrental = 2]
			head: Box<RentMut<T, U>>,
			suffix: &'head_1 mut V,
		}
	}
}
