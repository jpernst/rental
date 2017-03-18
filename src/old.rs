#![cfg_attr(not(feature = "std"), no_std)]

//! A macro to generate self-borrowing structs, plus a few predefined type
//! aliases for convenience.
//!
//! # Overview
//!
//! It can sometimes occur in the course of designing an API that you find
//! yourself in a situation where you need to store, in a single struct, both
//! an owned value and a borrow of that value. Rust's concept of ownership and
//! borrowing is quite flexible, but can't quite express such a scenario.
//!
//! One example might be `libloading`. That crate provides a `Library` struct
//! that defines methods to borrow `Symbol`s from it. These symbols are bounded
//! by the lifetime of the library, and are thus considered a borrow. Under
//! normal circumstances, one would be unable to store both the library and the
//! symbols within a single struct, but the macro defined in this crate allows
//! you to define a struct that is capable of storing both simultaneously.
//!
//! This crate uses the term "rental" to describe this concept of a borrow that
//! co-exsists with its owner in the same struct. The borrow itself is called
//! the "rented" type. The "owner", naturally, is the item in the struct that
//! owns the borrow.
//!
//! The API consists of the [`rental`](macro.rental.html) macro, which generates
//! rental structs, and a few premade instantiations of this macro handling
//! rented bare references.  If you only need to rent references, see
//! [`RentRef`](struct.RentRef.html) and [`RentMut`](struct.RentMut.html), as
//! well as the related type aliases for common rental scenarios. The
//! documentation for the [`rental`](macro.rental.html) macro describes the
//! kinds of items that can be generated.

#[cfg(feature = "std")]
extern crate core;


#[doc(hidden)]
pub mod c {
	pub use ::core::*;
}


extern crate stable_deref_trait;
pub use stable_deref_trait::StableDeref as FixedDeref;


/// This macro is the bedrock of the API. It allows you to define three
/// different kinds of items related to renting.
///
/// NOTE: This macro is only necessary to use if you want to define new rental
/// types that can store customized forms of borrow other than a bare reference.
/// For bare references, this crate provides premade types, called
/// [`RentRef`](struct.RentRef.html) and [`RentMut`](struct.RentMut.html), that
/// accomplish this. Several type aliases are also provided for common
/// scenarios.
///
/// The top level item in an invocation of this macro must be a module. This
/// module can have any name you like, may be public or not, and will hold all
/// declared items. Standard `use` statements are also accepted inside the module.
///
/// # Rentals
///
/// The first two items this macro can produce are shared and mutable rentals.
/// Both forms are tuple-structs that contain an (owner, borrow) pair. Both
/// have a similar API, with minor differences pointed out below. They are
/// declared as follows:
///
/// ```rust,ignore
/// // Shared
/// pub rental $rental:ident<'rental $(, $param:tt $(: [$($bounds:tt)*])*)*> (
/// 	$owner_ty:ty,
/// 	$($rental_ty:tt)*
/// )$(: Deref($($deref_ty:tt)*))* where [$($clause:tt)*];
///
/// // Mutable
/// pub rental mut $rental:ident<'rental $(, $param:tt $(: [$($bounds:tt)*])*)*> (
/// 	$owner_ty:ty,
/// 	$($rental_ty:tt)*
/// )$(: Deref($($deref_ty:tt)*))* where [$($clause:tt)*];
/// ```
///
/// The key difference being the presence or absence of `mut` before the struct
/// name. The first generic parameter must be a lifetime called `'rental`. This
/// lifetime is special and represents the lifetime of the rental struct
/// itself. It should be used anywhere in the rented or owner type signatures
/// that represents their mutual link with the rental struct itself. For
/// example, the definition of [`RentRef`](struct.RentRef.html) looks like this:
///
/// ```rust
/// # #[macro_use] extern crate rental;
/// # fn main() { }
/// # rental!{ mod example {
/// # use rental::FixedDeref;
/// pub rental RentRef<'rental, T: [FixedDeref + 'rental], B: [?Sized + 'rental]> (T, &'rental B): Deref(B);
/// # }}
/// ```
///
/// The lifetime `'rental` appears as the bound on the reference, because that
/// is the lifetime that will be attached when you create the reference by
/// borrowing from the owner object inside the creation closure. Note that it
/// also appears as a bound on the other generic types, since they must also
/// naturally live at least as long as the struct does. For a type to be
/// eligible as an owner, it must also implement the
/// [`FixedDeref`](trait.FixedDeref.html) trait. Finally, after the
/// declaration, you may optionally add `: Deref(Target)` where `Target` is the
/// `Deref` target of the rented type. This can't be inferred, because it must
/// be checked to ensure that it does NOT contain the `'rental` lifetime
/// anywhere in its signature. If it does, then implementing `Deref` would be
/// unsafe, and the macro will reject it.
///
/// It should also be noted that trait bounds and where clauses must be
/// enclosed in square brackets, e.g. `T: [MyTrait]` or `where [T: MyTrait]`.
/// This is a consequence of how macros are parsed and is not avoidable without
/// significant complexity.
///
/// For an overview of the API provided by rental structs, see the
/// documentation for the built-in [`RentRef`](struct.RentRef.html) and
/// [`RentMut`](struct.RentMut.html) structs.
///
/// # Mapping
///
/// At times you may wish to transform one rental into another. This can be
/// accomplished with "mappers", which are the third kind of item that this
/// macro allows you to define. They are declared thusly:
///
/// ```rust,ignore
/// pub mapper $mapper:ident<'rental $(, $param:tt)*>($($from_ty:tt)*) -> ($($into_ty:tt)*) where [$($clause:tt)*];
/// ```
///
/// As with rental structs, the special `'rental` lifetime makes an appearance
/// here and has the same meaning. An example of the built-in
/// [`MapRef`](struct.MapRef.html) that maps from one reference type to another
/// is:
///
/// ```rust
/// # #[macro_use] extern crate rental;
/// # fn main() { }
/// # rental!{ mod example {
/// pub mapper MapRef<'rental, T, U>(&'rental T) -> (&'rental U) where [T: 'rental, U: 'rental];
/// # }}
/// ```
///
/// A mapper may be used to transform between any rental structs provided they
/// have identical owner types. For details see the documentation for the
/// built-in [`MapRef`](struct.MapRef.html) and [`MapMut`](struct.MapMut.html)
/// structs.
#[macro_export]
macro_rules! rental {
	{
		mod $rental_mod:ident {
			$($items:tt)*
		}
	} => {
		mod $rental_mod {
			rental!{@ITEM $($items)*}
		}
	};
	{
		pub mod $rental_mod:ident {
			$($items:tt)*
		}
	} => {
		pub mod $rental_mod {
			rental!{@ITEM $($items)*}
		}
	};


	{
		@ITEM use $($rest:tt)*
	} => {
		rental!(@USES use $($rest)*);
	};
	{
		@USES $uses:item $($rest:tt)*
	} => {
		$uses
		rental!(@ITEM $($rest)*);
	};


	{
		@ITEM pub rental $rental:ident<'rental $(, $param:tt $(: [$($bounds:tt)*])*)*> (
			$owner_ty:ty,
			$($rental_ty:tt)*
		) where [$($clause:tt)*];
		$($rest:tt)*
	} => {
		/// A struct representing a particular (owner, rental) pair.
		///
		/// All such structs share a common API, but it is merely a convention
		/// enforced by the [`rental`](macro.rental.html) macro.  The
		/// capabilities of a rental struct cannot be fully described as a trait
		/// without language support for HKT (Higher-Kinded Types). What methods
		/// can be expressed in a trait are documented in the
		/// [`Rental`](trait.Rental.html) and
		/// [`RentalMut`](trait.RentalMut.html) traits.
		///
		/// A rental struct can implement `Deref`, but only if the rented type
		/// is `Deref` and its target does not contain the `'rental` lifetime
		/// in its signature.
		#[deny(lifetime_underscore)]
		pub struct $rental<'rental $(, $param $(: $($bounds)*)*)*> where
			'static: 'rental,
			$($clause)*
		{
			owner: $crate::c::option::Option<$owner_ty>,
			rental: $crate::c::option::Option<$($rental_ty)*>,
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			/// Instantiate a new shared rental pair. `owner` is the object
			/// from which a value will be rented. The closure takes a
			/// reborrowed shared reference to the owner's `Deref` target, and
			/// returns the rented value. Within this closure, the special
			/// `'rental` lifetime is "existential" and cannot be unified with
			/// any external lifetime. This is crucial in that it prevents any
			/// borrow of the owner from escaping from the closure other than as
			/// a return value.
			#[allow(dead_code)]
			pub fn new<F__>(owner: $owner_ty, f: F__)
				-> $rental<'rental $(, $param)*> where
				F__: for<'a__> $crate::c::ops::FnOnce(&'a__ <$owner_ty as $crate::c::ops::Deref>::Target) -> rental_rebind__!('a__; $($rental_ty)*)
			{
				$crate::static_assert_fixed_deref__::<$owner_ty>();
				$rental{
					rental: unsafe {
						let ptr: *const _ = <$owner_ty as $crate::c::ops::Deref>::deref(&owner);
						Some(f(&*ptr))
					},
					owner: Some(owner),
				}
			}


			/// As [`new`](#method.new), but the closure returns a `Result`
			/// with a generic error. In the event of such failure, this method
			/// will return `Err` with a tuple of the error itself and the
			/// owner object.
			#[allow(dead_code)]
			pub fn try_new<E__, F__>(owner: $owner_ty, f: F__)
				-> $crate::c::result::Result<$rental<'rental $(, $param)*>, (E__, $owner_ty)> where
				F__: for<'a__> $crate::c::ops::FnOnce(&'a__ <$owner_ty as $crate::c::ops::Deref>::Target) -> $crate::c::result::Result<rental_rebind__!('a__; $($rental_ty)*), E__>
			{
				$crate::static_assert_fixed_deref__::<$owner_ty>();
				Ok($rental{
					rental: unsafe {
						let ptr: *const _ = <$owner_ty as $crate::c::ops::Deref>::deref(&owner);
						match f(&*ptr) {
							Ok(asset) => Some(asset),
							Err(err) => return Err((err, owner)),
						}
					},
					owner: Some(owner),
				})
			}


			/// Borrows the owner.
			#[allow(dead_code)]
			pub fn owner(&self) -> &$owner_ty {
				self.owner.as_ref().unwrap()
			}


			/// Executes a closure on the existentially borrowed rental. The
			/// closure may return any value that does not include the `'rental`
			/// lifetime in its type signature.
			#[allow(dead_code)]
			pub fn rent<F__, R__>(&self, f: F__) -> R__ where
				F__: for<'a__, 'r__> $crate::c::ops::FnOnce(&'a__ rental_rebind__!('r__; $($rental_ty)*)) -> R__,
			{
				f(self.rental.as_ref().unwrap())
			}


			/// As [`rent`](#method.rent) but the return value of the closure
			/// is a reference of `'rental` lifetime. This is safe because the
			/// reference is reborrowed before being returned to you.
			#[allow(dead_code)]
			pub fn rent_ref<F__, R__>(&self, f: F__) -> &R__ where
				F__: for<'a__, 'r__> $crate::c::ops::FnOnce(&'a__ rental_rebind__!('r__; $($rental_ty)*)) -> &'a__ R__,
			{
				f(self.rental.as_ref().unwrap())
			}
		}


		unsafe impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::Rental for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			type Owner = $owner_ty;
			type Rental = $($rental_ty)*;


			#[inline]
			unsafe fn rental(&self) -> &$($rental_ty)* { self.rental.as_ref().unwrap() }
			fn from_parts(owner: $owner_ty, rent: $($rental_ty)*) -> Self { $rental{owner: Some(owner), rental: Some(rent)} }
			unsafe fn into_parts(mut self) -> ($owner_ty, $($rental_ty)*) { (self.owner.take().unwrap(), self.rental.take().unwrap()) }
			fn into_owner(mut self) -> $owner_ty { self.owner.take().unwrap() }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::c::ops::Drop for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			fn drop(&mut self) {
				$crate::c::mem::drop(self.rental.take());
				$crate::c::mem::drop(self.owner.take());
			}
		}


		rental!{@ITEM $($rest)*}
	};


	{
		@ITEM pub rental $rental:ident<'rental $(, $param:tt $(: [$($bounds:tt)*])*)*> (
			$owner_ty:ty,
			$($rental_ty:tt)*
		): Deref($($deref_ty:tt)*) where [$($clause:tt)*];
		$($rest:tt)*
	} => {
		/// A struct representing a particular (owner, rental) pair.
		///
		/// All such structs share a common API, but it is merely a convention
		/// enforced by the [`rental`](macro.rental.html) macro.  The
		/// capabilities of a rental struct cannot be fully described as a trait
		/// without language support for HKT (Higher-Kinded Types). What methods
		/// can be expressed in a trait are documented in the
		/// [`Rental`](trait.Rental.html) and
		/// [`RentalMut`](trait.RentalMut.html) traits.
		///
		/// A rental struct can implement `Deref`, but only if the rented type
		/// is `Deref` and its target does not contain the `'rental` lifetime
		/// in its signature.
		#[deny(lifetime_underscore)]
		pub struct $rental<'rental $(, $param $(: $($bounds)*)*)*> where
			'static: 'rental,
			$($clause)*
		{
			owner: $crate::c::option::Option<$owner_ty>,
			rental: $crate::c::option::Option<$($rental_ty)*>,
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			/// Instantiate a new shared rental pair. `owner` is the object
			/// from which a value will be rented. The closure takes a
			/// reborrowed shared reference to the owner's `Deref` target, and
			/// returns the rented value. Within this closure, the special
			/// `'rental` lifetime is "existential" and cannot be unified with
			/// any external lifetime. This is crucial in that it prevents any
			/// borrow of the owner from escaping from the closure other than as
			/// a return value.
			#[allow(dead_code)]
			pub fn new<F__>(owner: $owner_ty, f: F__)
				-> $rental<'rental $(, $param)*> where
				F__: for<'a__> $crate::c::ops::FnOnce(&'a__ <$owner_ty as $crate::c::ops::Deref>::Target) -> rental_rebind__!('a__; $($rental_ty)*)
			{
				$crate::static_assert_fixed_deref__::<$owner_ty>();
				$rental{
					rental: unsafe {
						let ptr: *const _ = <$owner_ty as $crate::c::ops::Deref>::deref(&owner);
						Some(f(&*ptr))
					},
					owner: Some(owner),
				}
			}


			/// As [`new`](#method.new), but the closure returns a `Result`
			/// with a generic error. In the event of such failure, this method
			/// will return `Err` with a tuple of the error itself and the
			/// owner object.
			#[allow(dead_code)]
			pub fn try_new<E__, F__>(owner: $owner_ty, f: F__)
				-> $crate::c::result::Result<$rental<'rental $(, $param)*>, (E__, $owner_ty)> where
				F__: for<'a__> $crate::c::ops::FnOnce(&'a__ <$owner_ty as $crate::c::ops::Deref>::Target) -> $crate::c::result::Result<rental_rebind__!('a__; $($rental_ty)*), E__>
			{
				$crate::static_assert_fixed_deref__::<$owner_ty>();
				Ok($rental{
					rental: unsafe {
						let ptr: *const _ = <$owner_ty as $crate::c::ops::Deref>::deref(&owner);
						match f(&*ptr) {
							Ok(asset) => Some(asset),
							Err(err) => return Err((err, owner)),
						}
					},
					owner: Some(owner),
				})
			}


			/// Borrows the owner.
			#[allow(dead_code)]
			pub fn owner(&self) -> &$owner_ty {
				self.owner.as_ref().unwrap()
			}


			/// Executes a closure on the existentially borrowed rental. The
			/// closure may return any value that does not include the `'rental`
			/// lifetime in its type signature.
			#[allow(dead_code)]
			pub fn rent<F__, R__>(&self, f: F__) -> R__ where
				F__: for<'a__, 'r__> $crate::c::ops::FnOnce(&'a__ rental_rebind__!('r__; $($rental_ty)*)) -> R__,
			{
				f(self.rental.as_ref().unwrap())
			}


			/// As [`rent`](#method.rent) but the return value of the closure
			/// is a reference of `'rental` lifetime. This is safe because the
			/// reference is reborrowed before being returned to you.
			#[allow(dead_code)]
			pub fn rent_ref<F__, R__>(&self, f: F__) -> &R__ where
				F__: for<'a__, 'r__> $crate::c::ops::FnOnce(&'a__ rental_rebind__!('r__; $($rental_ty)*)) -> &'a__ R__,
			{
				f(self.rental.as_ref().unwrap())
			}
		}


		unsafe impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::Rental for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			type Owner = $owner_ty;
			type Rental = $($rental_ty)*;


			#[inline]
			unsafe fn rental(&self) -> &$($rental_ty)* { self.rental.as_ref().unwrap() }
			fn from_parts(owner: $owner_ty, rent: $($rental_ty)*) -> Self { $rental{owner: Some(owner), rental: Some(rent)} }
			unsafe fn into_parts(mut self) -> ($owner_ty, $($rental_ty)*) { (self.owner.take().unwrap(), self.rental.take().unwrap()) }
			fn into_owner(mut self) -> $owner_ty { self.owner.take().unwrap() }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::c::ops::Deref for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			type Target = rental_deref_ty__!($($deref_ty)*);

			#[inline]
			fn deref(&self) -> &rental_deref_ty__!($($deref_ty)*) {
				use $crate::Rental;
				unsafe { &**self.rental() }
			}
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::c::convert::AsRef<rental_deref_ty__!($($deref_ty)*)> for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			fn as_ref(&self) -> &rental_deref_ty__!($($deref_ty)*) { &**self }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::c::borrow::Borrow<rental_deref_ty__!($($deref_ty)*)> for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			fn borrow(&self) -> &rental_deref_ty__!($($deref_ty)*) { &**self }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::c::ops::Drop for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			fn drop(&mut self) {
				$crate::c::mem::drop(self.rental.take());
				$crate::c::mem::drop(self.owner.take());
			}
		}


		rental!{@ITEM $($rest)*}
	};


	{
		@ITEM pub rental mut $rental:ident<'rental $(, $param:tt $(: [$($bounds:tt)*])*)*> (
			$owner_ty:ty,
			$($rental_ty:tt)*
		) where [$($clause:tt)*];
		$($rest:tt)*
	} => {
		/// A struct representing a particular mutable (owner, rental) pair.
		///
		/// All such structs share a common API, but it is merely a convention
		/// enforced by the [`rental`](macro.rental.html) macro.  The
		/// capabilities of a rental struct cannot be fully described as a trait
		/// without language support for HKT (Higher-Kinded Types). What methods
		/// can be expressed in a trait are documented in the
		/// [`Rental`](trait.Rental.html) and
		/// [`RentalMut`](trait.RentalMut.html) traits.
		///
		/// A rental struct can implement `Deref` and `DerefMut`, but only if
		/// the rented type is `Deref`/`DerefMut` and its target does not
		/// contain the `'rental` lifetime in its signature.
		#[deny(lifetime_underscore)]
		pub struct $rental<'rental $(, $param $(: $($bounds)*)*)*> where
			'static: 'rental,
			$($clause)*
		{
			owner: $crate::c::option::Option<$owner_ty>,
			rental: $crate::c::option::Option<$($rental_ty)*>,
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			/// Instantiate a new mutable rental pair. `owner` is the object
			/// from which a value will be rented. The closure takes a
			/// reborrowed mutable reference to the owner's `Deref` target, and
			/// returns the rented value. Within this closure, the special
			/// `'rental` lifetime is "existential" and cannot be unified with
			/// any external lifetime. This is crucial in that it prevents any
			/// borrow of the owner from escaping from the closure other than as
			/// a return value.
			#[allow(dead_code)]
			pub fn new<F__>(mut owner: $owner_ty, f: F__)
				-> $rental<'rental $(, $param)*> where
				F__: for<'a__> $crate::c::ops::FnOnce(&'a__ mut <$owner_ty as $crate::c::ops::Deref>::Target) -> rental_rebind__!('a__; $($rental_ty)*)
			{
				$crate::static_assert_fixed_deref__::<$owner_ty>();
				$rental{
					rental: unsafe {
						let ptr: *mut _ = <$owner_ty as $crate::c::ops::DerefMut>::deref_mut(&mut owner);
						Some(f(&mut *ptr))
					},
					owner: Some(owner),
				}
			}


			/// As [`new`](#method.new), but the closure returns a `Result`
			/// with a generic error. In the event of such failure, this method
			/// will return `Err` with a tuple of the error itself and the
			/// owner object.
			#[allow(dead_code)]
			pub fn try_new<E__, F__>(mut owner: $owner_ty, f: F__)
				-> $crate::c::result::Result<$rental<'rental $(, $param)*>, (E__, $owner_ty)> where
				F__: for<'a__> $crate::c::ops::FnOnce(&'a__ mut <$owner_ty as $crate::c::ops::Deref>::Target) -> $crate::c::result::Result<rental_rebind__!('a__; $($rental_ty)*), E__>
			{
				$crate::static_assert_fixed_deref__::<$owner_ty>();
				Ok($rental{
					rental: unsafe {
						let ptr: *mut _ = &mut *<$owner_ty as $crate::c::ops::DerefMut>::deref_mut(&mut owner);
						match f(&mut *ptr) {
							Ok(asset) => Some(asset),
							Err(err) => return Err((err, owner)),
						}
					},
					owner: Some(owner),
				})
			}


			/// Executes a closure on the existentially borrowed rental. The
			/// closure may return any value that does not include the `'rental`
			/// lifetime in its type signature.
			#[allow(dead_code)]
			pub fn rent<F__, R__>(&self, f: F__) -> R__ where
				F__: for<'a__, 'r__> $crate::c::ops::FnOnce(&'a__ rental_rebind__!('r__; $($rental_ty)*)) -> R__,
			{
				f(self.rental.as_ref().unwrap())
			}


			/// As [`rent`](#method.rent) but the rental is mutable.
			#[allow(dead_code)]
			pub fn rent_mut<F__, R__>(&mut self, f: F__) -> R__ where
				F__: for<'a__, 'r__> $crate::c::ops::FnOnce(&'a__ mut rental_rebind__!('r__; $($rental_ty)*)) -> R__,
			{
				f(self.rental.as_mut().unwrap())
			}


			/// As [`rent`](#method.rent) but the return value of the closure
			/// is a reference of `'rental` lifetime. This is safe because the
			/// reference is reborrowed before being returned to you.
			#[allow(dead_code)]
			pub fn rent_ref<F__, R__>(&self, f: F__) -> &R__ where
				F__: for<'a__, 'r__> $crate::c::ops::FnOnce(&'a__ rental_rebind__!('r__; $($rental_ty)*)) -> &'a__ R__,
			{
				f(self.rental.as_ref().unwrap())
			}


			/// As [`rent_ref`](#method.rent_ref) but the returned reference is
			/// mutable.
			#[allow(dead_code)]
			pub fn rent_mut_ref<F__, R__>(&mut self, f: F__) -> &mut R__ where
				F__: for<'a__, 'r__> $crate::c::ops::FnOnce(&'a__ mut rental_rebind__!('r__; $($rental_ty)*)) -> &'a__ mut R__,
			{
				f(self.rental.as_mut().unwrap())
			}
		}


		unsafe impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::Rental for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			type Owner = $owner_ty;
			type Rental = $($rental_ty)*;


			#[inline]
			unsafe fn rental(&self) -> &$($rental_ty)* { self.rental.as_ref().unwrap() }
			fn from_parts(owner: $owner_ty, rent: $($rental_ty)*) -> Self { $rental{owner: Some(owner), rental: Some(rent)} }
			unsafe fn into_parts(mut self) -> ($owner_ty, $($rental_ty)*) { (self.owner.take().unwrap(), self.rental.take().unwrap()) }
			fn into_owner(mut self) -> $owner_ty { self.owner.take().unwrap() }
		}


		unsafe impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::RentalMut for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			#[inline]
			unsafe fn rental_mut(&mut self) -> &mut <Self as $crate::Rental>::Rental { self.rental.as_mut().unwrap() }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::c::ops::Drop for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			fn drop(&mut self) {
				$crate::c::mem::drop(self.rental.take());
				$crate::c::mem::drop(self.owner.take());
			}
		}


		rental!{@ITEM $($rest)*}
	};


	{
		@ITEM pub rental mut $rental:ident<'rental $(, $param:tt $(: [$($bounds:tt)*])*)*> (
			$owner_ty:ty,
			$($rental_ty:tt)*
		): Deref($($deref_ty:tt)*) where [$($clause:tt)*];
		$($rest:tt)*
	} => {
		/// A struct representing a particular mutable (owner, rental) pair.
		///
		/// All such structs share a common API, but it is merely a convention
		/// enforced by the [`rental`](macro.rental.html) macro.  The
		/// capabilities of a rental struct cannot be fully described as a trait
		/// without language support for HKT (Higher-Kinded Types). What methods
		/// can be expressed in a trait are documented in the
		/// [`Rental`](trait.Rental.html) and
		/// [`RentalMut`](trait.RentalMut.html) traits.
		///
		/// A rental struct can implement `Deref` and `DerefMut`, but only if
		/// the rented type is `Deref`/`DerefMut` and its target does not
		/// contain the `'rental` lifetime in its signature.
		#[deny(lifetime_underscore)]
		pub struct $rental<'rental $(, $param $(: $($bounds)*)*)*> where
			'static: 'rental,
			$($clause)*
		{
			owner: $crate::c::option::Option<$owner_ty>,
			rental: $crate::c::option::Option<$($rental_ty)*>,
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			/// Instantiate a new mutable rental pair. `owner` is the object
			/// from which a value will be rented. The closure takes a
			/// reborrowed mutable reference to the owner's `Deref` target, and
			/// returns the rented value. Within this closure, the special
			/// `'rental` lifetime is "existential" and cannot be unified with
			/// any external lifetime. This is crucial in that it prevents any
			/// borrow of the owner from escaping from the closure other than as
			/// a return value.
			#[allow(dead_code)]
			pub fn new<F__>(mut owner: $owner_ty, f: F__)
				-> $rental<'rental $(, $param)*> where
				F__: for<'a__> $crate::c::ops::FnOnce(&'a__ mut <$owner_ty as $crate::c::ops::Deref>::Target) -> rental_rebind__!('a__; $($rental_ty)*)
			{
				$crate::static_assert_fixed_deref__::<$owner_ty>();
				$rental{
					rental: unsafe {
						let ptr: *mut _ = &mut *<$owner_ty as $crate::c::ops::DerefMut>::deref_mut(&mut owner);
						Some(f(&mut *ptr))
					},
					owner: Some(owner),
				}
			}


			/// As [`new`](#method.new), but the closure returns a `Result`
			/// with a generic error. In the event of such failure, this method
			/// will return `Err` with a tuple of the error itself and the
			/// owner object.
			#[allow(dead_code)]
			pub fn try_new<E__, F__>(mut owner: $owner_ty, f: F__)
				-> $crate::c::result::Result<$rental<'rental $(, $param)*>, (E__, $owner_ty)> where
				F__: for<'a__> $crate::c::ops::FnOnce(&'a__ mut <$owner_ty as $crate::c::ops::Deref>::Target) -> $crate::c::result::Result<rental_rebind__!('a__; $($rental_ty)*), E__>
			{
				$crate::static_assert_fixed_deref__::<$owner_ty>();
				Ok($rental{
					rental: unsafe {
						let ptr: *mut _ = &mut *<$owner_ty as $crate::c::ops::DerefMut>::deref_mut(&mut owner);
						match f(&mut *ptr) {
							Ok(asset) => Some(asset),
							Err(err) => return Err((err, owner)),
						}
					},
					owner: Some(owner),
				})
			}


			/// Executes a closure on the existentially borrowed rental. The
			/// closure may return any value that does not include the `'rental`
			/// lifetime in its type signature.
			#[allow(dead_code)]
			pub fn rent<F__, R__>(&self, f: F__) -> R__ where
				F__: for<'a__, 'r__> $crate::c::ops::FnOnce(&'a__ rental_rebind__!('r__; $($rental_ty)*)) -> R__,
			{
				f(self.rental.as_ref().unwrap())
			}


			/// As [`rent`](#method.rent) but the rental is mutable.
			#[allow(dead_code)]
			pub fn rent_mut<F__, R__>(&mut self, f: F__) -> R__ where
				F__: for<'a__, 'r__> $crate::c::ops::FnOnce(&'a__ mut rental_rebind__!('r__; $($rental_ty)*)) -> R__,
			{
				f(self.rental.as_mut().unwrap())
			}


			/// As [`rent`](#method.rent) but the return value of the closure
			/// is a reference of `'rental` lifetime. This is safe because the
			/// reference is reborrowed before being returned to you.
			#[allow(dead_code)]
			pub fn rent_ref<F__, R__>(&self, f: F__) -> &R__ where
				F__: for<'a__, 'r__> $crate::c::ops::FnOnce(&'a__ rental_rebind__!('r__; $($rental_ty)*)) -> &'a__ R__,
			{
				f(self.rental.as_ref().unwrap())
			}


			/// As [`rent_ref`](#method.rent_ref) but the returned reference is
			/// mutable.
			#[allow(dead_code)]
			pub fn rent_mut_ref<F__, R__>(&mut self, f: F__) -> &mut R__ where
				F__: for<'a__, 'r__> $crate::c::ops::FnOnce(&'a__ mut rental_rebind__!('r__; $($rental_ty)*)) -> &'a__ mut R__,
			{
				f(self.rental.as_mut().unwrap())
			}
		}


		unsafe impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::Rental for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			type Owner = $owner_ty;
			type Rental = $($rental_ty)*;


			#[inline]
			unsafe fn rental(&self) -> &$($rental_ty)* { self.rental.as_ref().unwrap() }
			fn from_parts(owner: $owner_ty, rent: $($rental_ty)*) -> Self { $rental{owner: Some(owner), rental: Some(rent)} }
			unsafe fn into_parts(mut self) -> ($owner_ty, $($rental_ty)*) { (self.owner.take().unwrap(), self.rental.take().unwrap()) }
			fn into_owner(mut self) -> $owner_ty { self.owner.take().unwrap() }
		}


		unsafe impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::RentalMut for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			#[inline]
			unsafe fn rental_mut(&mut self) -> &mut <Self as $crate::Rental>::Rental { self.rental.as_mut().unwrap() }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::c::ops::Deref for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			type Target = rental_deref_ty__!($($deref_ty)*);

			#[inline]
			fn deref(&self) -> &rental_deref_ty__!($($deref_ty)*) {
				use $crate::Rental;
				unsafe { &**self.rental() }
			}
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::c::ops::DerefMut for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			#[inline]
			fn deref_mut(&mut self) -> &mut <<$rental<'rental $(, $param)*> as $crate::Rental>::Rental as $crate::c::ops::Deref>::Target {
				use $crate::RentalMut;
				unsafe { <<$rental<'rental $(, $param)*> as $crate::Rental>::Rental as $crate::c::ops::DerefMut>::deref_mut(self.rental_mut()) }
			}
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::c::convert::AsRef<rental_deref_ty__!($($deref_ty)*)> for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			fn as_ref(&self) -> &rental_deref_ty__!($($deref_ty)*) { &**self }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::c::convert::AsMut<rental_deref_ty__!($($deref_ty)*)> for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			fn as_mut(&mut self) -> &mut rental_deref_ty__!($($deref_ty)*) { &mut **self }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::c::borrow::Borrow<rental_deref_ty__!($($deref_ty)*)> for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			fn borrow(&self) -> &rental_deref_ty__!($($deref_ty)*) { &**self }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::c::borrow::BorrowMut<rental_deref_ty__!($($deref_ty)*)> for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			fn borrow_mut(&mut self) -> &mut rental_deref_ty__!($($deref_ty)*) { &mut **self }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::c::ops::Drop for $rental<'rental $(, $param)*> where
			'static: 'rental,
			$($clause)*
		{
			fn drop(&mut self) {
				$crate::c::mem::drop(self.rental.take());
				$crate::c::mem::drop(self.owner.take());
			}
		}


		rental!{@ITEM $($rest)*}
	};


	{
		@ITEM pub mapper $mapper:ident<'rental $(, $param:tt $(: [$($bounds:tt)*])*)*>($($from_ty:tt)*) -> ($($into_ty:tt)*) where [$($clause:tt)*];
		$($rest:tt)*
	} => {
		/// A mapper that can convert one rental into another, provided they
		/// have identical owner types.
		#[allow(dead_code)]
		pub struct $mapper;
		impl $mapper {
			/// Maps from one rental to another. The closure assumes ownership
			/// of the rented value and converts it into a new value. A new
			/// rental pair is then returned to you from the original owner and
			/// the new value. As with other rental closures, the `'rental`
			/// lifetime is existential to prevent improper escaping or
			/// replacement.
			#[allow(dead_code)]
			pub fn map<'rental $(, $param $(: $($bounds)*)*)*, T__, U__, F__>(t: T__, f: F__) -> U__ where
				T__: $crate::Rental<Rental=$($from_ty)*>,
				U__: $crate::Rental<Owner=<T__ as $crate::Rental>::Owner, Rental=$($into_ty)*>,
				F__: for<'f__> $crate::c::ops::FnOnce(rental_rebind__!('f__; $($from_ty)*)) -> rental_rebind__!('f__; $($into_ty)*),
				$($clause)*
			{
				unsafe {
					let (o, r) = t.into_parts();
					U__::from_parts(o, f(r))
				}
			}


			/// As [`map`](#method.map), but the closure may fail. In addition
			/// to the error value, the closure must also return the rented
			/// value in the tuple, so that the original rented pair can be
			/// reconstituted and given back to you in the error tuple returned
			/// by this method.
			#[allow(dead_code)]
			pub fn try_map<'rental $(, $param $(: $($bounds)*)*)*, T__, U__, E__, F__>(t: T__, f: F__) -> $crate::c::result::Result<U__, (E__, T__)> where
				T__: $crate::Rental<Rental=$($from_ty)*>,
				U__: $crate::Rental<Owner=<T__ as $crate::Rental>::Owner, Rental=$($into_ty)*>,
				F__: for<'f__> $crate::c::ops::FnOnce(rental_rebind__!('f__; $($from_ty)*)) -> $crate::c::result::Result<rental_rebind__!('f__; $($into_ty)*), (E__, rental_rebind__!('f__; $($from_ty)*))>,
				$($clause)*
			{
				unsafe {
					let (o, r) = t.into_parts();
					match f(r) {
						Ok(r) => Ok(U__::from_parts(o, r)),
						Err((e, r)) => Err((e, T__::from_parts(o, r))),
					}
				}
			}
		}


		rental!{@ITEM $($rest)*}
	};


	{
		@ITEM pub rental mut $rental:ident<'rental $(, $param:tt $(: [$($bounds:tt)*])*)*>($($body:tt)*)$(: Deref($($target_ty:tt)*))*; $($rest:tt)*
	} => {
		rental!{@ITEM pub rental mut $rental<'rental $(, $param $(: [$($bounds)*])*)*>($($body)*)$(: Deref($($target_ty)*))* where []; $($rest)*}
	};
	{
		@ITEM pub rental $rental:ident<'rental $(, $param:tt $(: [$($bounds:tt)*])*)*>($($body:tt)*)$(: Deref($($target_ty:tt)*))*; $($rest:tt)*
	} => {
		rental!{@ITEM pub rental $rental<'rental $(, $param $(: [$($bounds)*])*)*>($($body)*)$(: Deref($($target_ty)*))* where []; $($rest)*}
	};
	{
		@ITEM pub mapper $mapper:ident<'rental $(, $param:tt $(: [$($bounds:tt)*])*)*> ($($from_ty:tt)*) -> ($($into_ty:tt)*); $($rest:tt)*
	} => {
		rental!{@ITEM pub mapper $mapper<'rental $(, $param $(: [$($bounds)*])*)*> ($($from_ty)*) -> ($($into_ty)*) where []; $($rest)*}
	};
	{ @ITEM } => { };
}


#[doc(hidden)]
#[macro_export]
macro_rules! rental_rebind__ {
	(
		$into:tt; {$($head:tt)*} 'rental $($tail:tt)*
	) => {
		rental_rebind__!($into; {$($head)* $into} $($tail)*)
	};
	(
		$into:tt; {$($head:tt)*} ($($inner:tt)*) $($tail:tt)*
	) => {
		rental_rebind__!($into; {($($head)*)} $($inner)* @> $($tail)*)
	};
	(
		$into:tt; {$($head:tt)*} [$($inner:tt)*] $($tail:tt)*
	) => {
		rental_rebind__!($into; {[$($head)*]} $($inner)* @> $($tail)*)
	};
	(
		$into:tt; {$($head:tt)*} {$($inner:tt)*} $($tail:tt)*
	) => {
		rental_rebind__!($into; {{$($head)*}} $($inner)* @> $($tail)*)
	};
	(
		$into:tt; {($($head:tt)*) $($inner:tt)*} @> $($tail:tt)*
	) => {
		rental_rebind__!($into; {$($head)*($($inner)*)} $($tail)*)
	};
	(
		$into:tt; {[$($head:tt)*] $($inner:tt)*} @> $($tail:tt)*
	) => {
		rental_rebind__!($into; {$($head)*[$($inner)*]} $($tail)*)
	};
	(
		$into:tt; {{$($head:tt)*} $($inner:tt)*} @> $($tail:tt)*
	) => {
		rental_rebind__!($into; {$($head)*{$($inner)*}} $($tail)*)
	};
	(
		$into:tt; {$($head:tt)*} $tok:tt $($tail:tt)*
	) => {
		rental_rebind__!($into; {$($head)* $tok} $($tail)*)
	};
	(
		$into:tt; {$($rebound:tt)*}
	) => {
		$($rebound)*
	};
	(
		$into:tt; $($tail:tt)*
	) => {
		rental_rebind__!($into; {} $($tail)*)
	};
}


#[doc(hidden)]
#[macro_export]
macro_rules! rental_deref_ty__ {
	( ) => { $crate::NoDeref };
	( $($deref_ty:tt)+ ) => { rental_rebind__!('_; $($deref_ty)+) };
}


#[cfg(feature = "std")]
use ::std::{cell, sync};


#[doc(hidden)]
#[allow(dead_code)]
#[inline(always)]
pub fn static_assert_fixed_deref__<O: FixedDeref>() { }


/// This trait is implemented for all rental structs.
///
/// It contains within everything that is possible to express without HKT. The
/// most important methods can't be expressed here, but can be seen on the
/// predefined [`RentRef`](struct.RentRef.html) and
/// [`RentMut`](struct.RentMut.html) structs.
pub unsafe trait Rental {
	type Owner: FixedDeref;
	type Rental;

	/// This returns to you the rented value outside of an existential closure.
	/// This is unsafe because the lifetime substituted for `'rental` here is a
	/// lie and does not reflect the true lifetime of the value. Only use this
	/// if you have no alternative, and think very carefully about how you're
	/// using the value to prevent it from outliving the rental struct, or
	/// inserting into it data that will not live long enough.
	unsafe fn rental(&self) -> &<Self as Rental>::Rental;

	/// This will produce a rental struct from component parts. Not unsafe
	/// because, if you already have the components safely, combining them in
	/// this way does not introduce any additional unsafety.
	fn from_parts(<Self as Rental>::Owner, <Self as Rental>::Rental) -> Self;

	/// This will decompose a rental struct into its component parts. This is
	/// obviously unsafe because one may drop the owner while retaining the
	/// borrow.
	unsafe fn into_parts(self) -> (<Self as Rental>::Owner, <Self as Rental>::Rental);

	/// This will consume a rental struct and return to you the owner,
	/// discarding the rented value.
	fn into_owner(self) -> <Self as Rental>::Owner;
}


/// This trait is implemented for all mutable rental structs.
pub unsafe trait RentalMut: Rental {
	/// This returns to you the rented value outside of an existential closure.
	/// This is unsafe because the lifetime substituted for `'rental` here is a
	/// lie and does not reflect the true lifetime of the value. Only use this
	/// if you have no alternative, and think very carefully about how you're
	/// using the value to prevent it from outliving the rental struct, or
	/// inserting into it data that will not live long enough.
	unsafe fn rental_mut(&mut self) -> &mut <Self as Rental>::Rental;
}


#[doc(hidden)]
pub enum NoDeref { }


rental! {
	mod premade {
		use super::FixedDeref;
		use c::ops::DerefMut;

		pub rental RentRef<'rental, T: [FixedDeref + 'rental], B: [?Sized + 'rental]> (T, &'rental B): Deref(B);
		pub rental mut RentMut<'rental, T: [FixedDeref + DerefMut + 'rental], B: [?Sized + 'rental]>(T, &'rental mut B): Deref(B);

		pub mapper MapRef<'rental, T: ['rental], U: [?Sized + 'rental]>(&'rental T) -> (&'rental U);
		pub mapper MapMut<'rental, T: ['rental], U: [?Sized + 'rental]>(&'rental mut T) -> (&'rental mut U);
	}
}

pub use premade::*;


/// A predefined type that rents references from an `Arc<T>`.
/// # Examples
/// ```rust
/// let arc = std::sync::Arc::new(1);
/// let rent = rental::RentArc::new(arc, |a| &*a);
/// assert_eq!(*rent, 1);
/// ```
#[cfg(feature = "std")]
pub type RentArc<'rental, T: 'rental, B: 'rental> = RentRef<'rental, sync::Arc<T>, B>;

/// A predefined type that rents references from a `Box<T>`.
/// # Examples
/// ```rust
/// let bx = Box::new(2);
/// let rent = rental::RentBox::new(bx, |b| &*b);
/// assert_eq!(*rent, 2);
/// ```
#[cfg(feature = "std")]
pub type RentBox<'rental, T: 'rental, B: 'rental> = RentRef<'rental, Box<T>, B>;

/// A predefined type that rents mutable references from a `Box<T>`.
/// # Examples
/// ```rust
/// let bx = Box::new(3);
/// let mut rent = rental::RentBoxMut::new(bx, |b| &mut *b);
/// *rent *= 10;
/// assert_eq!(*rent, 30);
/// ```
#[cfg(feature = "std")]
pub type RentBoxMut<'rental, T: 'rental, B: 'rental> = RentMut<'rental, Box<T>, B>;

/// A predefined type that rents references from a `MutexGuard<T>`.
/// # Examples
/// ```rust
/// let mutex = std::sync::Mutex::new(4);
/// let guard = mutex.lock().unwrap();
/// let rent = rental::RentMutex::new(guard, |g| &*g);
/// assert_eq!(*rent, 4);
/// ```
#[cfg(feature = "std")]
pub type RentMutex<'rental, T: 'rental, B: 'rental> = RentRef<'rental, sync::MutexGuard<'rental, T>, B>;

/// A predefined type that rents mutable references from a `MutexGuard<T>`.
/// # Examples
/// ```rust
/// let mutex = std::sync::Mutex::new(5);
/// let guard = mutex.lock().unwrap();
/// let mut rent = rental::RentMutexMut::new(guard, |g| &mut *g);
/// *rent *= 10;
/// assert_eq!(*rent, 50);
/// ```
#[cfg(feature = "std")]
pub type RentMutexMut<'rental, T: 'rental, B: 'rental> = RentMut<'rental, sync::MutexGuard<'rental, T>, B>;

/// A predefined type that rents references from a `Ref<T>`.
/// # Examples
/// ```rust
/// let cell = std::cell::RefCell::new(6);
/// let r = cell.borrow();
/// let rent = rental::RentRefCell::new(r, |r| &*r);
/// assert_eq!(*rent, 6);
/// ```
#[cfg(feature = "std")]
pub type RentRefCell<'rental, T: 'rental, B: 'rental> = RentRef<'rental, cell::Ref<'rental, T>, B>;

/// A predefined type that rents mutable references from a `RefMut<T>`.
/// # Examples
/// ```rust
/// let cell = std::cell::RefCell::new(7);
/// let r = cell.borrow_mut();
/// let mut rent = rental::RentRefCellMut::new(r, |r| &mut *r);
/// *rent *= 10;
/// assert_eq!(*rent, 70);
/// ```
#[cfg(feature = "std")]
pub type RentRefCellMut<'rental, T: 'rental, B: 'rental> = RentMut<'rental, cell::RefMut<'rental, T>, B>;

/// A predefined type that rents references from an `RwLockReadGuard<T>`.
/// # Examples
/// ```rust
/// let rw = std::sync::RwLock::new(8);
/// let read = rw.read().unwrap();
/// let rent = rental::RentRwLock::new(read, |r| &*r);
/// assert_eq!(*rent, 8);
/// ```
#[cfg(feature = "std")]
pub type RentRwLock<'rental, T: 'rental, B: 'rental> = RentRef<'rental, sync::RwLockReadGuard<'rental, T>, B>;

/// A predefined type that rents mutable references from an `RwLockWriteGuard<T>`.
/// # Examples
/// ```rust
/// let rw = std::sync::RwLock::new(9);
/// let write = rw.write().unwrap();
/// let mut rent = rental::RentRwLockMut::new(write, |w| &mut *w);
/// *rent *= 10;
/// assert_eq!(*rent, 90);
/// ```
#[cfg(feature = "std")]
pub type RentRwLockMut<'rental, T: 'rental, B: 'rental> = RentMut<'rental, sync::RwLockWriteGuard<'rental, T>, B>;

/// A predefined type that rents references from a `String`.
/// # Examples
/// ```rust
/// let s = "Hello, world!".to_string();
/// let rent = rental::RentString::new(s, |s| &s[0..5]);
/// assert_eq!(&*rent, "Hello");
/// ```
#[cfg(feature = "std")]
pub type RentString<'rental, B: 'rental> = RentRef<'rental, String, B>;

/// A predefined type that rents mutable references from a `String`.
/// # Examples
/// ```rust
/// use std::ascii::AsciiExt;
///
/// let s = "Hello, world!".to_string();
/// let mut rent = rental::RentStringMut::new(s, |s| &mut s[0..5]);
/// rent.make_ascii_uppercase();
/// assert_eq!(&*rent, "HELLO");
/// ```
#[cfg(feature = "std")]
pub type RentStringMut<'rental, B: 'rental> = RentMut<'rental, String, B>;

/// A predefined type that rents references from a `Vec<T>`.
/// # Examples
/// ```rust
/// let v = vec![1, 2, 3];
/// let rent = rental::RentVec::new(v, |v| &v[0..2]);
/// assert_eq!(rent.len(), 2);
/// ```
#[cfg(feature = "std")]
pub type RentVec<'rental, T: 'rental, B: 'rental> = RentRef<'rental, Vec<T>, B>;

/// A predefined type that rents mutable references from a `Vec<T>`.
/// # Examples
/// ```rust
/// let v = vec![1, 2, 3];
/// let mut rent = rental::RentVecMut::new(v, |v| &mut v[0..2]);
/// rent[1] *= 2;
/// rent[0] += rent[1];
/// assert_eq!(&*rent, [5, 4]);
/// ```
#[cfg(feature = "std")]
pub type RentVecMut<'rental, T: 'rental, B: 'rental> = RentMut<'rental, Vec<T>, B>;


#[cfg(test)]
mod test {
	use c::ops::{Deref, DerefMut};


	pub struct Foo<T> {
		val: T,
	}


	pub struct FooBorrow<'f, T: 'f> {
		val: &'f T,
	}


	pub struct FooBorrowMut<'f, T: 'f> {
		val: &'f mut T,
	}


	impl<T> Foo<T> {
		pub fn borrow(&self) -> FooBorrow<T> { FooBorrow{val: &self.val} }
		pub fn borrow_mut(&mut self) -> FooBorrowMut<T> { FooBorrowMut{val: &mut self.val} }
	}


	impl<'f, T> FooBorrow<'f, T> {
		pub fn frob(self) -> FooBorrow<'f, T> { self }
		pub fn try_frob(self) -> Result<FooBorrow<'f, T>, (i32, FooBorrow<'f, T>)> { Ok(self) }
	}


	impl<'f, T> Deref for FooBorrow<'f, T> {
		type Target = T;

		fn deref(&self) -> &T { self.val }
	}


	impl<'f, T> Clone for FooBorrow<'f, T> {
		fn clone(&self) -> FooBorrow<'f, T> {
			FooBorrow{val: self.val}
		}
	}


	impl<'f, T> FooBorrowMut<'f, T> {
		pub fn frob(self) -> FooBorrowMut<'f, T> { self }
		pub fn fail_frob(self) -> Result<FooBorrowMut<'f, T>, (i32, FooBorrowMut<'f, T>)> { Err((-1, self)) }
	}


	impl<'f, T> Deref for FooBorrowMut<'f, T> {
		type Target = T;

		fn deref(&self) -> &T { self.val }
	}


	impl<'f, T> DerefMut for FooBorrowMut<'f, T> {
		fn deref_mut(&mut self) -> &mut T { self.val }
	}


	rental!{
		mod test {
			use super::*;

			pub rental RentRefFoo<'rental, 'a, T: ['rental + 'a]> (&'a Foo<T>, FooBorrow<'rental, T>): Deref(T);
			pub rental mut RentRefFooMut<'rental, 'a, T: ['rental + 'a]> (&'a mut Foo<T>, FooBorrowMut<'rental, T>): Deref(T);

			pub rental TestRefFooI32<'rental, 'a> (&'a Foo<i32>, FooBorrow<'rental, i32>): Deref(i32);
			pub rental TestRefFooMutI32<'rental, 'a> (&'a mut Foo<i32>, FooBorrowMut<'rental, i32>): Deref(i32);

			pub mapper MapFoo<'rental, T> (FooBorrow<'rental, T>) -> (FooBorrow<'rental, T>) where [T: 'rental];
			pub mapper MapFooMut<'rental, T> (FooBorrowMut<'rental, T>) -> (FooBorrowMut<'rental, T>) where [T: 'rental];
		}
	}


	#[cfg(feature = "std")]
	rental!{
		mod test_std {
			use super::*;
			use std::cell::{RefCell, Ref};

			pub rental RentFoo<'rental, T: ['rental]> (Box<Foo<T>>, FooBorrow<'rental, T>): Deref(T);
			pub rental mut RentFooMut<'rental, T: ['rental]> (Box<Foo<T>>, FooBorrowMut<'rental, T>): Deref(T);

			pub rental TestFooI32<'rental> (Box<Foo<i32>>, FooBorrow<'rental, i32>): Deref(i32);
			pub rental TestFooMutI32<'rental> (Box<Foo<i32>>, FooBorrowMut<'rental, i32>): Deref(i32);
			pub rental TestFooBox<'rental, T: ['rental]> (Box<Foo<T>>, Box<FooBorrow<'rental, T>>);
			pub rental mut TestFooMutBox<'rental, T: ['rental]> (Box<Foo<T>>, Box<FooBorrowMut<'rental, T>>);

			pub rental TestRefCell<'rental, T: ['rental]> (Box<RefCell<T>>, Box<Ref<'rental, T>>);
		}
	}


	use self::test::*;
	#[cfg(feature = "std")]
	use self::test_std::*;


	#[test]
	fn new() {
		let foo = &Foo{val: 5};
		RentRefFoo::new(foo, |f| f.borrow());
	}


	#[test]
	fn new_mut() {
		let foo = &mut Foo{val: 5};
		RentRefFooMut::new(foo, |f| f.borrow_mut());
	}


	#[test]
	fn rent() {
		let foo = &Foo{val: 5};
		let foo = RentRefFoo::new(foo, |f| f.borrow());
		assert_eq!(foo.rent(|fb| **fb), 5);
	}


	#[test]
	fn rent_mut() {
		let foo = &mut Foo{val: 5};
		let mut foo = RentRefFooMut::new(foo, |f| f.borrow_mut());
		foo.rent_mut(|fbm| (*fbm.val) = 12);
		assert_eq!(foo.rent(|fbm| **fbm), 12);
	}


	#[test]
	fn rent_ref() {
		let foo = &Foo{val: 5};
		let foo = RentRefFoo::new(foo, |f| f.borrow());
		let ft = foo.rent_ref(|fb| fb.val);
		assert_eq!(*ft, 5);
	}


	#[test]
	fn rent_mut_ref() {
		let foo = &mut Foo{val: 5};
		let mut foo = RentRefFooMut::new(foo, |f| f.borrow_mut());
		let ft = foo.rent_mut_ref(|fbm| fbm.val);
		*ft = 3;
		assert_eq!(*ft, 3);
	}


	#[test]
	fn deref() {
		let foo = &Foo{val: 5};
		let foo = RentRefFoo::new(foo, |f| f.borrow());
		assert_eq!(*foo, 5);
	}


	#[test]
	fn deref_mut() {
		let foo = &mut Foo{val: 5};
		let mut foo = RentRefFooMut::new(foo, |f| f.borrow_mut());
		*foo = 12;
		assert_eq!(*foo, 12);
	}


	#[test]
	fn map() {
		let foo = &Foo{val: 5};
		let mut foo = RentRefFoo::new(foo, |f| f.borrow());
		foo = MapFoo::map(foo, |b| b.frob());
		foo.rent(|b| assert_eq!(**b, 5));

		let foo_mut = &mut Foo{val: 12};
		let mut foo_mut = RentRefFooMut::new(foo_mut, |f| f.borrow_mut());
		foo_mut = MapFooMut::map(foo_mut, |b| b.frob());
		foo_mut.rent_mut(|b| assert_eq!(**b, 12));
	}


	#[test]
	fn try_map() {
		let foo = &Foo{val: 5};
		let mut foo = RentRefFoo::new(foo, |f| f.borrow());
		foo = match MapFoo::try_map(foo, |b| b.try_frob()) {
			Ok(f) => f,
			Err((_, _)) => panic!(),
		};
		foo.rent(|b| assert_eq!(**b, 5));

		let _foo_mut = &mut Foo{val: 12};
		let mut _foo_mut = RentRefFooMut::new(_foo_mut, |f| f.borrow_mut());
		_foo_mut = match MapFooMut::try_map(_foo_mut, |b| b.fail_frob()) {
			Ok(f) => f,
			Err((e, mut f)) => {
				assert_eq!(e, -1);
				f.rent_mut(|b| assert_eq!(**b, 12));
				return;
			},
		};

		panic!();
	}


	#[test]
	fn borrow() {
		let foo = &Foo{val: 5};
		let foo = RentRefFoo::new(foo, |f| f.borrow());

		let _: &i32 = ::c::convert::AsRef::as_ref(&foo);
		let _: &i32 = ::c::borrow::Borrow::borrow(&foo);

		let foo_mut = &mut Foo{val: 12};
		let mut foo_mut = RentRefFooMut::new(foo_mut, |f| f.borrow_mut());

		let _: &mut i32 = ::c::convert::AsMut::as_mut(&mut foo_mut);
		let _: &mut i32 = ::c::borrow::BorrowMut::borrow_mut(&mut foo_mut);
	}


	#[cfg(feature = "std")]
	#[test]
	fn std_new() {
		RentFoo::new(Box::new(Foo{val: 5}), |f| f.borrow());
	}


	#[cfg(feature = "std")]
	#[test]
	fn std_new_mut() {
		RentFooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());
	}


	#[cfg(feature = "std")]
	#[test]
	fn std_rent() {
		let foo = RentFoo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		assert_eq!(foo.rent(|fb| **fb), 5);
	}


	#[cfg(feature = "std")]
	#[test]
	fn std_rent_mut() {
		let mut foo = RentFooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());
		foo.rent_mut(|fbm| (*fbm.val) = 12);
		assert_eq!(foo.rent(|fbm| **fbm), 12);
	}


	#[cfg(feature = "std")]
	#[test]
	fn std_rent_ref() {
		let foo = RentFoo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		let ft = foo.rent_ref(|fb| fb.val);
		assert_eq!(*ft, 5);
	}


	#[cfg(feature = "std")]
	#[test]
	fn std_rent_mut_ref() {
		let mut foo = RentFooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());
		let ft = foo.rent_mut_ref(|fbm| fbm.val);
		*ft = 3;
		assert_eq!(*ft, 3);
	}


	#[cfg(feature = "std")]
	#[test]
	fn std_deref() {
		let foo = RentFoo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		assert_eq!(*foo, 5);
	}


	#[cfg(feature = "std")]
	#[test]
	fn std_deref_mut() {
		let mut foo = RentFooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());
		*foo = 12;
		assert_eq!(*foo, 12);
	}


	#[cfg(feature = "std")]
	#[test]
	fn std_map() {
		let mut foo = RentFoo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		foo = MapFoo::map(foo, |b| b.frob());
		foo.rent(|b| assert_eq!(**b, 5));

		let mut foo_mut = RentFooMut::new(Box::new(Foo{val: 12}), |f| f.borrow_mut());
		foo_mut = MapFooMut::map(foo_mut, |b| b.frob());
		foo_mut.rent_mut(|b| assert_eq!(**b, 12));
	}


	#[cfg(feature = "std")]
	#[test]
	fn std_try_map() {
		let mut foo = RentFoo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		foo = match MapFoo::try_map(foo, |b| b.try_frob()) {
			Ok(f) => f,
			Err((_, _)) => panic!(),
		};
		foo.rent(|b| assert_eq!(**b, 5));

		let mut _foo_mut = RentFooMut::new(Box::new(Foo{val: 12}), |f| f.borrow_mut());
		_foo_mut = match MapFooMut::try_map(_foo_mut, |b| b.fail_frob()) {
			Ok(f) => f,
			Err((e, mut f)) => {
				assert_eq!(e, -1);
				f.rent_mut(|b| assert_eq!(**b, 12));
				return;
			},
		};

		panic!();
	}


	#[cfg(feature = "std")]
	#[test]
	fn std_borrow() {
		let foo = RentFoo::new(Box::new(Foo{val: 5}), |f| f.borrow());

		let _: &i32 = ::c::convert::AsRef::as_ref(&foo);
		let _: &i32 = ::c::borrow::Borrow::borrow(&foo);

		let mut foo_mut = RentFooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());

		let _: &mut i32 = ::c::convert::AsMut::as_mut(&mut foo_mut);
		let _: &mut i32 = ::c::borrow::BorrowMut::borrow_mut(&mut foo_mut);
	}
}
