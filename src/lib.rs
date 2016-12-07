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
		)$(: Deref($($deref_ty:tt)*))* where [$($clause:tt)*];
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
			$owner_ty: $crate::FixedDeref,
			$($clause)*
		{
			owner: ::std::option::Option<$owner_ty>,
			rental: ::std::option::Option<$($rental_ty)*>,
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref,
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
				F__: for<'a__> ::std::ops::FnOnce(&'a__ <$owner_ty as ::std::ops::Deref>::Target) -> rental_rebind__!('a__ $($rental_ty)*)
			{
				$rental{
					rental: unsafe {
						Some(::std::mem::transmute(f(&*<$owner_ty as ::std::ops::Deref>::deref(&owner))))
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
				-> ::std::result::Result<$rental<'rental $(, $param)*>, (E__, $owner_ty)> where
				F__: for<'a__> ::std::ops::FnOnce(&'a__ <$owner_ty as ::std::ops::Deref>::Target) -> ::std::result::Result<rental_rebind__!('a__ $($rental_ty)*), E__>
			{
				Ok($rental{
					rental: unsafe {
						let ptr: *const _ = &*<$owner_ty as ::std::ops::Deref>::deref(&owner);
						match f(&*ptr) {
							Ok(asset) => Some(::std::mem::transmute(asset)),
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
			/// closure may return anything, including a borrow, as long as the
			/// existential `'rental` lifetime does not appear in the type
			/// signature.
			#[allow(dead_code)]
			pub fn rent<'s__, F__, R__>(&'s__ self, f: F__) -> R__ where
				F__: for<'a__> ::std::ops::FnOnce(&'s__ rental_rebind__!('a__ $($rental_ty)*)) -> R__,
				R__: 's__
			{
				f(self.rental.as_ref().unwrap())
			}
		}


		unsafe impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::Rental for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref,
			$($clause)*
		{
			type Owner = $owner_ty;
			type Rental = $($rental_ty)*;


			#[inline(always)]
			unsafe fn rental(&self) -> &$($rental_ty)* { self.rental.as_ref().unwrap() }
			fn from_parts(owner: $owner_ty, rent: $($rental_ty)*) -> Self { $rental{owner: Some(owner), rental: Some(rent)} }
			unsafe fn into_parts(mut self) -> ($owner_ty, $($rental_ty)*) { (self.owner.take().unwrap(), self.rental.take().unwrap()) }
			fn into_owner(mut self) -> $owner_ty { self.owner.take().unwrap() }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::ops::Deref for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref,
			<$rental<'rental $(, $param)*> as $crate::Rental>::Rental: ::std::ops::Deref<Target=rental_deref_ty__!($($($deref_ty)*)*)>,
			$($clause)*
		{
			type Target = <<$rental<'rental $(, $param)*> as $crate::Rental>::Rental as ::std::ops::Deref>::Target;

			#[inline(always)]
			fn deref(&self) -> &<<$rental<'rental $(, $param)*> as $crate::Rental>::Rental as ::std::ops::Deref>::Target {
				use $crate::Rental;
				unsafe { <<$rental<'rental $(, $param)*> as $crate::Rental>::Rental as ::std::ops::Deref>::deref(self.rental()) }
			}
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::convert::AsRef<rental_deref_ty__!($($($deref_ty)*)*)> for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref,
			$rental<'rental $(, $param)*>: ::std::ops::Deref<Target=rental_deref_ty__!($($($deref_ty)*)*)>,
			$($clause)*
		{
			fn as_ref(&self) -> &rental_deref_ty__!($($($deref_ty)*)*) { &**self }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::borrow::Borrow<rental_deref_ty__!($($($deref_ty)*)*)> for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref,
			$rental<'rental $(, $param)*>: ::std::ops::Deref<Target=rental_deref_ty__!($($($deref_ty)*)*)>,
			$($clause)*
		{
			fn borrow(&self) -> &rental_deref_ty__!($($($deref_ty)*)*) { &**self }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::cmp::PartialEq for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref,
			$rental<'rental $(, $param)*>: ::std::ops::Deref,
			<$rental<'rental $(, $param)*> as ::std::ops::Deref>::Target: PartialEq,
			$($clause)*
		{
			fn eq(&self, other: &$rental<'rental $(, $param)*>) -> bool { **self == **other }
			fn ne(&self, other: &$rental<'rental $(, $param)*>) -> bool { **self != **other }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::cmp::Eq for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref,
			$rental<'rental $(, $param)*>: ::std::ops::Deref,
			<$rental<'rental $(, $param)*> as ::std::ops::Deref>::Target: Eq,
			$($clause)*
		{ }


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::cmp::PartialOrd for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref,
			$rental<'rental $(, $param)*>: ::std::ops::Deref,
			<$rental<'rental $(, $param)*> as ::std::ops::Deref>::Target: PartialOrd,
			$($clause)*
		{
			fn partial_cmp(&self, other: &$rental<'rental $(, $param)*>) -> ::std::option::Option<::std::cmp::Ordering> { ::std::cmp::PartialOrd::partial_cmp(&**self, &**other) }

			fn lt(&self, other: &$rental<'rental $(, $param)*>) -> bool { **self < **other }
			fn le(&self, other: &$rental<'rental $(, $param)*>) -> bool { **self <= **other }
			fn gt(&self, other: &$rental<'rental $(, $param)*>) -> bool { **self > **other }
			fn ge(&self, other: &$rental<'rental $(, $param)*>) -> bool { **self >= **other }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::cmp::Ord for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref,
			$rental<'rental $(, $param)*>: ::std::ops::Deref,
			<$rental<'rental $(, $param)*> as ::std::ops::Deref>::Target: Ord,
			$($clause)*
		{
			fn cmp(&self, other: &$rental<'rental $(, $param)*>) -> ::std::cmp::Ordering { ::std::cmp::Ord::cmp(&**self, &**other) }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::hash::Hash for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref,
			for<'a__> rental_rebind__!('a__ $($rental_ty)*): ::std::hash::Hash,
			$($clause)*
		{
			fn hash<H>(&self, state: &mut H) where H: ::std::hash::Hasher {
				self.rent(|b| ::std::hash::Hash::hash(b, state))
			}
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::fmt::Debug for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref,
			for<'a__> rental_rebind__!('a__ $($rental_ty)*): ::std::fmt::Debug,
			$($clause)*
		{
			fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::result::Result<(), ::std::fmt::Error> {
				self.rent(|b| ::std::fmt::Debug::fmt(b, fmt))
			}
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::fmt::Display for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref,
			for<'a__> rental_rebind__!('a__ $($rental_ty)*): ::std::fmt::Display,
			$($clause)*
		{
			fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::result::Result<(), ::std::fmt::Error> {
				self.rent(|b| ::std::fmt::Display::fmt(b, fmt))
			}
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::ops::Drop for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref,
			$($clause)*
		{
			fn drop(&mut self) {
				::std::mem::drop(self.rental.take());
				::std::mem::drop(self.owner.take());
			}
		}


		rental!{@ITEM $($rest)*}
	};


	{
		@ITEM pub rental mut $rental:ident<'rental $(, $param:tt $(: [$($bounds:tt)*])*)*> (
			$owner_ty:ty,
			$($rental_ty:tt)*
		)$(: Deref($($deref_ty:tt)*))* where [$($clause:tt)*];
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
			$owner_ty: $crate::FixedDeref + ::std::ops::DerefMut,
			$($clause)*
		{
			owner: ::std::option::Option<$owner_ty>,
			rental: ::std::option::Option<$($rental_ty)*>,
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref + ::std::ops::DerefMut,
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
				F__: for<'a__> ::std::ops::FnOnce(&'a__ mut <$owner_ty as ::std::ops::Deref>::Target) -> rental_rebind__!('a__ $($rental_ty)*)
			{
				$rental{
					rental: unsafe {
						Some(::std::mem::transmute(f(&mut *<$owner_ty as ::std::ops::DerefMut>::deref_mut(&mut owner))))
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
				-> ::std::result::Result<$rental<'rental $(, $param)*>, (E__, $owner_ty)> where
				F__: for<'a__> ::std::ops::FnOnce(&'a__ mut <$owner_ty as ::std::ops::Deref>::Target) -> ::std::result::Result<rental_rebind__!('a__ $($rental_ty)*), E__>
			{
				Ok($rental{
					rental: unsafe {
						let ptr: *mut _ = &mut *<$owner_ty as ::std::ops::DerefMut>::deref_mut(&mut owner);
						match f(&mut *ptr) {
							Ok(asset) => Some(::std::mem::transmute(asset)),
							Err(err) => return Err((err, owner)),
						}
					},
					owner: Some(owner),
				})
			}


			/// Executes a closure on the existentially borrowed rental. The
			/// closure may return anything, including a borrow, as long as the
			/// existential `'rental` lifetime does not appear in the type
			/// signature.
			#[allow(dead_code)]
			pub fn rent<'s__, F__, R__>(&'s__ self, f: F__) -> R__ where
				F__: for<'a__> ::std::ops::FnOnce(&'s__ rental_rebind__!('a__ $($rental_ty)*)) -> R__,
				R__: 's__
			{
				f(self.rental.as_ref().unwrap())
			}


			/// As [`rent`](#method.rent) but the rental is mutable.
			#[allow(dead_code)]
			pub fn rent_mut<'s__, F__, R__>(&'s__ mut self, f: F__) -> R__ where
				F__: for<'a__> ::std::ops::FnOnce(&'s__ mut rental_rebind__!('a__ $($rental_ty)*)) -> R__,
				R__: 's__
			{
				f(self.rental.as_mut().unwrap())
			}
		}


		unsafe impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::Rental for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref + ::std::ops::DerefMut,
			$($clause)*
		{
			type Owner = $owner_ty;
			type Rental = $($rental_ty)*;


			#[inline(always)]
			unsafe fn rental(&self) -> &$($rental_ty)* { self.rental.as_ref().unwrap() }
			fn from_parts(owner: $owner_ty, rent: $($rental_ty)*) -> Self { $rental{owner: Some(owner), rental: Some(rent)} }
			unsafe fn into_parts(mut self) -> ($owner_ty, $($rental_ty)*) { (self.owner.take().unwrap(), self.rental.take().unwrap()) }
			fn into_owner(mut self) -> $owner_ty { self.owner.take().unwrap() }
		}


		unsafe impl<'rental $(, $param $(: $($bounds)*)*)*> $crate::RentalMut for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref + ::std::ops::DerefMut,
			$($clause)*
		{
			#[inline(always)]
			unsafe fn rental_mut(&mut self) -> &mut <Self as $crate::Rental>::Rental { self.rental.as_mut().unwrap() }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::ops::Deref for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref + ::std::ops::DerefMut,
			<$rental<'rental $(, $param)*> as $crate::Rental>::Rental: ::std::ops::Deref<Target=rental_deref_ty__!($($($deref_ty)*)*)>,
			$($clause)*
		{
			type Target = <<$rental<'rental $(, $param)*> as $crate::Rental>::Rental as ::std::ops::Deref>::Target;

			#[inline(always)]
			fn deref(&self) -> &<<$rental<'rental $(, $param)*> as $crate::Rental>::Rental as ::std::ops::Deref>::Target {
				use $crate::Rental;
				unsafe { <<$rental<'rental $(, $param)*> as $crate::Rental>::Rental as ::std::ops::Deref>::deref(self.rental()) }
			}
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::ops::DerefMut for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref + ::std::ops::DerefMut,
			<$rental<'rental $(, $param)*> as $crate::Rental>::Rental: ::std::ops::Deref<Target=rental_deref_ty__!($($($deref_ty)*)*)> + ::std::ops::DerefMut,
			$($clause)*
		{
			#[inline(always)]
			fn deref_mut(&mut self) -> &mut <<$rental<'rental $(, $param)*> as $crate::Rental>::Rental as ::std::ops::Deref>::Target {
				use $crate::RentalMut;
				unsafe { <<$rental<'rental $(, $param)*> as $crate::Rental>::Rental as ::std::ops::DerefMut>::deref_mut(self.rental_mut()) }
			}
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::convert::AsRef<rental_deref_ty__!($($($deref_ty)*)*)> for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref + ::std::ops::DerefMut,
			$rental<'rental $(, $param)*>: ::std::ops::Deref<Target=rental_deref_ty__!($($($deref_ty)*)*)>,
			$($clause)*
		{
			fn as_ref(&self) -> &rental_deref_ty__!($($($deref_ty)*)*) { &**self }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::convert::AsMut<rental_deref_ty__!($($($deref_ty)*)*)> for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref + ::std::ops::DerefMut,
			$rental<'rental $(, $param)*>: ::std::ops::Deref<Target=rental_deref_ty__!($($($deref_ty)*)*)> + ::std::ops::DerefMut,
			$($clause)*
		{
			fn as_mut(&mut self) -> &mut rental_deref_ty__!($($($deref_ty)*)*) { &mut **self }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::borrow::Borrow<rental_deref_ty__!($($($deref_ty)*)*)> for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref + ::std::ops::DerefMut,
			$rental<'rental $(, $param)*>: ::std::ops::Deref<Target=rental_deref_ty__!($($($deref_ty)*)*)>,
			$($clause)*
		{
			fn borrow(&self) -> &rental_deref_ty__!($($($deref_ty)*)*) { &**self }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::borrow::BorrowMut<rental_deref_ty__!($($($deref_ty)*)*)> for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref + ::std::ops::DerefMut,
			$rental<'rental $(, $param)*>: ::std::ops::Deref<Target=rental_deref_ty__!($($($deref_ty)*)*)> + ::std::ops::DerefMut,
			$($clause)*
		{
			fn borrow_mut(&mut self) -> &mut rental_deref_ty__!($($($deref_ty)*)*) { &mut **self }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::cmp::PartialEq for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref + ::std::ops::DerefMut,
			$rental<'rental $(, $param)*>: ::std::ops::Deref,
			<$rental<'rental $(, $param)*> as ::std::ops::Deref>::Target: PartialEq,
			$($clause)*
		{
			fn eq(&self, other: &$rental<'rental $(, $param)*>) -> bool { **self == **other }
			fn ne(&self, other: &$rental<'rental $(, $param)*>) -> bool { **self != **other }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::cmp::Eq for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref + ::std::ops::DerefMut,
			$rental<'rental $(, $param)*>: ::std::ops::Deref,
			<$rental<'rental $(, $param)*> as ::std::ops::Deref>::Target: Eq,
			$($clause)*
		{ }


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::cmp::PartialOrd for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref + ::std::ops::DerefMut,
			$rental<'rental $(, $param)*>: ::std::ops::Deref,
			<$rental<'rental $(, $param)*> as ::std::ops::Deref>::Target: PartialOrd,
			$($clause)*
		{
			fn partial_cmp(&self, other: &$rental<'rental $(, $param)*>) -> ::std::option::Option<::std::cmp::Ordering> { ::std::cmp::PartialOrd::partial_cmp(&**self, &**other) }

			fn lt(&self, other: &$rental<'rental $(, $param)*>) -> bool { **self < **other }
			fn le(&self, other: &$rental<'rental $(, $param)*>) -> bool { **self <= **other }
			fn gt(&self, other: &$rental<'rental $(, $param)*>) -> bool { **self > **other }
			fn ge(&self, other: &$rental<'rental $(, $param)*>) -> bool { **self >= **other }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::cmp::Ord for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref + ::std::ops::DerefMut,
			$rental<'rental $(, $param)*>: ::std::ops::Deref,
			<$rental<'rental $(, $param)*> as ::std::ops::Deref>::Target: Ord,
			$($clause)*
		{
			fn cmp(&self, other: &$rental<'rental $(, $param)*>) -> ::std::cmp::Ordering { ::std::cmp::Ord::cmp(&**self, &**other) }
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::hash::Hash for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref + ::std::ops::DerefMut,
			for<'a__> rental_rebind__!('a__ $($rental_ty)*): ::std::hash::Hash,
			$($clause)*
		{
			fn hash<H>(&self, state: &mut H) where H: ::std::hash::Hasher {
				self.rent(|b| ::std::hash::Hash::hash(b, state))
			}
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::fmt::Debug for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref + ::std::ops::DerefMut,
			for<'a__> rental_rebind__!('a__ $($rental_ty)*): ::std::fmt::Debug,
			$($clause)*
		{
			fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::result::Result<(), ::std::fmt::Error> {
				self.rent(|b| ::std::fmt::Debug::fmt(b, fmt))
			}
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::fmt::Display for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref + ::std::ops::DerefMut,
			for<'a__> rental_rebind__!('a__ $($rental_ty)*): ::std::fmt::Display,
			$($clause)*
		{
			fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::result::Result<(), ::std::fmt::Error> {
				self.rent(|b| ::std::fmt::Display::fmt(b, fmt))
			}
		}


		impl<'rental $(, $param $(: $($bounds)*)*)*> ::std::ops::Drop for $rental<'rental $(, $param)*> where
			$owner_ty: $crate::FixedDeref + ::std::ops::DerefMut,
			$($clause)*
		{
			fn drop(&mut self) {
				::std::mem::drop(self.rental.take());
				::std::mem::drop(self.owner.take());
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
				F__: for<'f__: 'rental> ::std::ops::FnOnce(rental_rebind__!('f__ $($from_ty)*)) -> rental_rebind__!('f__ $($into_ty)*),
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
			pub fn try_map<'rental $(, $param $(: $($bounds)*)*)*, T__, U__, E__, F__>(t: T__, f: F__) -> ::std::result::Result<U__, (E__, T__)> where
				T__: $crate::Rental<Rental=$($from_ty)*>,
				U__: $crate::Rental<Owner=<T__ as $crate::Rental>::Owner, Rental=$($into_ty)*>,
				F__: for<'f__: 'rental> ::std::ops::FnOnce(rental_rebind__!('f__ $($from_ty)*)) -> ::std::result::Result<rental_rebind__!('f__ $($into_ty)*), (E__, rental_rebind__!('f__ $($from_ty)*))>,
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
		$into:tt {$($head:tt)*} 'rental $($tail:tt)*
	) => {
		rental_rebind__!($into {$($head)* $into} $($tail)*)
	};
	(
		$into:tt {$($head:tt)*} ($($inner:tt)*) $($tail:tt)*
	) => {
		rental_rebind__!($into {($($head)*)} $($inner)* @> $($tail)*)
	};
	(
		$into:tt {$($head:tt)*} [$($inner:tt)*] $($tail:tt)*
	) => {
		rental_rebind__!($into {[$($head)*]} $($inner)* @> $($tail)*)
	};
	(
		$into:tt {$($head:tt)*} {$($inner:tt)*} $($tail:tt)*
	) => {
		rental_rebind__!($into {{$($head)*}} $($inner)* @> $($tail)*)
	};
	(
		$into:tt {($($head:tt)*) $($inner:tt)*} @> $($tail:tt)*
	) => {
		rental_rebind__!($into {$($head)*($($inner)*)} $($tail)*)
	};
	(
		$into:tt {[$($head:tt)*] $($inner:tt)*} @> $($tail:tt)*
	) => {
		rental_rebind__!($into {$($head)*[$($inner)*]} $($tail)*)
	};
	(
		$into:tt {{$($head:tt)*} $($inner:tt)*} @> $($tail:tt)*
	) => {
		rental_rebind__!($into {$($head)*{$($inner)*}} $($tail)*)
	};
	(
		$into:tt {$($head:tt)*} $tok:tt $($tail:tt)*
	) => {
		rental_rebind__!($into {$($head)* $tok} $($tail)*)
	};
	(
		$into:tt {$($rebound:tt)*}
	) => {
		$($rebound)*
	};
	(
		$into:tt $($tail:tt)*
	) => {
		rental_rebind__!($into {} $($tail)*)
	};
}


#[doc(hidden)]
#[macro_export]
macro_rules! rental_deref_ty__ {
	( ) => { $crate::NoDeref };
	( $($deref_ty:tt)+ ) => { rental_rebind__!('_ $($deref_ty)+) };
}


use std::ops::Deref;
use std::{cell, rc, sync};


/// This trait indicates both that the type can be dereferenced, and that when
/// it is, the target has a fixed memory address while it is held by a rental
/// struct.
///
/// This trait is already implemented for common standard types that
/// fulfill these requirements. It must be implemented for a type to be eligible
/// as an owner in a rental struct.
pub unsafe trait FixedDeref: Deref { }

unsafe impl<'t, T: ?Sized> FixedDeref for &'t T { }
unsafe impl<'t, T: ?Sized> FixedDeref for &'t mut T { }

unsafe impl<T: ?Sized> FixedDeref for Box<T> { }
unsafe impl<T> FixedDeref for Vec<T> { }
unsafe impl FixedDeref for String { }

unsafe impl<T: ?Sized> FixedDeref for rc::Rc<T> { }
unsafe impl<T: ?Sized> FixedDeref for sync::Arc<T> { }

unsafe impl<'t, T: ?Sized> FixedDeref for cell::Ref<'t, T> { }
unsafe impl<'t, T: ?Sized> FixedDeref for cell::RefMut<'t, T> { }
unsafe impl<'t, T: ?Sized> FixedDeref for sync::MutexGuard<'t, T> { }
unsafe impl<'t, T: ?Sized> FixedDeref for sync::RwLockReadGuard<'t, T> { }
unsafe impl<'t, T: ?Sized> FixedDeref for sync::RwLockWriteGuard<'t, T> { }


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
		use std::ops::DerefMut;

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
pub type RentArc<'rental, T: 'rental, B: 'rental> = RentRef<'rental, sync::Arc<T>, B>;

/// A predefined type that rents references from a `Box<T>`.
/// # Examples
/// ```rust
/// let bx = Box::new(2);
/// let rent = rental::RentBox::new(bx, |b| &*b);
/// assert_eq!(*rent, 2);
/// ```
pub type RentBox<'rental, T: 'rental, B: 'rental> = RentRef<'rental, Box<T>, B>;

/// A predefined type that rents mutable references from a `Box<T>`.
/// # Examples
/// ```rust
/// let bx = Box::new(3);
/// let mut rent = rental::RentBoxMut::new(bx, |b| &mut *b);
/// *rent *= 10;
/// assert_eq!(*rent, 30);
/// ```
pub type RentBoxMut<'rental, T: 'rental, B: 'rental> = RentMut<'rental, Box<T>, B>;

/// A predefined type that rents references from a `MutexGuard<T>`.
/// # Examples
/// ```rust
/// let mutex = std::sync::Mutex::new(4);
/// let guard = mutex.lock().unwrap();
/// let rent = rental::RentMutex::new(guard, |g| &*g);
/// assert_eq!(*rent, 4);
/// ```
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
pub type RentMutexMut<'rental, T: 'rental, B: 'rental> = RentMut<'rental, sync::MutexGuard<'rental, T>, B>;

/// A predefined type that rents references from a `Ref<T>`.
/// # Examples
/// ```rust
/// let cell = std::cell::RefCell::new(6);
/// let r = cell.borrow();
/// let rent = rental::RentRefCell::new(r, |r| &*r);
/// assert_eq!(*rent, 6);
/// ```
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
pub type RentRefCellMut<'rental, T: 'rental, B: 'rental> = RentMut<'rental, cell::RefMut<'rental, T>, B>;

/// A predefined type that rents references from an `RwLockReadGuard<T>`.
/// # Examples
/// ```rust
/// let rw = std::sync::RwLock::new(8);
/// let read = rw.read().unwrap();
/// let rent = rental::RentRwLock::new(read, |r| &*r);
/// assert_eq!(*rent, 8);
/// ```
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
pub type RentRwLockMut<'rental, T: 'rental, B: 'rental> = RentMut<'rental, sync::RwLockWriteGuard<'rental, T>, B>;

/// A predefined type that rents references from a `String`.
/// # Examples
/// ```rust
/// let s = "Hello, world!".to_string();
/// let rent = rental::RentString::new(s, |s| &s[0..5]);
/// assert_eq!(&*rent, "Hello");
/// ```
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
pub type RentStringMut<'rental, B: 'rental> = RentMut<'rental, String, B>;

/// A predefined type that rents references from a `Vec<T>`.
/// # Examples
/// ```rust
/// let v = vec![1, 2, 3];
/// let rent = rental::RentVec::new(v, |v| &v[0..2]);
/// assert_eq!(rent.len(), 2);
/// ```
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
pub type RentVecMut<'rental, T: 'rental, B: 'rental> = RentMut<'rental, Vec<T>, B>;


#[cfg(test)]
mod test {
	use std::ops::{Deref, DerefMut};
	use std::fmt::{self, Debug, Display};


	pub struct Foo<T> {
		val: T,
	}


	pub struct FooBorrow<'f, T: 'f> {
		val: &'f T,
		tag: i32,
	}


	pub struct FooBorrowMut<'f, T: 'f> {
		val: &'f mut T,
		tag: i32,
	}


	pub struct FooNoDeref<'f, T: 'f> {
		_val: &'f T,
		_tag: i32,
	}


	impl<T> Foo<T> {
		pub fn borrow(&self) -> FooBorrow<T> { FooBorrow{val: &self.val, tag: 1} }
		pub fn borrow_mut(&mut self) -> FooBorrowMut<T> { FooBorrowMut{val: &mut self.val, tag: 2} }
		pub fn _borrow_no_deref(&self) -> FooNoDeref<T> { FooNoDeref{_val: &self.val, _tag: 2} }
	}


	impl<'f, T> FooBorrow<'f, T> {
		pub fn frob(self) -> FooBorrow<'f, T> { self }
		pub fn try_frob(self) -> Result<FooBorrow<'f, T>, (String, FooBorrow<'f, T>)> { Ok(self) }
	}


	impl<'f, T> Deref for FooBorrow<'f, T> {
		type Target = T;

		fn deref(&self) -> &T { self.val }
	}


	impl<'f, T> Clone for FooBorrow<'f, T> {
		fn clone(&self) -> FooBorrow<'f, T> {
			FooBorrow{val: self.val, tag: self.tag}
		}
	}


	impl<'f, T: Debug> Debug for FooBorrow<'f, T> {
		fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
			fmt::Debug::fmt(&**self, fmt)
		}
	}


	impl<'f, T: Display> Display for FooBorrow<'f, T> {
		fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
			fmt::Display::fmt(&**self, fmt)
		}
	}


	impl<'f, T> FooBorrowMut<'f, T> {
		pub fn frob(self) -> FooBorrowMut<'f, T> { self }
		pub fn fail_frob(self) -> Result<FooBorrowMut<'f, T>, (String, FooBorrowMut<'f, T>)> { Err(("Error".to_string(), self)) }
	}


	impl<'f, T> Deref for FooBorrowMut<'f, T> {
		type Target = T;

		fn deref(&self) -> &T { self.val }
	}


	impl<'f, T> DerefMut for FooBorrowMut<'f, T> {
		fn deref_mut(&mut self) -> &mut T { self.val }
	}


	impl<'f, T: Debug> Debug for FooBorrowMut<'f, T> {
		fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
			fmt::Debug::fmt(&**self, fmt)
		}
	}


	impl<'f, T: Display> Display for FooBorrowMut<'f, T> {
		fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
			fmt::Display::fmt(&**self, fmt)
		}
	}


	rental!{
		mod test {
			use super::*;

			pub rental RentFoo<'rental, T: ['rental]> (Box<Foo<T>>, FooBorrow<'rental, T>): Deref(T);
			pub rental mut RentFooMut<'rental, T: ['rental]> (Box<Foo<T>>, FooBorrowMut<'rental, T>): Deref(T);

			pub mapper MapFoo<'rental, T> (FooBorrow<'rental, T>) -> (FooBorrow<'rental, T>) where [T: 'rental];
			pub mapper MapFooMut<'rental, T> (FooBorrowMut<'rental, T>) -> (FooBorrowMut<'rental, T>) where [T: 'rental];

			pub rental TestFooI32<'rental> (Box<Foo<i32>>, FooBorrow<'rental, i32>): Deref(i32);
			pub rental TestFooMutI32<'rental> (Box<Foo<i32>>, FooBorrowMut<'rental, i32>): Deref(i32);
			pub rental TestFooBox<'rental, T: ['rental]> (Box<Foo<T>>, Box<FooBorrow<'rental, T>>);
			pub rental TestFooNoDeref<'rental, T: ['rental]> (Box<Foo<T>>, FooNoDeref<'rental, T>);
		}
	}

	use self::test::*;


	#[test]
	fn new() {
		RentFoo::new(Box::new(Foo{val: 5}), |f| f.borrow());
	}


	#[test]
	fn new_mut() {
		RentFooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());
	}


	#[test]
	fn rent() {
		let foo = RentFoo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		assert_eq!(foo.rent(|fb| **fb), 5);
	}


	#[test]
	fn rent_mut() {
		let mut foo = RentFooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());
		foo.rent_mut(|fbm| (*fbm.val) = 12);
		assert_eq!(foo.rent(|fbm| **fbm), 12);
	}


	#[test]
	fn rent_borrow() {
		let foo = RentFoo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		let ft = foo.rent(|fb| &fb.tag);
		assert_eq!(*ft, 1);
	}


	#[test]
	fn rent_borrow_mut() {
		let mut foo = RentFooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());
		let ft = foo.rent_mut(|fbm| &mut fbm.tag);
		*ft = 3;
		assert_eq!(*ft, 3);
	}


	#[test]
	fn deref() {
		let foo = RentFoo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		assert_eq!(*foo, 5);
	}


	#[test]
	fn deref_mut() {
		let mut foo_mut = RentFooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());
		*foo_mut = 12;
		assert_eq!(*foo_mut, 12);
	}


	#[test]
	fn map() {
		let mut foo = RentFoo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		foo = MapFoo::map(foo, |b| b.frob());
		foo.rent(|b| assert_eq!(**b, 5));

		let mut foo_mut = RentFooMut::new(Box::new(Foo{val: 12}), |f| f.borrow_mut());
		foo_mut = MapFooMut::map(foo_mut, |b| b.frob());
		foo_mut.rent_mut(|b| assert_eq!(**b, 12));
	}


	#[test]
	fn try_map() {
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
				assert_eq!(e, "Error");
				f.rent_mut(|b| assert_eq!(**b, 12));
				return;
			},
		};

		panic!();
	}


	#[test]
	fn debug_display() {
		let foo = RentFoo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		println!("{:?}", foo);
		println!("{}", foo);

		let foo_mut = RentFooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());
		println!("{:?}", foo_mut);
		println!("{}", foo_mut);
	}


	#[test]
	fn cmp() {
		let a = RentFoo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		let b = RentFoo::new(Box::new(Foo{val: 5}), |f| f.borrow());

		assert_eq!(a, b);
		assert!(a <= b);
		assert!(a >= b);
		assert!(!(a < b));
		assert!(!(a > b));
	}


	#[test]
	fn borrow() {
		let foo = RentFoo::new(Box::new(Foo{val: 5}), |f| f.borrow());

		let _: &i32 = ::std::convert::AsRef::as_ref(&foo);
		let _: &i32 = ::std::borrow::Borrow::borrow(&foo);

		let mut foo_mut = RentFooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());

		let _: &mut i32 = ::std::convert::AsMut::as_mut(&mut foo_mut);
		let _: &mut i32 = ::std::borrow::BorrowMut::borrow_mut(&mut foo_mut);
	}
}
