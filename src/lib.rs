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
//! well as the associated type aliases for common rental scenarios. The
//! documentation for the [`rental`](macro.rental.html) macro describes the
//! kinds of items that can be generated. 


/// This macro is the bedrock of the API. It allows you to define three 
/// different kinds of items related to renting.
///
/// NOTE: This macro is only necessary to use if you want to define new rental
/// types that can store customized forms of borrow other than a bare reference.
/// For bare references, this crate provides premade types, called
/// [`RentRef`](struct.RentRef.html) and [`RentMut`](struct.RentMut.html), that
/// accomplish this. Several types aliases are also provided for common
/// scenarios.
///
/// The top level item in an invocation of this macro must be a module. This
/// module can have any name you like, may be public or not, and will hold all
/// declared items.
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
/// pub rental $rental:ident<'rental $(, $param:tt)*> where [$($clause:tt)*] (
///     $owner_ty:ty,
///     $rental_ty:ty$(,)*
/// );
///
/// // Mutable
/// pub rental $rental:ident<'rental $(, $param:tt)*> where [$($clause:tt)*] (
///     mut $owner_ty:ty,
///     $rental_ty:ty$(,)*
/// );
/// ```
///
/// The key difference being the presence or absence of `mut` on the owner
/// declaration. Note that the first generic parameter must be a lifetime
/// called `'rental`. This lifetime is special and represents the lifetime of
/// the rental struct itself. It should be used anywhere in the rented or owner
/// type signatures that represents their mutual link. For example, the
/// definition of [`RentRef`](struct.RentRef.html) looks like this:
///
/// ```rust
/// # #[macro_use] extern crate rental;
/// # fn main() { }
/// # rental!{ mod example {
/// pub rental RentRef<'rental, T, B> where [T: FixedDeref + 'rental, B: 'rental] (T, &'rental B);
/// # }}
/// ```
///
/// The lifetime `'rental` appears as the bound on the reference, because that
/// is the lifetime that will be attached when you create the reference by
/// borrowing from the owner object inside the creation closure. Note that it
/// also appears as a bound on the other generic types, since they must also
/// naturally live at least as long as the struct does. Also, for a type to be
/// eligible as an owner, it must implement the
/// [`FixedDeref`](trait.FixedDeref.html) trait.
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
			#![allow(unused_imports)]
			use ::std::ops::{Deref, DerefMut};
			use ::std::ops::Drop;
			use ::std::result::Result;
			use ::std::mem;

			use $crate::{FixedDeref, Rental, RentalMut, RentalDeref, RentalDerefMut, RentalDerefEq};

			rental!{@ITEM $($items)*}
		}
	};
	{
		pub mod $rental_mod:ident {
			$($items:tt)*
		}
	} => {
		pub mod $rental_mod {
			#![allow(unused_imports)]
			use ::std::ops::{Deref, DerefMut};
			use ::std::ops::Drop;
			use ::std::result::Result;
			use ::std::mem;

			use $crate::{FixedDeref, Rental, RentalMut, RentalDeref, RentalDerefMut, RentalDerefEq};

			rental!{@ITEM $($items)*}
		}
	};


	{
		@ITEM pub rental $rental:ident<'rental $(, $param:tt)*> where [$($clause:tt)*] (
			mut $owner_ty:ty,
			$($rental_ty:tt)*
		);
		$($rest:tt)*
	} => {
		/// A struct representing a particular (owner, rental) pair. All such
		/// structs share a common API, but it is merely a convention enforced
		/// by the [`rental`](macro.rental.html) macro.  The capabilities of a
		/// rental struct cannot be fully described as a trait without language
		/// support for HKT (Higher-Kinded Types). What methods can be
		/// expressed in a trait are documented in the
		/// [`Rental`](trait.Rental.html) and
		/// [`RentalMut`](trait.RentalMut.html) traits.
		///
		/// A rental struct will implement `Deref` and `DerefMut`, but only if
		/// the rented type is `Deref`/`DerefMut` and its target does not
		/// contain the `'rental` lifetime in its signature. 
		pub struct $rental<'rental $(, $param)*> where
			$owner_ty: FixedDeref + DerefMut,
			$($clause)*
		{
			owner: Option<$owner_ty>,
			rental: Option<$($rental_ty)*>,
		}


		impl<'rental $(, $param)*> $rental<'rental $(, $param)*> where
			$owner_ty: FixedDeref + DerefMut,
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
				F__: for<'a__> FnOnce(&'a__ mut <$owner_ty as Deref>::Target) -> rental_rebind__!('a__ $($rental_ty)*)
			{
				$rental{
					rental: unsafe {
						Some(mem::transmute(f(&mut *<$owner_ty as DerefMut>::deref_mut(&mut owner))))
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
				-> Result<$rental<'rental $(, $param)*>, (E__, $owner_ty)> where
				F__: for<'a__> FnOnce(&'a__ mut <$owner_ty as Deref>::Target) -> Result<rental_rebind__!('a__ $($rental_ty)*), E__>
			{
				Ok($rental{
					rental: unsafe {
						let ptr: *mut _ = &mut *<$owner_ty as DerefMut>::deref_mut(&mut owner);
						match f(&mut *ptr) {
							Ok(asset) => Some(mem::transmute(asset)),
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
				F__: for<'a__: 's__> FnOnce(&'s__ rental_rebind__!('a__ $($rental_ty)*)) -> R__, R__: 's__
			{
				f(self.rental.as_ref().unwrap())
			}


			/// As [`rent`](#method.rent) but the rental is mutable.
			#[allow(dead_code)]
			pub fn rent_mut<'s__, F__, R__>(&'s__ mut self, f: F__) -> R__ where
				F__: for<'a__: 's__> FnOnce(&'s__ mut rental_rebind__!('a__ $($rental_ty)*)) -> R__, R__: 's__
			{
				f(self.rental.as_mut().unwrap())
			}
		}


		unsafe impl<'rental $(, $param)*> Rental<'rental> for $rental<'rental $(, $param)*> where
			$owner_ty: FixedDeref + DerefMut,
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


		unsafe impl<'rental $(, $param)*> RentalMut<'rental> for $rental<'rental $(, $param)*> where
			$owner_ty: FixedDeref + DerefMut,
			$($clause)*
		{
			#[inline(always)]
			unsafe fn rental_mut(&mut self) -> &mut <Self as Rental<'rental>>::Rental { self.rental.as_mut().unwrap() }
		}


		impl<'rental $(, $param)*> Deref for $rental<'rental $(, $param)*> where
			$owner_ty: FixedDeref + DerefMut,
			$rental<'rental $(, $param)*>: RentalDeref<'rental> + for<'r__> RentalDerefEq<'r__, $rental<'r__ $(, $param)*>>,
			$($clause)*
		{
			type Target = <$rental<'rental $(, $param)*> as RentalDeref<'rental>>::Target;

			#[inline(always)]
			fn deref(&self) -> &<$rental<'rental $(, $param)*> as RentalDeref<'rental>>::Target { unsafe { self.rental_target() } }
		}


		impl<'rental $(, $param)*> DerefMut for $rental<'rental $(, $param)*> where
			$owner_ty: FixedDeref + DerefMut,
			$rental<'rental $(, $param)*>: RentalDerefMut<'rental> + for<'a__> RentalDerefEq<'a__, $rental<'a__ $(, $param)*>>,
			$($clause)*
		{
			#[inline(always)]
			fn deref_mut(&mut self) -> &mut <$rental<'rental $(, $param)*> as Deref>::Target { unsafe { self.rental_target_mut() } }
		}


		impl<'rental $(, $param)*> Drop for $rental<'rental $(, $param)*> where
			$owner_ty: FixedDeref + DerefMut,
			$($clause)*
		{
			fn drop(&mut self) {
				mem::drop(self.rental.take());
				mem::drop(self.owner.take());
			}
		}


		rental!{@ITEM $($rest)*}
	};


	{
		@ITEM pub rental $rental:ident<'rental $(, $param:tt)*> where [$($clause:tt)*] (
			$owner_ty:ty,
			$($rental_ty:tt)*
		);
		$($rest:tt)*
	} => {
		/// A struct representing a particular (owner, rental) pair. All such
		/// structs share a common API, but it is merely a convention enforced
		/// by the [`rental`](macro.rental.html) macro.  The capabilities of a
		/// rental struct cannot be fully described as a trait without language
		/// support for HKT (Higher-Kinded Types). What methods can be
		/// expressed in a trait are documented in the
		/// [`Rental`](trait.Rental.html) and
		/// [`RentalMut`](trait.RentalMut.html) traits.
		///
		/// A rental struct will implement `Deref`, but only if the rented type
		/// is `Deref` and its target does not contain the `'rental` lifetime
		/// in its signature. 
		pub struct $rental<'rental $(, $param)*> where
			$owner_ty: FixedDeref,
			$($clause)*
		{
			owner: Option<$owner_ty>,
			rental: Option<$($rental_ty)*>,
		}


		impl<'rental $(, $param)*> $rental<'rental $(, $param)*> where
			$owner_ty: FixedDeref,
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
			pub fn new<F__>(owner: $owner_ty, f: F__)
				-> $rental<'rental $(, $param)*> where
				F__: for<'a__> FnOnce(&'a__ <$owner_ty as Deref>::Target) -> rental_rebind__!('a__ $($rental_ty)*)
			{
				$rental{
					rental: unsafe {
						Some(mem::transmute(f(&*<$owner_ty as Deref>::deref(&owner))))
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
				-> Result<$rental<'rental $(, $param)*>, (E__, $owner_ty)> where
				F__: for<'a__> FnOnce(&'a__ <$owner_ty as Deref>::Target) -> Result<rental_rebind__!('a__ $($rental_ty)*), E__>
			{
				Ok($rental{
					rental: unsafe {
						let ptr: *const _ = &*<$owner_ty as Deref>::deref(&owner);
						match f(&*ptr) {
							Ok(asset) => Some(mem::transmute(asset)),
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
				F__: for<'a__: 's__> FnOnce(&'s__ rental_rebind__!('a__ $($rental_ty)*)) -> R__, R__: 's__
			{
				f(self.rental.as_ref().unwrap())
			}
		}


		unsafe impl<'rental $(, $param)*> Rental<'rental> for $rental<'rental $(, $param)*> where
			$owner_ty: FixedDeref,
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


		impl<'rental $(, $param)*> Deref for $rental<'rental $(, $param)*> where
			$owner_ty: FixedDeref,
			$rental<'rental $(, $param)*>: RentalDeref<'rental> + for<'a__> RentalDerefEq<'a__, $rental<'a__ $(, $param)*>>,
			$($clause)*
		{
			type Target = <$rental<'rental $(, $param)*> as RentalDeref<'rental>>::Target;

			#[inline(always)]
			fn deref(&self) -> &<$rental<'rental $(, $param)*> as RentalDeref<'rental>>::Target { unsafe { self.rental_target() } }
		}


		impl<'rental $(, $param)*> Drop for $rental<'rental $(, $param)*> where
			$owner_ty: FixedDeref,
			$($clause)*
		{
			fn drop(&mut self) {
				mem::drop(self.rental.take());
				mem::drop(self.owner.take());
			}
		}


		rental!{@ITEM $($rest)*}
	};


	{
		@ITEM pub mapper $mapper:ident<'rental $(, $param:tt)*>($($from_ty:tt)*) -> ($($into_ty:tt)*) where [$($clause:tt)*];
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
			pub fn map<'rental $(, $param)*, T__, U__, F__>(t: T__, f: F__) -> U__ where
				T__: Rental<'rental, Rental=$($from_ty)*>,
				U__: Rental<'rental, Owner=<T__ as Rental<'rental>>::Owner, Rental=$($into_ty)*>,
				F__: for<'f__: 'rental> FnOnce(rental_rebind__!('f__ $($from_ty)*)) -> rental_rebind__!('f__ $($into_ty)*),
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
			pub fn try_map<'rental $(, $param)*, T__, U__, E__, F__>(t: T__, f: F__) -> Result<U__, (E__, T__)> where
				T__: Rental<'rental, Rental=$($from_ty)*>,
				U__: Rental<'rental, Owner=<T__ as Rental<'rental>>::Owner, Rental=$($into_ty)*>,
				F__: for<'f__: 'rental> FnOnce(rental_rebind__!('f__ $($from_ty)*)) -> Result<rental_rebind__!('f__ $($into_ty)*), (E__, rental_rebind__!('f__ $($from_ty)*))>,
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
		@ITEM pub rental $rental:ident<'rental $(, $param:tt)*>($($body:tt)*); $($rest:tt)*
	} => {
		rental!{@ITEM pub rental $rental<'rental $(, $param)*> where [] ($($body)*); $($rest)*}
	};
	{
		@ITEM pub mapper $mapper:ident<'rental $(, $param:tt)*> ($($from_ty:tt)*) -> ($($into_ty:tt)*); $($rest:tt)*
	} => {
		rental!{@ITEM pub mapper $mapper<'rental $(, $param)*> ($($from_ty)*) -> ($($into_ty)*) where []; $($rest)*}
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


use std::ops::{Deref, DerefMut};
use std::{cell, rc, sync};

/// This trait indicates both that the type can be dereferenced, and that when
/// it is, the target has a fixed memory address while it is held by a rental
/// struct. This trait is already implemented for common standard types that
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


/// This trait is implement for all generated rental types. It contains within
/// everything that is possible to express without HKT. The most important
/// methods can't be expressed here, but can be seen on the predefined
/// [`RentRef`](struct.RentRef.html) and [`RentMut`](struct.RentMut.html)
/// structs.
pub unsafe trait Rental<'rental> {
	type Owner: FixedDeref + 'rental;
	type Rental: 'rental;

	/// This returns to you the rented value outside of an existential closure.
	/// This is unsafe because the lifetime substituted for `'rental` here is a
	/// lie and does not reflect the true lifetime of the value. Only use this
	/// if you have no alternative, and think very carefully about how you're
	/// using the value to prevent it from outliving the rental struct, or
	/// inserting into it data that will not live long enough.
	unsafe fn rental(&self) -> &<Self as Rental<'rental>>::Rental;

	/// This will produce a rental struct from compnent parts. Not unsafe
	/// because, if you already have the components safely, combining them in
	/// this way does not introduce any additional unsafety.
	fn from_parts(<Self as Rental<'rental>>::Owner, <Self as Rental<'rental>>::Rental) -> Self;

	/// This will decompose a rental struct into its component parts. This is
	/// obviously unsafe because one may drop the owner while retaining the
	/// borrow.
	unsafe fn into_parts(self) -> (<Self as Rental<'rental>>::Owner, <Self as Rental<'rental>>::Rental);

	/// This sill consume a rental struct and return to you the owner,
	/// discarding the rented value.
	fn into_owner(self) -> <Self as Rental<'rental>>::Owner;
}


/// This trait is implemented for all mutable rental structs.
pub unsafe trait RentalMut<'rental>: Rental<'rental> {
	/// This returns to you the rented value outside of an existential closure.
	/// This is unsafe because the lifetime substituted for `'rental` here is a
	/// lie and does not reflect the true lifetime of the value. Only use this
	/// if you have no alternative, and think very carefully about how you're
	/// using the value to prevent it from outliving the rental struct, or
	/// inserting into it data that will not live long enough.
	unsafe fn rental_mut(&mut self) -> &mut <Self as Rental<'rental>>::Rental;
}


#[doc(hidden)]
pub unsafe trait RentalDeref<'rental> {
	type Target: ?Sized;

	unsafe fn rental_target<'s>(&'s self) -> &<Self as RentalDeref<'rental>>::Target where 'rental: 's;
}


#[doc(hidden)]
pub unsafe trait RentalDerefMut<'rental>: RentalDeref<'rental> {
	unsafe fn rental_target_mut<'s>(&'s mut self) -> &mut <Self as RentalDeref<'rental>>::Target where 'rental: 's;
}


#[doc(hidden)]
pub unsafe trait RentalDerefEq<'rental, U: RentalDeref<'rental>> { }


unsafe impl<'rental, T> RentalDeref<'rental> for T where
	T: Rental<'rental>, <T as Rental<'rental>>::Rental: Deref
{
	type Target = <<T as Rental<'rental>>::Rental as Deref>::Target;

	#[inline(always)]
	unsafe fn rental_target<'s>(&'s self) -> &<T as RentalDeref<'rental>>::Target where 'rental: 's { &**self.rental() }
}


unsafe impl<'rental, T> RentalDerefMut<'rental> for T where
	T: RentalMut<'rental> + RentalDeref<'rental, Target=<<T as Rental<'rental>>::Rental as Deref>::Target>,
	<T as Rental<'rental>>::Rental: DerefMut,
{
	#[inline(always)]
	unsafe fn rental_target_mut<'s>(&'s mut self) -> &mut <T as RentalDeref<'rental>>::Target where 'rental: 's { &mut **self.rental_mut() }
}


unsafe impl<'t, 'u, T, U> RentalDerefEq<'u, U> for T where
	T: RentalDeref<'t>,
	U: RentalDeref<'u, Target=<T as RentalDeref<'t>>::Target>,
{ }


rental! {
	mod premade {
		pub rental RentRef<'rental, T, B> where [T: FixedDeref + 'rental, B: ?Sized + 'rental] (T, &'rental B);
		pub rental RentMut<'rental, T, B> where [T: FixedDeref + DerefMut + 'rental, B: ?Sized + 'rental] (mut T, &'rental mut B);
		pub mapper MapRef<'rental, T, U>(&'rental T) -> (&'rental U) where [T: 'rental, U: ?Sized + 'rental];
		pub mapper MapMut<'rental, T, U>(&'rental mut T) -> (&'rental mut U) where [T: 'rental, U: ?Sized + 'rental];
	}
}


pub use premade::*;


/// A predefined type that rents values from an `Arc<T>`.
/// # Examples
/// ```rust
/// let arc = std::sync::Arc::new(1);
/// rental::RentArc::new(arc, |a| &*a);
/// ```
pub type RentArc<'rental, T: 'rental, B: 'rental> = RentRef<'rental, sync::Arc<T>, B>;

/// A predefined type that rents values from a `Box<T>`.
/// # Examples
/// ```rust
/// let bx = Box::new(2);
/// rental::RentBox::new(bx, |b| &*b);
/// ```
pub type RentBox<'rental, T: 'rental, B: 'rental> = RentRef<'rental, Box<T>, B>;

/// A predefined type that rents mutable values from a `Box<T>`.
/// # Examples
/// ```rust
/// let mut bx = Box::new(3);
/// rental::RentBoxMut::new(bx, |b| &mut *b);
/// ```
pub type RentBoxMut<'rental, T: 'rental, B: 'rental> = RentMut<'rental, Box<T>, B>;

/// A predefined type that rents values from a `MutexGuard<T>`.
/// # Examples
/// ```rust
/// let mutex = std::sync::Mutex::new(4);
/// let guard = mutex.lock().unwrap();
/// rental::RentMutex::new(guard, |g| &*g);
/// ```
pub type RentMutex<'rental, T: 'rental, B: 'rental> = RentRef<'rental, sync::MutexGuard<'rental, T>, B>;

/// A predefined type that rents mutable values from a `MutexGuard<T>`.
/// # Examples
/// ```rust
/// let mutex = std::sync::Mutex::new(5);
/// let mut guard = mutex.lock().unwrap();
/// rental::RentMutexMut::new(guard, |g| &mut *g);
/// ```
pub type RentMutexMut<'rental, T: 'rental, B: 'rental> = RentMut<'rental, sync::MutexGuard<'rental, T>, B>;

/// A predefined type that rents values from a `Ref<T>`.
/// # Examples
/// ```rust
/// let cell = std::cell::RefCell::new(6);
/// let r = cell.borrow();
/// rental::RentRefCell::new(r, |r| &*r);
/// ```
pub type RentRefCell<'rental, T: 'rental, B: 'rental> = RentRef<'rental, cell::Ref<'rental, T>, B>;

/// A predefined type that rents mutable values from a `RefMut<T>`.
/// # Examples
/// ```rust
/// let cell = std::cell::RefCell::new(7);
/// let r = cell.borrow_mut();
/// rental::RentRefCellMut::new(r, |r| &mut *r);
/// ```
pub type RentRefCellMut<'rental, T: 'rental, B: 'rental> = RentMut<'rental, cell::RefMut<'rental, T>, B>;

/// A predefined type that rents values from an `RwLockReadGuard<T>`.
/// # Examples
/// ```rust
/// let rw = std::sync::RwLock::new(8);
/// let read = rw.read().unwrap();
/// rental::RentRwLock::new(read, |r| &*r);
/// ```
pub type RentRwLock<'rental, T: 'rental, B: 'rental> = RentRef<'rental, sync::RwLockReadGuard<'rental, T>, B>;

/// A predefined type that rents mutable values from an `RwLockWriteGuard<T>`.
/// # Examples
/// ```rust
/// let rw = std::sync::RwLock::new(9);
/// let write = rw.write().unwrap();
/// rental::RentRwLockMut::new(write, |w| &mut *w);
/// ```
pub type RentRwLockMut<'rental, T: 'rental, B: 'rental> = RentMut<'rental, sync::RwLockWriteGuard<'rental, T>, B>;

/// A predefined type that rents values from a `String`.
/// # Examples
/// ```rust
/// let s = "Hello, world!".to_string();
/// rental::RentString::new(s, |s| &s[0..5]);
/// ```
pub type RentString<'rental, B: 'rental> = RentRef<'rental, String, B>;

/// A predefined type that rents mutable values from a `String`.
/// # Examples
/// ```rust
/// let mut s = "Hello, world!".to_string();
/// rental::RentStringMut::new(s, |s| &mut s[0..5]);
/// ```
pub type RentStringMut<'rental, B: 'rental> = RentMut<'rental, String, B>;

/// A predefined type that rents values from a `Vec<T>`.
/// # Examples
/// ```rust
/// let v = vec![1, 2, 3];
/// rental::RentVec::new(v, |v| &v[0..2]);
/// ```
pub type RentVec<'rental, T: 'rental, B: 'rental> = RentRef<'rental, Vec<T>, B>;

/// A predefined type that rents mutable values from a `Vec<T>`.
/// # Examples
/// ```rust
/// let mut v = vec![1, 2, 3];
/// rental::RentVecMut::new(v, |v| &mut v[0..2]);
/// ```
pub type RentVecMut<'rental, T: 'rental, B: 'rental> = RentMut<'rental, Vec<T>, B>;


#[cfg(test)]
mod test {
	use std::ops::{Deref, DerefMut};


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


	impl<T> Foo<T> {
		pub fn borrow(&self) -> FooBorrow<T> { FooBorrow{val: &self.val, tag: 1} }
		pub fn borrow_mut(&mut self) -> FooBorrowMut<T> { FooBorrowMut{val: &mut self.val, tag: 2} }
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


	rental!{
		mod test {
			pub rental Foo<'rental, T> where [T: 'rental] (Box<super::Foo<T>>, super::FooBorrow<'rental, T>);
			pub rental FooMut<'rental, T> where [T: 'rental] (mut Box<super::Foo<T>>, super::FooBorrowMut<'rental, T>);
			pub mapper MapFoo<'rental, T> (super::FooBorrow<'rental, T>) -> (super::FooBorrow<'rental, T>) where [T: 'rental];
			pub mapper MapFooMut<'rental, T> (super::FooBorrowMut<'rental, T>) -> (super::FooBorrowMut<'rental, T>) where [T: 'rental];
		}
	}


	#[test]
	fn new() {
		test::Foo::new(Box::new(Foo{val: 5}), |f| f.borrow());
	}


	#[test]
	fn new_mut() {
		test::FooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());
	}


	#[test]
	fn rent() {
		let foo = test::Foo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		assert_eq!(foo.rent(|fb| **fb), 5);
	}


	#[test]
	fn rent_mut() {
		let mut foo = test::FooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());
		foo.rent_mut(|fbm| (*fbm.val) = 12);
		assert_eq!(foo.rent(|fbm| **fbm), 12);
	}


	#[test]
	fn rent_borrow() {
		let foo = test::Foo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		let ft = foo.rent(|fb| &fb.tag);
		assert_eq!(*ft, 1);
	}


	#[test]
	fn rent_borrow_mut() {
		let mut foo = test::FooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());
		let ft = foo.rent_mut(|fbm| &mut fbm.tag);
		*ft = 3;
		assert_eq!(*ft, 3);
	}


	#[test]
	fn deref() {
		let foo = test::Foo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		assert_eq!(*foo, 5);
	}


	#[test]
	fn deref_mut() {
		let mut foo_mut = test::FooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());
		*foo_mut = 12;
		assert_eq!(*foo_mut, 12);
	}


	#[test]
	fn map() {
		let mut foo = test::Foo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		foo = test::MapFoo::map(foo, |b| b.frob());
		assert_eq!(*foo, 5);

		let mut foo_mut = test::FooMut::new(Box::new(Foo{val: 12}), |f| f.borrow_mut());
		foo_mut = test::MapFooMut::map(foo_mut, |b| b.frob());
		assert_eq!(*foo_mut, 12);
	}


	#[test]
	fn try_map() {
		let mut foo = test::Foo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		foo = match test::MapFoo::try_map(foo, |b| b.try_frob()) {
			Ok(f) => f,
			Err((_, _)) => panic!(),
		};
		assert_eq!(*foo, 5);

		let mut _foo_mut = test::FooMut::new(Box::new(Foo{val: 12}), |f| f.borrow_mut());
		_foo_mut = match test::MapFooMut::try_map(_foo_mut, |b| b.fail_frob()) {
			Ok(f) => f,
			Err((e, f)) => {
				assert_eq!(e, "Error");
				assert_eq!(*f, 12);
				return;
			},
		};

		panic!();
	}
}
