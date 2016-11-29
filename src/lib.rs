#[macro_export]
macro_rules! rental{
	{
		mod $rent_mod:ident {
			$($items:tt)*
		}
	} => {
		mod $rent_mod {
			#![allow(unused_imports)]
			use ::std::ops::{Deref, DerefMut};
			use ::std::ops::Drop;
			use ::std::result::Result;
			use ::std::mem;

			use $crate::{FixedDeref, Rental, RentalMut, RentalDeref, RentalDerefMut, RentalDerefEq};

			rental!{@ITEMS $($items)*}
		}
	};


	{
		@ITEMS pub struct $rent:ident<'owner $(, $param:tt)*> where [$($clause:tt)*] (
			mut $owner_ty:ty,
			$rental_ty:ty$(,)*
		);
		$($rest:tt)*
	} => {
		pub struct $rent<'owner $(, $param)*> where
			$owner_ty: FixedDeref + DerefMut, $($clause)*
		{
			owner: Option<$owner_ty>,
			rental: Option<$rental_ty>,
		}


		impl<'rent $(, $param)*> $rent<'rent $(, $param)*> where
			$owner_ty: FixedDeref + DerefMut
		{
			#[allow(dead_code)]
			pub fn new<F: for<'owner> FnOnce(&'owner mut <$owner_ty as Deref>::Target) -> $rental_ty>(mut owner: $owner_ty, f: F)
				-> $rent<'rent $(, $param)*>
			{
				$rent{
					rental: unsafe {
						Some(mem::transmute(f(&mut *<$owner_ty as DerefMut>::deref_mut(&mut owner))))
					},
					owner: Some(owner),
				}
			}


			#[allow(dead_code)]
			pub fn try_new<E, F: for<'owner> FnOnce(&'owner mut <$owner_ty as Deref>::Target) -> Result<$rental_ty, E>>(mut owner: $owner_ty, f: F)
				-> Result<$rent<'rent $(, $param)*>, ($owner_ty, E)>
			{
				Ok($rent{
					rental: unsafe {
						let ptr: *mut _ = &mut *<$owner_ty as DerefMut>::deref_mut(&mut owner);
						match f(&mut *ptr) {
							Ok(asset) => Some(mem::transmute(asset)),
							Err(err) => return Err((owner, err)),
						}
					},
					owner: Some(owner),
				})
			}


			#[allow(dead_code)]
			pub fn rent<'s, F, R>(&'s self, f: F) -> R where
				F: for<'owner: 's> FnOnce(&'s $rental_ty) -> R, R: 's
			{
				f(self.rental.as_ref().unwrap())
			}


			#[allow(dead_code)]
			pub fn rent_mut<'s, F, R>(&'s mut self, f: F) -> R where
				F: for<'owner: 's> FnOnce(&'s mut $rental_ty) -> R, R: 's
			{
				f(self.rental.as_mut().unwrap())
			}
		}


		unsafe impl<'owner $(, $param)*> Rental<'owner> for $rent<'owner $(, $param)*> where
			$owner_ty: FixedDeref + DerefMut,
			$($clause)*
		{
			type Owner = $owner_ty;
			type Rental = $rental_ty;


			#[inline(always)]
			unsafe fn rental(&self) -> &$rental_ty { self.rental.as_ref().unwrap() }
			unsafe fn from_parts(owner: $owner_ty, rent: $rental_ty) -> Self { $rent{owner: Some(owner), rental: Some(rent)} }
			unsafe fn into_parts(mut self) -> ($owner_ty, $rental_ty) { (self.owner.take().unwrap(), self.rental.take().unwrap()) }
			fn into_owner(mut self) -> $owner_ty { self.owner.take().unwrap() }
		}


		unsafe impl<'owner $(, $param)*> RentalMut<'owner> for $rent<'owner $(, $param)*> where
			$owner_ty: FixedDeref + DerefMut,
			$($clause)*
		{
			#[inline(always)]
			unsafe fn rental_mut(&mut self) -> &mut <Self as Rental<'owner>>::Rental { self.rental.as_mut().unwrap() }
		}


		impl<'owner $(, $param)*> Deref for $rent<'owner $(, $param)*> where
			$owner_ty: FixedDeref + DerefMut,
			$rent<'owner $(, $param)*>: RentalDeref<'owner> + for<'rent> RentalDerefEq<'rent, $rent<'rent $(, $param)*>>,
		{
			type Target = <$rent<'owner $(, $param)*> as RentalDeref<'owner>>::Target;

			#[inline(always)]
			fn deref(&self) -> &<$rent<'owner $(, $param)*> as RentalDeref<'owner>>::Target { unsafe { self.rental_target() } }
		}


		impl<'owner $(, $param)*> DerefMut for $rent<'owner $(, $param)*> where
			$owner_ty: FixedDeref + DerefMut,
			$rent<'owner $(, $param)*>: RentalDerefMut<'owner> + for<'rent> RentalDerefEq<'rent, $rent<'rent $(, $param)*>>,
		{
			#[inline(always)]
			fn deref_mut(&mut self) -> &mut <$rent<'owner $(, $param)*> as Deref>::Target { unsafe { self.rental_target_mut() } }
		}


		impl<'rent $(, $param)*> Drop for $rent<'rent $(, $param)*> where
			$owner_ty: FixedDeref + DerefMut,
		{
			fn drop(&mut self) {
				mem::drop(self.rental.take());
				mem::drop(self.owner.take());
			}
		}


		rental!{@ITEMS $($rest)*}
	};
	{
		@ITEMS pub struct $rent:ident<'owner $(, $param:tt)*> where [$($clause:tt)*] (
			$owner_ty:ty,
			$rental_ty:ty$(,)*
		);
		$($rest:tt)*
	} => {
		pub struct $rent<'owner $(, $param)*> where
			$owner_ty: FixedDeref, $($clause)*
		{
			owner: Option<$owner_ty>,
			rental: Option<$rental_ty>,
		}


		impl<'rent $(, $param)*> $rent<'rent $(, $param)*> where
			$owner_ty: FixedDeref
		{
			#[allow(dead_code)]
			pub fn new<F: for<'owner> FnOnce(&'owner <$owner_ty as Deref>::Target) -> $rental_ty>(owner: $owner_ty, f: F)
				-> $rent<'rent $(, $param)*>
			{
				$rent{
					rental: unsafe {
						Some(mem::transmute(f(&*<$owner_ty as Deref>::deref(&owner))))
					},
					owner: Some(owner),
				}
			}


			#[allow(dead_code)]
			pub fn try_new<E, F: for<'owner> FnOnce(&'owner <$owner_ty as Deref>::Target) -> Result<$rental_ty, E>>(owner: $owner_ty, f: F)
				-> Result<$rent<'rent $(, $param)*>, ($owner_ty, E)>
			{
				Ok($rent{
					rental: unsafe {
						let ptr: *const _ = &*<$owner_ty as Deref>::deref(&owner);
						match f(&*ptr) {
							Ok(asset) => Some(mem::transmute(asset)),
							Err(err) => return Err((owner, err)),
						}
					},
					owner: Some(owner),
				})
			}


			#[allow(dead_code)]
			pub fn owner(&self) -> &$owner_ty {
				self.owner.as_ref().unwrap()
			}


			#[allow(dead_code)]
			pub fn rent<'s, F, R>(&'s self, f: F) -> R where
				F: for<'owner: 's> FnOnce(&'s $rental_ty) -> R, R: 's
			{
				f(self.rental.as_ref().unwrap())
			}
		}


		unsafe impl<'owner $(, $param)*> Rental<'owner> for $rent<'owner $(, $param)*> where
			$owner_ty: FixedDeref,
			$($clause)*
		{
			type Owner = $owner_ty;
			type Rental = $rental_ty;


			#[inline(always)]
			unsafe fn rental(&self) -> &$rental_ty { self.rental.as_ref().unwrap() }
			unsafe fn from_parts(owner: $owner_ty, rent: $rental_ty) -> Self { $rent{owner: Some(owner), rental: Some(rent)} }
			unsafe fn into_parts(mut self) -> ($owner_ty, $rental_ty) { (self.owner.take().unwrap(), self.rental.take().unwrap()) }
			fn into_owner(mut self) -> $owner_ty { self.owner.take().unwrap() }
		}


		impl<'owner $(, $param)*> Deref for $rent<'owner $(, $param)*> where
			$owner_ty: FixedDeref,
			$rent<'owner $(, $param)*>: RentalDeref<'owner> + for<'rent> RentalDerefEq<'rent, $rent<'rent $(, $param)*>>,
		{
			type Target = <$rent<'owner $(, $param)*> as RentalDeref<'owner>>::Target;

			#[inline(always)]
			fn deref(&self) -> &<$rent<'owner $(, $param)*> as RentalDeref<'owner>>::Target { unsafe { self.rental_target() } }
		}


		impl<'rent $(, $param)*> Drop for $rent<'rent $(, $param)*> where
			$owner_ty: FixedDeref
		{
			fn drop(&mut self) {
				mem::drop(self.rental.take());
				mem::drop(self.owner.take());
			}
		}


		rental!{@ITEMS $($rest)*}
	};
	{ @ITEMS } => { };


	{
		@$mode:ident pub struct $rent:ident<'owner $(, $param:tt)*>($($body:tt)*); $($rest:tt)*
	} => {
		rental!{@$mode pub struct $rent<'owner $(, $param)*> where [] ($($body)*); $($rest)*}
	};
}


use std::ops::{Deref, DerefMut};
use std::{cell, rc, sync};


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


pub unsafe trait Rental<'rent> {
	type Owner: FixedDeref + 'rent;
	type Rental: 'rent;

	unsafe fn rental(&self) -> &<Self as Rental<'rent>>::Rental;
	unsafe fn from_parts(<Self as Rental<'rent>>::Owner, <Self as Rental<'rent>>::Rental) -> Self;
	unsafe fn into_parts(self) -> (<Self as Rental<'rent>>::Owner, <Self as Rental<'rent>>::Rental);
	fn into_owner(self) -> <Self as Rental<'rent>>::Owner;
}


pub unsafe trait RentalMut<'rent>: Rental<'rent> {
	unsafe fn rental_mut(&mut self) -> &mut <Self as Rental<'rent>>::Rental;
}


pub unsafe trait RentalMap<'rent> {
	type From: 'rent,
	type Into: 'rent,
}


#[doc(hidden)]
pub unsafe trait RentalDeref<'rent> {
	type Target: ?Sized;

	unsafe fn rental_target<'s>(&'s self) -> &<Self as RentalDeref<'rent>>::Target where 'rent: 's;
}


#[doc(hidden)]
pub unsafe trait RentalDerefMut<'rent>: RentalDeref<'rent> {
	unsafe fn rental_target_mut<'s>(&'s mut self) -> &mut <Self as RentalDeref<'rent>>::Target where 'rent: 's;
}


#[doc(hidden)]
pub unsafe trait RentalDerefEq<'rent, U: RentalDeref<'rent>> { }


unsafe impl<'rent, T> RentalDeref<'rent> for T where
	T: Rental<'rent>, <T as Rental<'rent>>::Rental: Deref
{
	type Target = <<T as Rental<'rent>>::Rental as Deref>::Target;

	#[inline(always)]
	unsafe fn rental_target<'s>(&'s self) -> &<T as RentalDeref<'rent>>::Target where 'rent: 's { &**self.rental() }
}


unsafe impl<'rent, T> RentalDerefMut<'rent> for T where
	T: RentalMut<'rent> + RentalDeref<'rent, Target=<<T as Rental<'rent>>::Rental as Deref>::Target>,
	<T as Rental<'rent>>::Rental: DerefMut,
{
	#[inline(always)]
	unsafe fn rental_target_mut<'s>(&'s mut self) -> &mut <T as RentalDeref<'rent>>::Target where 'rent: 's { &mut **self.rental_mut() }
}


unsafe impl<'t, 'u, T, U> RentalDerefEq<'u, U> for T where
	T: RentalDeref<'t>,
	U: RentalDeref<'u, Target=<T as RentalDeref<'t>>::Target>,
{ }


rental! {
	mod premade {
		pub struct RentRef<'owner, T, B> where [T: FixedDeref + 'owner, B: 'owner] (T, &'owner B);
		pub struct RentRefMut<'owner, T, B> where [T: FixedDeref + DerefMut + 'owner, B: 'owner] (mut T, &'owner mut B);
	}
}


pub use premade::*;


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


	impl<'f, T> Deref for FooBorrow<'f, T> {
		type Target = T;

		fn deref(&self) -> &T { self.val }
	}


	impl<'f, T> Clone for FooBorrow<'f, T> {
		fn clone(&self) -> FooBorrow<'f, T> {
			FooBorrow{val: self.val, tag: self.tag}
		}
	}


	impl<'f, T> Deref for FooBorrowMut<'f, T> {
		type Target = T;

		fn deref(&self) -> &T { self.val }
	}


	impl<'f, T> DerefMut for FooBorrowMut<'f, T> {
		fn deref_mut(&mut self) -> &mut T { self.val }
	}


	rental!{
		mod rental {
			pub struct Foo<'owner, T> where [T: 'owner] (Box<super::Foo<T>>, super::FooBorrow<'owner, T>);
			pub struct FooMut<'owner, T> where [T: 'owner] (mut Box<super::Foo<T>>, super::FooBorrowMut<'owner, T>);
		}
	}


	#[test]
	fn new() {
		rental::Foo::new(Box::new(Foo{val: 5}), |f| f.borrow());
	}


	#[test]
	fn new_mut() {
		rental::FooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());
	}


	#[test]
	fn rent() {
		let foo = rental::Foo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		assert_eq!(foo.rent(|fb| **fb), 5);
	}


	#[test]
	fn rent_mut() {
		let mut foo = rental::FooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());
		foo.rent_mut(|fbm| (*fbm.val) = 12);
		assert_eq!(foo.rent(|fbm| **fbm), 12);
	}


	#[test]
	fn rent_borrow() {
		let foo = rental::Foo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		let ft = foo.rent(|fb| &fb.tag);
		assert_eq!(*ft, 1);
	}


	#[test]
	fn rent_borrow_mut() {
		let mut foo = rental::FooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());
		let ft = foo.rent_mut(|fbm| &mut fbm.tag);
		*ft = 3;
		assert_eq!(*ft, 3);
	}


	#[test]
	fn deref() {
		let foo = rental::Foo::new(Box::new(Foo{val: 5}), |f| f.borrow());
		assert_eq!(*foo, 5);
	}


	#[test]
	fn deref_mut() {
		let mut foo_mut = rental::FooMut::new(Box::new(Foo{val: 5}), |f| f.borrow_mut());
		*foo_mut = 12;
		assert_eq!(*foo_mut, 12);
	}
}
