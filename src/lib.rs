#![feature(cell_extras)]


#[macro_export]
macro_rules! rental{
	{
		mod $rent_mod:ident {
			$($items:tt)*
		}
	} => {
		mod $rent_mod {
			mod rent_impl {
				use ::std::ops::{Deref, DerefMut};
				use ::std::ops::Drop;
				use ::std::result::Result;
				use ::std::mem;


				rental!{@ITEMS $($items)*}
			}


			rental!{@ALIASES $($items)*}
		}
	};


	{
		@ITEMS pub struct $rent:ident<$([$lt:tt]),*$(,)* $($param:ident),*> [$($clause:tt)*] (
			mut $owner_ty:ty,
			$rental_ty:ty$(,)*
		);


		$($rest:tt)*
	} => {
		pub struct $rent<'owner $(, $lt)*, O: $crate::FixedDeref + DerefMut $(, $param)*> $($clause)* {
			owner: Option<O>,
			rental: Option<$rental_ty>,
		}


		impl<$($lt, )* $($param, )*> $rent<'static $(, $lt)*, $owner_ty $(, $param)*> {
			#[allow(non_camel_case_types)]
			pub fn new<F: for<'owner> FnOnce(&'owner mut <$owner_ty as Deref>::Target) -> $rental_ty>(mut owner: $owner_ty, f: F)
				-> $rent<'static $(, $lt)*, $owner_ty $(, $param)*>
			{
				$rent{
					rental: unsafe {
						let ptr: *mut _ = &mut *owner;
						Some(mem::transmute(f(&mut *ptr)))
					},
					owner: Some(owner),
				}
			}


			#[allow(non_camel_case_types)]
			pub fn try_new<E, F: for<'owner> FnOnce(&'owner mut <$owner_ty as Deref>::Target) -> Result<$rental_ty, E>>(mut owner: $owner_ty, f: F)
				-> Result<$rent<'static $(, $lt)*, $owner_ty $(, $param)*>, ($owner_ty, E)>
			{
				Ok($rent{
					rental: unsafe {
						let ptr: *mut _ = &mut *owner;
						match f(&mut *ptr) {
							Ok(asset) => Some(mem::transmute(asset)),
							Err(err) => return Err((owner, err)),
						}
					},
					owner: Some(owner),
				})
			}


			pub fn rental<'owner>(&'owner self) -> &'owner $rental_ty {
				self.rental.as_ref().unwrap()
			}


			pub fn rental_mut<'owner>(&'owner mut self) -> &'owner mut $rental_ty {
				unsafe { mem::transmute(self.rental.as_mut().unwrap()) }
			}


			pub fn into_owner(mut self) -> $owner_ty {
				self.owner.take().unwrap()
			}
		}


		impl<'owner $(, $lt)*, O: $crate::FixedDeref + DerefMut $(, $param)*> Drop for $rent<'owner $(, $lt)*, O $(, $param)*> {
			fn drop(&mut self) {
				mem::drop(self.rental.take());
				mem::drop(self.owner.take());
			}
		}


		rental!{@ITEMS $($rest)*}
	};
	{
		@ITEMS pub struct $rent:ident<$([$lt:tt]),*$(,)* $($param:ident),*> [$($clause:tt)*] (
			$owner_ty:ty,
			$rental_ty:ty$(,)*
		);


		$($rest:tt)*
	} => {
		pub struct $rent<'owner $(, $lt)*, O: $crate::FixedDeref $(, $param)*> $($clause)* {
			owner: Option<O>,
			rental: Option<$rental_ty>,
		}


		impl<$($lt, )* $($param, )*> $rent<'static $(, $lt)*, $owner_ty $(, $param)*> {
			#[allow(non_camel_case_types)]
			pub fn new<F: for<'owner> FnOnce(&'owner <$owner_ty as Deref>::Target) -> $rental_ty>(owner: $owner_ty, f: F)
				-> $rent<'static $(, $lt)*, $owner_ty $(, $param)*>
			{
				$rent{
					rental: unsafe {
						let ptr: *const _ = &*owner;
						Some(mem::transmute(f(&*ptr)))
					},
					owner: Some(owner),
				}
			}


			#[allow(non_camel_case_types)]
			pub fn try_new<E, F: for<'owner> FnOnce(&'owner <$owner_ty as Deref>::Target) -> Result<$rental_ty, E>>(owner: $owner_ty, f: F)
				-> Result<$rent<'static $(, $lt)*, $owner_ty $(, $param)*>, ($owner_ty, E)>
			{
				Ok($rent{
					rental: unsafe {
						let ptr: *const _ = &*owner;
						match f(&*ptr) {
							Ok(asset) => Some(mem::transmute(asset)),
							Err(err) => return Err((owner, err)),
						}
					},
					owner: Some(owner),
				})
			}


			pub fn owner(&self) -> &$owner_ty {
				self.owner.as_ref().unwrap()
			}


			pub fn rental<'owner>(&'owner self) -> &'owner $rental_ty {
				self.rental.as_ref().unwrap()
			}


			pub fn into_owner(mut self) -> $owner_ty {
				self.owner.take().unwrap()
			}
		}


		impl<'owner $(, $lt)*, O: $crate::FixedDeref $(, $param)*> Drop for $rent<'owner $(, $lt)*, O $(, $param)*> {
			fn drop(&mut self) {
				mem::drop(self.rental.take());
				mem::drop(self.owner.take());
			}
		}


		rental!{@ITEMS $($rest)*}
	};
	{ @ITEMS } => { };


	{
		@ALIASES pub struct $rent:ident<$([$lt:tt]),*$(,)* $($param:ident),*> [$($clause:tt)*] (
			mut $owner_ty:ty,
			$rental_ty:ty$(,)*
		);


		$($rest:tt)*
	} => {
		pub type $rent<$($lt, )* $($param, )*> = rent_impl::$rent<'static $(, $lt)*, $owner_ty $(, $param)*>;
		rental!{@ALIASES $($rest)*}
	};
	{
		@ALIASES pub struct $rent:ident<$([$lt:tt]),*$(,)* $($param:ident),*> [$($clause:tt)*] (
			$owner_ty:ty,
			$rental_ty:ty$(,)*
		);


		$($rest:tt)*
	} => {
		pub type $rent<$($lt, )* $($param, )*> = rent_impl::$rent<'static $(, $lt)*, $owner_ty $(, $param)*>;
		rental!{@ALIASES $($rest)*}
	};
	{ @ALIASES } => { };
}


rental!{
	mod rent_refs {
		pub struct RentRef<T> [where T: 'static] (Box<::std::cell::RefCell<T>>, Box<::std::cell::Ref<'owner, T>>);
		pub struct RentRefMut<T> [where T: 'static] (mut Box<::std::cell::RefCell<T>>, Box<::std::cell::RefMut<'owner, T>>);
	}
}


pub unsafe trait FixedDeref: ::std::ops::Deref { }

unsafe impl<'t, T: ?Sized> FixedDeref for &'t T { }
unsafe impl<'t, T: ?Sized> FixedDeref for &'t mut T { }

unsafe impl<T: ?Sized> FixedDeref for Box<T> { }
unsafe impl<T> FixedDeref for Vec<T> { }
unsafe impl FixedDeref for String { }

unsafe impl<T: ?Sized> FixedDeref for ::std::rc::Rc<T> { }
unsafe impl<T: ?Sized> FixedDeref for ::std::sync::Arc<T> { }

unsafe impl<'t, T: ?Sized> FixedDeref for ::std::cell::Ref<'t, T> { }
unsafe impl<'t, T: ?Sized> FixedDeref for ::std::cell::RefMut<'t, T> { }
unsafe impl<'t, T: ?Sized> FixedDeref for ::std::sync::MutexGuard<'t, T> { }
unsafe impl<'t, T: ?Sized> FixedDeref for ::std::sync::RwLockReadGuard<'t, T> { }
unsafe impl<'t, T: ?Sized> FixedDeref for ::std::sync::RwLockWriteGuard<'t, T> { }
