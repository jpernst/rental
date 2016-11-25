#[macro_export]
macro_rules! rental{
	{
		mod $rent_mod:ident {
			$($items:tt)*
		}
	} => {
		mod $rent_mod {
			use ::std::ops::{Deref, DerefMut};
			use ::std::ops::Drop;
			use ::std::result::Result;
			use ::std::mem;

			rental!{@ITEMS $($items)*}
		}
	};


	{
		@ITEMS pub struct $rent:ident<'owner $(, $param:tt)*> [$($clause:tt)*] (
			mut $owner_ty:ty,
			$rental_ty:ty$(,)*
		);
		$($rest:tt)*
	} => {
		pub struct $rent<'owner $(, $param)*> $($clause)* {
			owner: Option<$owner_ty>,
			rental: Option<$rental_ty>,
		}


		impl<'rent $(, $param)*> $rent<'rent $(, $param)*> {
			#[allow(dead_code)]
			fn static_assert_valid_owner_ty() {
				$crate::static_assert_fixed_deref_mut::<$owner_ty>();
			}


			#[allow(dead_code)]
			pub fn new<F: for<'owner> FnOnce(&'owner mut <$owner_ty as Deref>::Target) -> $rental_ty>(mut owner: $owner_ty, f: F)
				-> $rent<'static $(, $param)*>
			{
				$rent{
					rental: unsafe {
						let ptr: *mut _ = &mut *owner;
						Some(mem::transmute(f(&mut *ptr)))
					},
					owner: Some(owner),
				}
			}


			#[allow(dead_code)]
			pub fn try_new<E, F: for<'owner> FnOnce(&'owner mut <$owner_ty as Deref>::Target) -> Result<$rental_ty, E>>(mut owner: $owner_ty, f: F)
				-> Result<$rent<'static $(, $param)*>, ($owner_ty, E)>
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


			#[allow(dead_code)]
			pub fn rent<'s, F, R>(&'s self, f: F) -> R
				where F: for<'owner: 's> FnOnce(&'s $rental_ty) -> R, R: 's
			{
				f(self.rental.as_ref().unwrap())
			}


			#[allow(dead_code)]
			pub fn rent_mut<'s, F, R>(&'s mut self, f: F) -> R
				where F: for<'owner: 's> FnOnce(&'s mut $rental_ty) -> R, R: 's
			{
				f(self.rental.as_mut().unwrap())
			}


			#[allow(dead_code)]
			pub fn into_owner(mut self) -> $owner_ty {
				self.owner.take().unwrap()
			}
		}


		impl<'rent $(, $param)*> Drop for $rent<'rent $(, $param)*> {
			fn drop(&mut self) {
				mem::drop(self.rental.take());
				mem::drop(self.owner.take());
			}
		}


		rental!{@ITEMS $($rest)*}
	};
	{
		@ITEMS pub struct $rent:ident<'owner $(, $param:tt)*> [$($clause:tt)*] (
			$owner_ty:ty,
			$rental_ty:ty$(,)*
		);
		$($rest:tt)*
	} => {
		pub struct $rent<'owner $(, $param)*> $($clause)* {
			owner: Option<$owner_ty>,
			rental: Option<$rental_ty>,
		}


		impl<'rent $(, $param)*> $rent<'rent $(, $param)*> {
			#[allow(dead_code)]
			fn static_assert_valid_owner_ty() {
				$crate::static_assert_fixed_deref::<$owner_ty>();
			}


			#[allow(dead_code)]
			pub fn new<F: for<'owner> FnOnce(&'owner <$owner_ty as Deref>::Target) -> $rental_ty>(owner: $owner_ty, f: F)
				-> $rent<'static $(, $param)*>
			{
				$rent{
					rental: unsafe {
						let ptr: *const _ = &*owner;
						Some(mem::transmute(f(&*ptr)))
					},
					owner: Some(owner),
				}
			}


			#[allow(dead_code)]
			pub fn try_new<E, F: for<'owner> FnOnce(&'owner <$owner_ty as Deref>::Target) -> Result<$rental_ty, E>>(owner: $owner_ty, f: F)
				-> Result<$rent<'static $(, $param)*>, ($owner_ty, E)>
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


			#[allow(dead_code)]
			pub fn owner(&self) -> &$owner_ty {
				self.owner.as_ref().unwrap()
			}


			#[allow(dead_code)]
			pub fn rent<'s, F, R>(&'s self, f: F) -> R
				where F: for<'owner: 's> FnOnce(&'s $rental_ty) -> R, R: 's
			{
				f(self.rental.as_ref().unwrap())
			}


			#[allow(dead_code)]
			pub fn into_owner(mut self) -> $owner_ty {
				self.owner.take().unwrap()
			}
		}


		impl<'rent $(, $param)*> Drop for $rent<'rent $(, $param)*> {
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
		rental!{@$mode pub struct $rent<'owner $(, $param)*> [] ($($body)*); $($rest)*}
	};
}


#[allow(dead_code)]
#[inline(always)]
fn static_assert_fixed_deref<O: FixedDeref>() { }
#[allow(dead_code)]
#[inline(always)]
fn static_assert_fixed_deref_mut<O: FixedDeref + ::std::ops::DerefMut>() { }


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


#[cfg(test)]
mod test {
	pub struct Foo<T> {
		val: T,
	}

	impl<T> Foo<T> {
		pub fn borrow(&self) -> FooBorrow<T> { FooBorrow{val: &self.val, tag: 5} }
		pub fn borrow_mut(&mut self) -> FooBorrowMut<T> { FooBorrowMut{val: &mut self.val, tag: 12} }
	}

	pub struct FooBorrow<'f, T: 'f> {
		val: &'f T,
		tag: i32,
	}

	pub struct FooBorrowMut<'f, T: 'f> {
		val: &'f mut T,
		tag: i32,
	}


	rental!{
		mod rental {
			pub struct Foo<'owner, T> [where T: 'owner] (Box<super::Foo<T>>, super::FooBorrow<'owner, T>);
			pub struct FooMut<'owner, T> [where T: 'owner] (mut Box<super::Foo<T>>, super::FooBorrowMut<'owner, T>);
		}
	}


//	#[test]
//	fn instantiate() {
//		let _rent = rental::RentRef::new(Box::new(RefCell::new(5)), |r| Box::new(r.borrow()));
//		let _rent_mut = rental::RentRefMut::new(Box::new(RefCell::new(12)), |r| Box::new(r.borrow_mut()));
//	}


	#[test]
	fn unsound() {
//		let mut _unsound = rental::Unsound::new(Box::new(Foo{val: 5}), |f| f.borrow());
//		let b = _unsound.rent_mut(|f| &mut f.tag);

//		let mut freed = {
//			let (mut unsound, fake) = (rental::Unsound::new(Box::new(RefCell::new(5)), |r| r.borrow()), RefCell::new(12));
//			{
//				let r = unsound.rental_mut();
//				*r = fake.borrow();
//			}
//			unsound
//		};
//
//		println!("Freed: {}", **freed.rental_mut());
	}
}
