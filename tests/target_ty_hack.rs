#[macro_use]
extern crate rental;


type MyVec<T> = Vec<T>;


rental! {
	pub mod rent_vec_slice {
		use super::*;

		#[rental(deref_suffix)]
		pub struct OwnedSlice {
			#[target_ty_hack = "[u8]"]
			buffer: MyVec<u8>,
			slice: &'buffer [u8],
		}

		#[rental_mut(deref_mut_suffix)]
		pub struct OwnedMutSlice {
			#[target_ty_hack = "[u8]"]
			buffer: MyVec<u8>,
			slice: &'buffer mut [u8],
		}
	}
}


#[test]
fn new() {
	let vec = vec![1, 2, 3];
	let _ = rent_vec_slice::OwnedSlice::new(vec, |slice| slice);
}


#[test]
fn read() {
	let vec = vec![1, 2, 3];
	let rvec = rent_vec_slice::OwnedSlice::new(vec, |slice| slice);

	assert_eq!(rvec.rent(|slice| slice[1]), 2);
	assert_eq!(rvec.rent(|slice| slice[1]), rvec[1]);
}


#[test]
fn write() {
	let vec = vec![1, 2, 3];
	let mut rvec = rent_vec_slice::OwnedMutSlice::new(vec, |slice| slice);

	rvec[1] = 4;
	assert_eq!(rvec.rent(|slice| slice[1]), 4);
	assert_eq!(rvec.rent(|slice| slice[1]), rvec[1]);
}
