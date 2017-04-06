#[macro_use]
extern crate rental;


rental! {
	pub mod rent_string {
		#[rental(deref_suffix)]
		pub struct OwnedStr {
			buffer: String,
			slice: &'buffer str,
		}
	}
}


#[test]
fn new() {
	let buf = "Hello, World!".to_string();
	let _ = rent_string::OwnedStr::new(buf, |slice| slice);
}


#[test]
fn read() {
	let buf = "Hello, World!".to_string();
	let rbuf = rent_string::OwnedStr::new(buf, |slice| slice);

	assert_eq!(&*rbuf, "Hello, World!");
}
