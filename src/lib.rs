#[macro_use]
extern crate procedural_masquerade;
#[macro_use]
extern crate rental_impl;

pub use rental_impl::*;


define_invoke_proc_macro!(rental__invoke_proc_macro);


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
			rental__invoke_proc_macro!{
				rental__impl!($($body)*)
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
			rental__invoke_proc_macro!{
				rental__impl!($($body)*)
			}
		}
	};
}


rental!{
	mod rental_mod {
		#[rental]
		pub struct Foo(
			i32
		);
	}
}
