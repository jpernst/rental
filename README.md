# Macro to generate self-borrowing structs

[Documentation](http://docs.rs/rental)

Rental is a crate designed to handle generalized cases where a struct needs to borrow its own contents. This can arise as an implementation detail in an API that you don't want to expose to your users by making them handle the borrowed lifetime. Instead, you can just present a single, owned struct that they can move around as normal. To achieve this, the rental macro is used to define new types that represent pairs of an owner and a borrow associated with that owner. These can then be handled as a single unit. Naturally this is very unsafe by nature, so great care is taken to make sure no borrows can escape that will outlive the struct, or that short-lived data will be squirrelled into the struct.

One motivating case for me was being able to store a [`libloading`](https://crates.io/crates/libloading) `Library` and the `Symbol`s loaded from it in a single `Api` struct. Under normal circumstances this would be impossible, but rental allows you to define a type that can do it safely (well, aside from the natural unsafety of calling symbols loaded from a dynamic library). Declaring that type looks as follows:

```rust
rental!{
    mod rent_lib {
        pub rental RentSym<'rental, S: ['rental]>(::std::sync::Arc<::libloading::Library>, ::libloading::Symbol<'rental, S>): Deref(S);
    }
}
```

This defines a new struct that can hold a `Library`, and a `Symbol` loaded from it. The `'rental` lifetime is handled specially and represents the shared connection between the owner and the borrow.

To create an instance:

```rust
RentSym::try_new(lib, |l| unsafe { l.get(b"my_symbol") })
```

NOTE: The `unsafe` here is solely because loading a symbol from the dylib is unsafe, unrelated to this library, I promise :)

Inside this closure, the `'rental` lifetime becomes "existential" and cannot be satisfied by anything outside the closure, preventing anything tied to it from being exfiltrated. The `Deref(S)` attached to the struct definition means that the struct will deref to the `S` type of the Symbol. This is safe only because the `'rental` lifetime does NOT appear in the type signature of `S`. If it did, the derefed lifetime would be a lie and unsafety would ensue. The macro takes steps to ensure the type is compliant and will prevent `Deref` from being impld if it fails. In such cases, the borrowed value can instead by accessed with the `rent` method that gives you an existentially bound reference to the inner data that you can manipulate safely and return anything from (as long as it does not include the `'rental` lifetime).

This crate also provides predefined types `RentRef` and `RentMut` for the simple case where you only need to store a borrowed reference along with its owner, and serve as examples for the documentation.
