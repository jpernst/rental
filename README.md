# Macro to generate self-borrowing structs

[Documentation](http://docs.rs/rental)

It can sometimes occur in the course of designing an API that you find yourself in a situation where you need to store, in a single struct, both an owned value and a borrow of that value. Rust's concept of ownership and borrowing is quite flexible, but can't quite express such a scenario.

This crate uses the term "rental" to describe this concept of a borrow that co-exsists with its owner in the same struct. The borrow itself is called the "rented" type. The "owner", naturally, is the item in the struct that owns the borrow.

The API consists of the `rental` macro, which generates rental structs, and a few premade instantiations of this macro handling rented bare references.  If you only need to rent references, see `RentRef` and `RentMut`, as well as the associated type aliases for common rental scenarios. The documentation for the `rental` macro describes the kinds of items that can be generated. 

One example might be `libloading`. That crate provides a `Library` struct that defines methods to borrow `Symbol`s from it. These symbols are bounded by the lifetime of the library, and are thus considered a borrow. Under normal circumstances, one would be unable to store both the library and the symbols within a single struct, but the macro defined in this crate allows you to define a struct that is capable of storing both simultaneously.

Such a struct can be declared like this:

```rust
rental!{
    mod rent_lib {
        pub rental RentSym<'rental, S: ['rental]>(::std::sync::Arc<::libloading::Library>, ::libloading::Symbol<'rental, S>): Deref(S);
    }
}
```

This defines a new struct that can hold an `Arc` of a `Library`, and a `Symbol` loaded from it. An `Arc` is used here so that the library can be shared with other such structs. The `'rental` lifetime is handled specially and represents the shared connection between the owner and the borrow. The lifetime `'rental` appears as the bound on the `Symbol`, because that is the lifetime that will be attached when you obtain the `Symbol` by calling `Library::get` inside the creation closure.

For a type to be eligible as an owner, it must also implement the `FixedDeref` trait. Because the rental struct can be moved freely, the internal borrow could be invalidated if it refers to data that has been moved. For this reason, The owner must be `Deref`, and furthermove must deref to a fixed memory address for the life of the rental. `FixedDeref` is an unsafe trait that reflects these requirements. Finally, after the declaration, you may optionally add `: Deref(Target)` where `Target` is the `Deref` target of the rented type. This can't be inferred, because it must be checked to ensure that it does NOT contain the `'rental` lifetime anywhere in its signature. If it does, then implementing `Deref` would be unsafe, and the macro will reject it.
 
To create an instance:

```rust
let sym = RentSym::try_new(lib, |lib| unsafe { lib.get::<fn()>(b"my_symbol") })
```

NOTE: The `unsafe` here is solely because loading a symbol from the dylib is unsafe, unrelated to this library, I promise :)

Inside this closure, the `'rental` lifetime becomes "existential" and cannot be satisfied by anything outside the closure, preventing anything tied to it from being exfiltrated. The function held by the struct can then be called by simply:

```rust
sym();
```

In this way, the symbol can be carried around and called as a single unit, but retains within it the library that it depends on. The user is thus relieved of the concern of keeping the library alive, or even knowing that it exists at all.

In cases where the rented valued is not `Deref` or does not meet the requirements for a safe `Deref` impl as described earlier, you can access the internal value with the `rent` method, which will pass to you an existentially bound reference to the rented value. The example above could also be written like this:

```rust
sym.rent(|s| s());
```
