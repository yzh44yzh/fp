
In Haskell you cannot make decisions based on the type of an argument,
only on the value of the argument.  This implies that Haskell is
*parametric*.

It makes type *erasure possible*. Type erasure means that no information
about the type of values is present at run-time. The compiler erases
the types from your program as it compiles it to machine code.
