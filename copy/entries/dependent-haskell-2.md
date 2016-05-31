---
title: "Practical Dependent Types in Haskell: Type-Safe Neural Networks (Part 2)"
categories: Haskell, Ramblings
series: Practical Dependent Types in Haskell
tags: functional programming, dependent types, numerical, haskell, singletons, types, linear algebra, artificial neural networks, machine learning, existential types
create-time: 2016/05/27 22:20:42
date: Never
identifier: dependent-haskell-2
slug: practical-dependent-types-in-haskell-2
---

We're back to continue on [our journey][series] in using practical dependent
types to write type-safe neural networks!  In [Part 1][], we wrote things out
in normal, untyped Haskell, and looked at red flags and general design
principles that nudged us in the direction of adding dependent types to our
program.  We learned to appreciate what dependent types offered in terms of
guiding us in writing our code, helping the compiler check our correctness,
providing a better interface for users, and more.

[series]: https://blog.jle.im/entries/series/+practical-dependent-types-in-haskell.html
[Part 1]: https://blog.jle.im/entry/practical-dependent-types-in-haskell-1.html

We also learned how to use singletons to work around some of Haskell's
fundamental limitations to let us "pattern match" on the structure of types,
and how to use typeclasses to generate singletons reflecting the structure of
types we are dealing with.

All of what we've dealt with so far has essentially been with types that are
fixed at compile-time.  All the networks we've made have had "static" types,
with their sizes in their types indicated directly in the source code.

Today, we're going to dive into the world of types that *depend* on factors
unknown until runtime, and see how dependent types in a strongly typed language
like Haskell helps us write safer, more correct, and more maintainable code.
We'll also look at the gamut of techniques and design principles for dealing
with them.

