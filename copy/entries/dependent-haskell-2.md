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

(If you read [Part 1][] *before* the singletons section was re-written to use
the [singletons][] library, [here's a link to the section][new-section] in
specific.  This tutorial will assume familiarity with what is discussed there!)

[singletons]: https://hackage.haskell.org/package/singletons
[new-section]: https://blog.jle.im/entry/practical-dependent-types-in-haskell-1.html#singletons-and-induction

All of what we've dealt with so far has essentially been with types that are
fixed at compile-time.  All the networks we've made have had "static" types,
with their sizes in their types indicated directly in the source code.

Today, we're going to dive into the world of types that *depend* on factors
unknown until runtime, and see how dependent types in a strongly typed language
like Haskell helps us write safer, more correct, and more maintainable code.
Along the way, we'll encounter and learn first-hand about techniques and
guiding high-level principles that we can apply to our other dependently typed
coding endeavours.

Serializing Networks
--------------------

To warm up, let's talk about serializing networks.

Serializing networks of 



<!-- At the Boundary -->
<!-- --------------- -->

<!-- There's a sort of mode of thinking that comes with --> 

<!-- You can see in the last post a definite demarcation of two "worlds": the world -->
<!-- of "untyped", non-dependently typed programming, and the world of typed, -->
<!-- dependently typed programming. -->

<!-- So far we've worked completely -->

<!-- When I first heard about types that depend on runtime values, my mind -->
<!-- immediately jumped to the idea of dynamic types ... which is, of course, the -->
<!-- thing that all Haskellers are indoctrinated to hate from day 1.  But --> 








<!-- sameNat and existentials -->
