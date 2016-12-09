---
title: Hamiltonian Dynamics in Haskell
categories: Haskell
tags: functional programming, haskell, physics, numerical methods, dependent types
create-time: 2016/12/08 15:05:10
date: never
identifier: hamilton-1
slug: hamilton-dynamics-in-haskell
---

As promised in my [*hamilton* introduction post][intro], I'm going to go over
implementing of the *[hamilton][]* library using *[ad][]* and dependent types.

[intro]: https://blog.jle.im/entry/introducing-the-hamilton-library.html
[hamilton]: http://hackage.haskell.org/package/hamilton
[ad]: http://hackage.haskell.org/package/ad

This post will be a bit heavy in some mathematics and Haskell concepts.  The
expected audience is intermediate Haskell programmers, and no previous
knowledge of dependent types is expected.

The mathematics and physics are "extra" flavor text and could potentially be
skipped, but you'll get the most out of this article if you have basic
familiarity with:

1.  Basic concepts of multivariable calculus (like partial and total
    derivatives).
2.  Concepts of linear algebra (like dot products, matrix multiplication, and
    matrix inverses)

No physics knowledge is assumed, but knowing a little bit of first semester
physics would help you gain a bit more of an appreciation for the end result!
