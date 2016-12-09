---
title: Hamiltonian Dynamics in Haskell
categories: Haskell
tags: functional programming, haskell, physics, numerical methods, dependent types
create-time: 2016/12/08 15:05:10
date: never
identifier: hamilton-1
slug: Hamilton Dynamics in Haskell
---

As promised in my [*hamilton* introduction post][intro], I'm going to go over
implementing of the *[hamilton][]* library using *[ad][]* and dependent types.

[intro]: https://blog.jle.im/entry/introducing-the-hamilton-library.html
[hamilton]: http://hackage.haskell.org/package/hamilton
[ad]: http://hackage.haskell.org/package/ad

This post will be a bit heavy in some mathematics and Haskell concepts, so I'm
just going to clarify the expected audience/prerequisites for making the most
out of this post:

1.  Working intermediate Haskell knowledge.  No knowledge of dependent types is
    assumed or required.
2.  A familiarity with concepts of multivariable calculus (like partial and
    total derivatives).
3.  Familiarity with concepts of linear algebra (like dot products, matrix
    multiplication, and matrix inverses)
4.  No knowledge of physics should be required, but a basic familiarity with
    first-year physics concepts would help your appreciation of this post!

