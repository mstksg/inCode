---
title: Lenses embody Products, Prisms embody Sums
categories: Haskell
tags: lenses, profunctors
create-time: 2018/05/22 23:29:16
identifier: lenses-and-prisms
slug: lenses-products-prisms-sums
---

I've written about a variety of topics on this blog, but one thing I haven't
touched in too much detail is the topic of lenses and optics.  A big part of
this is because there are already so many great resources on lenses, like the
famous (and my favorite) [lenses over tea][tea] series.

[tea]: https://artyom.me/lens-over-tea-1

This post won't be a "lens tutorial", but rather a dive into a (what I believe
is an) insightful perspective on lenses and prisms that I've heard repeated
many times, but not yet all gathered together into a single place.  In
particular, I'm going to talk about the perspective of lenses and prisms as
embodying the essences of products and sums (respectively), and how that
observation can help you with a more "practical" understanding of lenses and
prisms.

Products and Sums
-----------------

In Haskell, "products and sums" can roughly be said to correspond to "tuples
and `Either`".  If I have two types `A` and `B`, `(A, B)` is their "product"
type.  It's often called an "anonymous product", because we can make one
without having to give it a fancy name.  It's called a product type because `A`
has $n$ possible values and `B` has $m$ possible values, then `(A, B)` has
$n \times m$ possible values[^bottom].  And, `Either A B` is their (anonymous)
"sum" type.  It's called a sum type because `Either A B` has $n + m$ possible
values.  I won't go much deeper into this, but there are [many useful tutorials
already online][adts] on this topic!

[^bottom]: All of this is disregarding the notorious "bottom" value that
inhabits every type.

[adts]: https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types

It's easy to recognize `(Int, Double)` as a product between `Int` and
`Bool`.  However, did you know that some types are secretly product types in
disguise?

For example, here's a classic example of a lensable data type

```haskell
data Person = P { _pName :: String
                , _pAge  :: Int
                }
```

`Person` is an algebraic data type --- so-called because it is actually a
*product* between a `String` and `Int`.  `Person` is isomorphic to 

<!-- Here's an easy one! -->

<!-- ```haskell -->
<!-- data MyType = MyType Double Int -->
<!-- ``` -->

<!-- `MyType` is a product between `Double` and `Int`.  It's isomorphic to `(Double, -->
<!-- Int)`.  By isomorphic, I mean that there are functions `f :: MyType -> (Double, -->
<!-- Int)` and `g :: (Double, Int) -> MyType` such that `f . g = id` and `g . f = -->
<!-- id`. -->

<!-- We have another one: the *non-empty list* type, `NonEmpty a`, is actually a -->
<!-- product between a `a` (the head/first item) and an `[a]` (the tail/rest of the -->
<!-- items).  So, `NonEmpty a` is isomorphic to `(a, [a])`.  It's a product in -->
<!-- disguise! -->

<!-- It's easy enough to recognize `Either String Bool` as a sum between -->
<!-- `String` and `Bool`, but did you know that some types are secretly sum types in -->
<!-- disguise? -->
