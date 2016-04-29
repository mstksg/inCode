---
title: Automatic Propagation of Uncertainty with AD
categories: Haskell, Tutorials
tags: haskell, ad, numerical methods
create-time: 2016/04/25 16:04:25
date: Nothing
identifier: uncertain
slug: automatic-propagation-of-uncertainty-with-ad
---

Some of my favorite Haskell "tricks" involve working with exotic numeric types
with custom "overloaded" numeric functions and literals that let us work with
data in surprisingly elegant and expressive ways.

Here is one example --- from my work in experimental physics and statistics, we
often deal with experimental/sampled values with inherent uncertainty.  If you
ever measure something to be $12.4 cm$, that doesn't mean it's $12.400000 cm$,
it means that it's somewhere between $12.3 cm$ and $12.5 cm$...and we don't
know exactly.  We can write it as $12.4 +/- 0.1 cm$.



<!-- One of my favorite Haskell magic tricks is "automatic differentiation", "ad", -->
<!-- which is a surprising application of Haskell's overloaded numeric -->
<!-- typeclasses/literals, simple algebraic data type manipulation, and universal -->
<!-- quantification.  The magic happens when you think you're calculating normal -->
<!-- numeric functions with `+` and `*` and `sin`, etc.,...but you're actually -->
<!-- calculating their derivatives instead. -->

<!-- ~~~haskell -->
<!-- ghci> diff (\x -> x^2) 10 -->
<!-- 20 -->
<!-- ghci> diff (\x -> sin x) 0 -->
<!-- 1.0 -->
<!-- ghci> diff (\x -> sin (x^3)) -->
<!-- 0.47901729549851046 -->
<!-- ~~~ -->

<!-- We define a new data type with a funky `Num` instance, so instead of defining -->
<!-- $x^3$ as actual exponentiation, we define it to return $3x^2 \dot{x}$ instead, -->
<!-- etc. It's a rather cute technique and something that's accessible to any -->
<!-- Haskell beginner. -->




