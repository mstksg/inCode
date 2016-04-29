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
ever measure something to be $12.4\,\mathrm{cm}$, that doesn't mean it's
$12.400000\,\mathrm{cm}$, it means that it's somewhere between
$12.3\,\mathrm{cm}$ and $12.5\,\mathrm{cm}$...and we don't know exactly.  We
can write it as $12.4 \pm 0.1\,\mathrm{cm}$.

The interesting thing happens when we try to add, multiply, divide numbers with
uncertainty.  What happens when you "add" $12 \pm 3$ and $19 \pm 6$?

The initial guess might be $27 \pm 9$, because one is $\pm 3$ and the other
is $\pm 6$.

But!  If you actually do experiments like this several times, you'll see that
this isn't the case.  If you tried this out experimentally and simulate several
hundred trials, you'll see that the answer is actually something like $31 \pm
7$.

Let's write ourselves a Haskell data type that lets us work with "numbers with
inherent uncertainty":

~~~haskell
ghci> let x = 12 +/- 3
ghci> let y = 19 +/- 6
ghci> x + y
31 +/- 7
ghci> x * y
550 +/- 90
ghci> logBase y x
1.4 +/- 0.1
ghci> sin(x) / y
-0.2 +/- 0.1
~~~



<!-- That's because more often than not, the errors in both -->
<!-- values will "cancel each other out" -- it's relatively unlikely that they'll -->
<!-- both error in the same direction, and so when you add two uncertain values -->
<!-- together, their uncertainties tend to cancel each other out. -->

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




