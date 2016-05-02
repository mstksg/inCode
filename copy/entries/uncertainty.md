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
7$.[^try]

[^try]: If you don't believe me, stop reading this article now and try it
yourself!  You can simulate noisy data by using uniform noise distributions,
Gaussian distributions, or however manner you like.  Verify by checking the
[variance][] of the sum.

[variance]: https://en.wikipedia.org/wiki/Variance

Let's write ourselves a Haskell data type that lets us work with "numbers with
inherent uncertainty":

~~~haskell
ghci> let x = 14.6 +/- 0.8
ghci> let y = 31   +/- 2
ghci> x + y
46 +/- 2
ghci> x * y
450 +/- 40
ghci> sqrt (x + y)
6.8 +/- 0.2
ghci> logBase y x
0.78 +/- 0.02
ghci> log (x**y)
85.9 +/- 0.3
~~~

Along the way, we'll also learn how to harness the power of awesome [ad][]
library, a library used in implementing back-propagation and other optimization
algorithms, to analyze numerical functions in a mathematical way and break down
their derivatives and gradients.

[ad]: http://hackage.haskell.org/package/ad

Certain Uncertainty
-------------------

First of all, let's think about why adding two "uncertain" values doesn't
involve simply adding the uncertainties linearly.  If you don't care about the
math and just want to get on to the Haskell, feel free to skip this section!

If I have a value $16 \pm 3$ (maybe I have a ruler whose ticks are 2 units
apart, or an instrument that produces measurements with 4 units of noise), it
either means that it's a little below 16 or a little above 16.  If I have an
independently sampled value $25 \pm 4$, it means that it's a little below 25 or
a little above 25.

What happens if I want to think about their sum?  Well, it's going to be
somewhere around 41.  But, the uncertainty won't be $\pm 7$.  In order for that
to be possible, the errors in the two values have to *always be aligned*.  Only
when every "little bit above" 16 error lines up perfectly with a "little bit
above" 25 error, and when every single "little bit below" 16 error lines up
perfectly with a "little bit above" 25 error, would you really get something
that is $\pm 7$.  But, because the two values are sampled independently, you
shouldn't expect such alignment.  So, you'll get an uncertainty that's *less
than* $\pm 7$.  In fact, it'll actually be around $\pm 5$.

In general, we find that, for *independent* $X$ and $Y$:

$$
\operatorname{Var}[aX + bY + c] = a^2 \sigma_X^2 + b^2 \sigma_Y^2
$$

Where $\sigma_X^2$ is the variance in $X$.  We consider $\sigma_X$ to be the
standard deviation of $X$, or the "plus or minus" part of our numbers.  In the
simple case of addition, we have $\operatorname{Var}[X + Y] = \sigma_X^2
+ \sigma_Y^2$, so our new uncertainty is $\sqrt{\sigma_X^2 + \sigma_Y^2}$.

However, not all functions that combine $X$ and $Y$ can be expressed as simple
linear combinations $aX + bY + c$.  But!  If you dig back to your days of high
school calculus, you might remember a method for expressing any arbitrary
function as a linear approximation -- the [Taylor Expansion][]!

[Taylor Expansion]: https://en.wikipedia.org/wiki/Taylor_series

In general, we can attempt to approximate any well-behaving function as its
tangent hyperplane:

$$
f(x_0 + x, y_0 + y) \approx f_x(x_0, y_0) x + f_y(x_0, y_0) y + f(x_0, y_0)
$$

Look familiar?  This is exactly the form that we used earlier to calculate
"combined" variance!

$$
\operatorname{Var}[f(X,Y)] \approx f_x(\mu_X, \mu_Y)^2 \sigma_X^2 + f_y(\mu_X,\mu_Y)^2 \sigma_Y^2
$$


A similar analysis can be used to figure out how the expected value changes by
taking the taylor expansion to the *second* degree:

$$
\operatorname{E}[f(X,Y)] \approx
f(\mu_X, \mu_Y) +
\frac{1}{2} f_{xx}(\mu_X, \mu_Y) \sigma_X^2 +
\frac{1}{2} f_{yy}(\mu_X, \mu_Y) \sigma_Y^2 +
$$

For our case of simple addition, $\operatorname{E}[X + Y] = \mu_X + \mu_Y$,
because the second-order partials of $f(x,y) = x + y$ are 0.


Uncertain Values in Haskell
---------------------------

So, how are we going to model our uncertain values in Haskell ... ?  With an
Algebraic Data Type, of course! [^adt]

[^adt]: What else were you expecting!

~~~haskell
data Uncert a = Un { uMean :: !a
                   , uVar  :: !a
                   }
~~~

We'll keep track of the mean (the central point) and the *variance*, which is
the standard deviation *squared*.  We keep track of the variance and not the
standard deviation (the "plus or minus") because the mathematics is a bit more
straightforward.

We can write a function to turn a "plus or minus" statement into an `Uncert`:

~~~haskell
(+/-) :: Num a => a -> a -> Uncert a
x +/- dx = Un x (dx*dx)
~~~

Give the `dx` (the standard deviation) and store `dx^2`, the variance.

Let's also throw in a handy helper function for "exact" values:

~~~haskell
exact :: Num a => a -> Uncert a
exact x = x +/- 0
~~~

But, we can do better.  We can use pattern synonyms to basically "abstract"
away the data type itself, and let people "pattern match" on a mean and
standard deviation:

~~~haskell
pattern (:+/-) :: () => Floating a => a -> a -> Uncert a
pattern x :+/- dx <- Un x (sqrt->dx)
  where
    x :+/- dx = Un x (dx*dx)
~~~

Now, people can pattern match on `x :+/- dx` and receive the mean and
uncertainty directly.  Neat!

### Making it Numeric

Now, time for the magic!  Let's write a `Num` instance!

~~~haskell
instance Num a => Num (Uncert a) where
    fromIntegral      = exact . fromIntegral
    Un x vx + Un y vy = Un (x + y)    (vx + vy)
    Un x vx - Un y vy = Un (x - y)    (vx + vy)
    Un x vx * Un y vy = Un (x * y)    (y*y * vx + x*x * vy)
    negate (Un x vx)  = Un (negate x) vx
    -- ...
~~~

And...that's it!  Do the same thing for every numeric typeclass, and you get
automatic propagation of uncertainty woo hoo.

Pretty anticlimactic, huh?

### The Problem

But, wait...this method is definitely not ideal.  It's pretty repetitive, and
involves a but of copy-and-pasting code that is slightly different in ways the
typechecker can't verify.  What if we didn't change something we were supposed
to?  And...if you look at the `Fractional` instance...

~~~haskell
instance Fractional a => Fractional (Uncert a) where
    fromRational      = exact . fromRational
    Un x vx / Un y vy = Un (x/y + x/y^3*vy)   (x^2/y^4*vx + vy/y^2)
    recip (Un x vx)   = Un (recip x + vx/x^3) (vx / x^4)
~~~

Yikes.  All that ugly and complicated numerical code that the typechecker
verify.  Those are runtime bugs just waiting to happen.  How do we even *know*
that we calculated the right derivatives, and implemented the formula
correctly?

What if we could reduce this boilerplate to things that the typechecker can
enforce for us?  That'd be ideal, right?  What if we could somehow analytically
compute derivatives for functions instead of computing them manually?

Automatic Differentiation
-------------------------

Surprise!







<!-- Some people like to talk about probability and statistics as "inexact maths" or -->
<!-- "non-deterministic math", but the exact opposite is true.  Probability and -->
<!-- statistics is the *exact*, rigorous, and *deterministic* math of -->
<!-- non-deterministic domains. -->

<!-- But first, let's think about why adding -->

<!-- Quantum mechanics, after all, is one of the most -->
<!-- exact and deterministic triumphs of mathematical physics -- despite what you -->
<!-- might hear in physics popularisations.[^qm] -->

<!-- [^qm]: Quantum mechanics, the discipline, makes very precise, exact, and -->
<!-- testable predictions about probability distributions and non-deterministic -->
<!-- processes, and the predictions of quantum mechanics are some of the most -->
<!-- precisely tested and verified predictions in the history of physics. -->



<!-- ~~~haskell -->
<!-- 46 +/- 2 -->
<!-- 450 +/- 41 -->
<!-- 6.8 +/- 0.2 -->
<!-- 0.78 +/- 0.02 -->
<!-- 83 +/- 6 -->
<!-- ~~~ -->



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




