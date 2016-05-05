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
$12.400000\,\mathrm{cm}$ --- it means that it's somewhere between
$12.3\,\mathrm{cm}$ and $12.5\,\mathrm{cm}$...and we don't know exactly.  We
can write it as $12.4 \pm 0.1\,\mathrm{cm}$.

The interesting thing happens when we try to add, multiply, divide numbers with
uncertainty.  What happens when you "add" $12 \pm 3$ and $19 \pm 6$?

The initial guess might be $27 \pm 9$, because one is $\pm 3$ and the other is
$\pm 6$.  But!  If you actually do experiments like this several times, you'll
see that this isn't the case.  If you tried this out experimentally and
simulate several hundred trials, you'll see that the answer is actually
something like $31 \pm 7$.[^try]

[^try]: If you don't believe me, stop reading this article now and try it
yourself!  You can simulate noisy data by using uniform noise distributions,
Gaussian distributions, or however manner you like.  Verify by checking the
[standard deviation][] of the sums.

[standard deviation]: https://en.wikipedia.org/wiki/Standard_deviation

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

[ad]: https://hackage.haskell.org/package/ad

This post and series is actually a walk through of the implementation of my
[uncertain][] library, now on hackage!  The functionality in this library is
offered along with more features there.

[uncertain]: https://hackage.haskell.org/package/uncertain

You can follow along with [the source code][source], which is actually a
*[stack][]* executable!  If you download the source and you have *[stack][]*
installed, you can run it (and run the tests above) as an executable:

[stack]: http://www.haskellstack.org
!!![source]:uncertain/Uncertain.hs

~~~bash
$ ./Uncertain.hs
~~~

Otherwise, you can run it directly with stack (using `runhaskell`) and the
[linear][] and [ad][] packages installed...or load it up with `stack ghci` to
play with it.  This article was written under snapshot [lts-5.15][]!

[linear]: https://hackage.haskell.org/package/linear
[lts-5.15]: https://www.stackage.org/lts-5.15

Dealing with Uncertainty Precisely
----------------------------------

First of all, let's think about why adding two "uncertain" values doesn't
involve simply adding the uncertainties linearly.  (If you don't care about the
math and just want to get on to the Haskell, feel free to skip this section!)

If I have a value $16 \pm 3$ (maybe I have a ruler whose ticks are 3 units
apart, or an instrument that produces measurements with 4 units of noise), it
either means that it's a little below 16 or a little above 16.  If I have an
independently sampled value $25 \pm 4$, it means that it's a little below 25 or
a little above 25.

What happens if I want to think about their sum?  Well, it's going to be
somewhere around 41.  But, the uncertainty won't be $\pm 7$.  It would only be
$\pm 7$ if the errors in the two values are *always aligned*.  Only when every
"little bit above" 16 error lines up perfectly with a "little bit above" 25
error, and when every single "little bit below" 16 error lines up perfectly
with a "little bit above" 25 error, would you really get something that is $\pm
7$.  But, because the two values are sampled independently, you shouldn't
expect such alignment.  So, you'll get an uncertainty that's *less than* $\pm
7$.  In fact, it'll actually be around $\pm 5$.

In general, we find that for *independent* $X$ and $Y$:

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

Where $f_x(x_0,y_0)$ is the first (partial) derivative with respect to $x$ at $(x_0,
y_0)$.

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
\frac{1}{2} f_{yy}(\mu_X, \mu_Y) \sigma_Y^2
$$

Where $f_{xx}(\mu_X, \mu_Y)$ is the second (partial) derivative with respect to
$x$ twice at $(\mu_X, \mu_Y)$

For our case of simple addition, $\operatorname{E}[X + Y] = \mu_X + \mu_Y$,
because the second-order partials of $f(x,y) = x + y$ are 0.

Uncertain Values in Haskell
---------------------------

So, how are we going to model our uncertain values in Haskell ... ?  With an
Algebraic Data Type, of course! [^adt]

[^adt]: What else were you expecting!

~~~haskell
!!!uncertain/Uncertain.hs "data Uncert"
~~~

We'll keep track of the mean (the central point) and the *variance*, which is
the standard deviation *squared*.  We keep track of the variance and not the
standard deviation (the "plus or minus") because the mathematics is a bit more
straightforward.

We can write a function to turn a "plus or minus" statement into an `Uncert`:

~~~haskell
!!!uncertain/Uncertain.hs "(+/-) ::"
~~~

Give the `dx` (the standard deviation) and store `dx^2`, the variance.

Let's also throw in a handy helper function for "exact" values:

~~~haskell
!!!uncertain/Uncertain.hs "exact ::"
~~~

But, we can do better (if just for fun).  We can use pattern synonyms to
basically "abstract" away the data type itself, and let people pattern match on
a mean and standard deviation:

~~~haskell
!!!uncertain/Uncertain.hs "pattern (:+/-)"
~~~

Now, people can pattern match on `x :+/- dx` and receive the mean and
uncertainty directly.  Neat!

~~~haskell
!!!uncertain/Uncertain.hs "uStdev ::"
~~~

### Making it Numeric

Now, time for the magic!  Let's write a `Num` instance!

~~~haskell
instance Num a => Num (Uncert a) where
    fromIntegral      = exact . fromIntegral
    Un x vx + Un y vy = Un (x + y)    (vx + vy)
    Un x vx - Un y vy = Un (x - y)    (vx + vy)
    Un x vx * Un y vy = Un (x * y)    (y^2 * vx + x^2 * vy)
    negate (Un x vx)  = Un (negate x) vx
    -- ...
~~~

And...that's it!  Do the same thing for every numeric typeclass, and you get
automatic propagation of uncertainty woo hoo.

Pretty anticlimactic, huh?

### The Problem

But, wait --- this method is definitely not ideal.  It's pretty repetitive, and
involves a but of copy-and-pasting code that is slightly different in ways the
typechecker can't verify.  What if we didn't change something we were supposed
to?  And, if you look at the `Fractional` instance...

~~~haskell
instance Fractional a => Fractional (Uncert a) where
    fromRational      = exact . fromRational
    Un x vx / Un y vy = Un (x/y + x/y^3*vy)   (x^2/y^4*vx + vy/y^2)
    recip (Un x vx)   = Un (recip x + vx/x^3) (vx / x^4)
~~~

Yikes.  All that ugly and complicated numerical code that the typechecker can't
verify.  Those are runtime bugs just waiting to happen.  How do we even *know*
that we calculated the right derivatives, and implemented the formula
correctly?

What if we could reduce this boilerplate?  What if we could somehow
analytically compute derivatives for functions instead of computing them
manually?

Automatic Differentiation
-------------------------

Automatic differentiation is honestly one of the coolest Haskell tricks you can
show that any beginner can immediately understand.  Like our trick with
`Uncert`, it is nice to use because of its overloaded `Num`/numeric
typeclasses.

~~~haskell
ghci> diff (\x -> x^2) 10
20
ghci> diff (\x -> sin x) 0
1.0
ghci> diff (\x -> sin (x^3))
0.47901729549851046
~~~

A very rough explanation about how forward-mode automatic differentiation works
is that it uses a wrapper type (like ours) that defines `*`, `negate`, etc. so
that they also compute the *derivative(s)* of the function, instead of just the
*result*, like normal.

### Single-variable functions

And, now that we can automatically differentiate functions, we can use this
knowledge directly in our implementations.  Let's define a universal "lifter"
of single-variable functions.

We use the function `diffs0` to get a "tower" of derivatives:

~~~haskell
ghci> diffs0 (\x -> x^2 - 2 x^3) 4
[-112, -88, -46, -12, 0, 0, 0, 0...
~~~

The first value is actually $4^2 - 2 \times 4^3$.  The second is the derivative
($2 x - 6x^2$) at 4, the third is the second derivative $2 - 12 x$ at 4, then
the third derivative $-12$, then the fourth derivative $0$, etc.

We only need the actual value and the first two derivatives, so we can pattern
match them as `fx:dfx:ddfx:_ = diffs0 f x`, the derivatives and values of the
function we lift, `f`, around the mean `x`.

At that point, the equations we have from before just translate nicely:

$$
\operatorname{E}[f(X)] = f(\mu_X) + \frac{1}{2} f_{xx}(\mu_X) \sigma_X^2
$$

$$
\operatorname{Var}[f(X)] = f_x(\mu_X)^2 \sigma_X^2
$$

And we call $\mu_X$ `x` and $\sigma_X^2$ `vx`, and this becomes:

~~~haskell
y  = fx + ddfx * vx / 2
vy = dfx^2 * vx
~~~

Putting it all together:

~~~haskell
!!!uncertain/Uncertain.hs "liftU"
~~~

The type `forall s. AD s (Tower a) -> AD s (Tower a)` looks a little scary, but
you can think of it as representing a function on `a` (like `negate`, `(*2)`,
etc.) that the *ad* library can differentiate several times --- something you
could use with `diff0` to get a "tower" of derivatives.

And ... that's it!  We can already define things like:

~~~haskell
negate = liftU negate
recip  = liftU recip
sqrt   = liftU sqrt
sin    = liftU sin
~~~

### Multivariable functions

*ad* also lets you work multivariable functions, too.  To model multivariable
functions, it takes a function from a `Traversable` of vales to a single value.
We can use the `V2` type from the *[linear][]* package to pass in a
two-variable function:

[linear]: shttp://hackage.haskell.org/package/linear/docs/Linear-V2.html

~~~haskell
ghci> grad (\(V2 x y) -> x * y^2 + 3*x) (V2 3 1)
V2 4 6
~~~

The gradient of $f(x, y) = x y^2 + 3x$ is $(y^2 + 3, 2xy)$, which, at $(3, 1)$,
is indeed $(4, 6)$.

The gradient gives us the first order partials, but we need the second order
partials to calculate the new mean, so for that, we can use `hessian`:

~~~haskell
ghci> hessian (\(V2 x y) -> x * y^2 + 3*x) (V2 3 1)
V2 (V2 0 2)
   (V2 2 6)
~~~

The [hessian][] of a function is a symmetric matrix where the diagonals are the
second-order repeated partial derivatives of each variable, and the
off-diagonals are mixed partial derivatives.  In our case, we only care about
the diagonal.  Indeed, the double-partial with respect to $x$ is $0$, and the
double-partial with respect to $y$ is $2x$, which gives us a hessian with a
diagonal $(0, 6)$ at $(3, 1)$.

[hessian]: https://en.wikipedia.org/wiki/Hessian_matrix

The *ad* package generously gives us a function that lets us calculate the
function's result, its gradient, and its hessian all in one pass:

~~~haskell
ghci> hessian' (\(V2 x y) -> x * y^2 + 3*x) (V2 3 1)
(12, V2 (4, V2 0 3)
     V2 (6, V2 2 6)
)
~~~

We can access the gradient by using `fmap fst` on the second component of the
tuple and access the hessian by using `fmap snd`.

We need a couple of helpers, first --- one to get the "diagonal" of our
hessian, because we only care about the repeated partials:

~~~haskell
!!!uncertain/Uncertain.hs "diag ::"
~~~

And then a "dot product", which sums two lists component-wise and adds the
results:

~~~haskell
!!!uncertain/Uncertain.hs "dot ::"
~~~

And now we can write our multi-variate function lifter:

~~~haskell
!!!uncertain/Uncertain.hs "liftUF"
~~~

(Again, don't mind the scary type `forall s. f (AD s (Sparse a)) -> AD s (Sparse a)`,
it's just *ad*'s type for things you can use `hessian'` on)

And we can write some nice helper functions so we can use them more easily:

~~~haskell
!!!uncertain/Uncertain.hs "liftU2" "liftU3"
~~~

At this point, we're officially done.  We can fill in the other two-argument
functions:

~~~haskell
(+)     = liftU2 (+)
(*)     = liftU2 (*)
(/)     = liftU2 (/)
(**)    = liftU2 (**)
logBase = liftU2 logBase
~~~

Admittedly, there's still some slight boilerplate (that you can get rid of with
some Template Haskell, maybe), but you have a *lot* less room for error, and a
lot simpler to check over and read to make sure you didn't miss any bugs.

Wrapping it up
--------------

The full code (with all of the numeric instances fully implemented) is up [on
github][source], which you can run and explore and test by executing it or
loading it with `stack ghci`.  I've added a special *Show* instance that
"rounds" your values to as many digits that your uncertainty suggests, to give
more meaningful `show`s.

All of what's in this post is actually up on my *[uncertain][]* package on
hackage, if you want to use it in your own projects, or see how I take this and
make it more robust for real-world applications.  The project also has more
features on top of the basic things shown here.

### Verification and Accuracy

My *[uncertain][]* package has a monte carlo module to propagate uncertainty
through monte carlo simulations.  Let's see how the values compare!

~~~haskell
ghci> x + y         -- Monte Carlo Results:
46 +/- 2            -- actually 46 +/- 2
ghci> x * y
450 +/- 40          -- actually 450 +/- 40
ghci> sqrt (x + y)
6.8 +/- 0.2         -- actually 6.8 +/- 0.2
ghci> logBase y x
0.78 +/- 0.02       -- actually 0.78 +/- 0.02
ghci> log (x**y)
85.9 +/- 0.3        -- actually 83 +/- 6
~~~

So, it looks like the mathematical model of uncertainty propagation matched up
well with the "actual" results we gain from monte carlo simulations!  The only
one of our examples that was significantly wrong was the
$\operatorname{log}(x^y)$ example, which heavily underestimated the uncertainty
by about a factor of 20.  But, remember, the model was derived after dropping
the 2nd, 3rd, 4th, etc. terms of the taylor expansion for the calculation of
the new uncertainty, and the 4th, 6th, etc. terms of the taylor expansion for
the calculation of the new mean.  For functions that have high second, third,
fourth derivatives relative to the mean and the uncertainty, it's going to be a
bit off.


### What next?

A simple extension of this would be to implement the monte carlo simulator I
mentioned above, which is pretty straightforward to implement with the
*[mwc-random][]* package.

[mwc-random]: https://hackage.haskell.org/package/mwc-random

However, the most disturbing thing here that we never deal with is what happens
correlated terms that are combined.  All of our math assumed uncorrelated
samples.  But what happens if we have expressions that involve additions of
correlated values?

For example:

~~~haskell
ghci> let x = 14.6 +/- 0.8 in x + x
29 +/- 1
ghci> let x = 14.6 +/- 0.8 in 2*x
29 +/- 2
~~~

Unfortunately, `x + x` is different than `2*x`.  This is because `x` acts like
an *independent generator*, so when you say `x + x`, it expands to
`(14.6 +/- 0.8) + (14.6 +/- 0.8)`, which represents the addition of two
independent samples.

When you say `2*x`, that represents sampling `x` *once* and *doubling* it.  If
you sample `x` and double it, any error in `x` will also be doubled.  That's
why the uncertainty is greater in the `2*x` version.

How can we account for correlated values that are combined in complex ways?
Stay tuned for the next part![^spoilers]

[^spoilers]: Or just look at my [package][uncertain] :)

