---
title: Introducing the backprop library
categories: Haskell
tags: functional programming, haskell, numerical, machine learning, artificial neural networks
create-time: 2018/02/11 21:44:37
date: 2018/02/12 10:27:44
identifier: backprop-intro
slug: introducing-the-backprop-library
series: Backprop
---

**backprop**: [hackage][backprop] / [github][]

[backprop]: http://hackage.haskell.org/package/backprop
[github]: https://github.com/mstksg/backprop

I'm excited to announce the first official release of the *[backprop][]*
library (currently at version 0.1.2.0 on hackage)!  *backprop* is a library
that allows you write functions on your heterogeneous values like you would
normally and takes them and (with reverse-mode automatic differentiation)
automatically generate functions computing their gradients.  *backprop* differs
from the related *[ad][]* by working with functions using and transforming
different types, instead of only one monomorphic scalar type.

[ad]: http://hackage.haskell.org/package/ad

This has been something I've been working on for a while (trying to find a good
API for *heterogeneous* automatic differentiation), and I'm happy to finally
find something that I feel good about, with the help of a *[lens][]*-based API.

[lens]: http://hackage.haskell.org/package/lens

As a quick demonstration, this post will walk through the creation of a simple
neural network implementation (inspired by the [Tensorflow Tutorial][tf-intro]
for beginners) to learn handwritten digit recognition for the MNIST data set.
To help tell the story, we're going to be implementing it "normally", using the
*[hmatrix][]* library API, and then re-write the same thing using
*[backprop][]* and *[hmatrix-backprop][]* (a drop-in replacement for
*hmatrix*).

[tf-intro]: https://www.tensorflow.org/versions/r1.2/get_started/mnist/beginners
[hmatrix]: http://hackage.haskell.org/package/hmatrix
[hmatrix-backprop]: http://hackage.haskell.org/package/hmatrix-backprop

The Basics
----------

For this network, we're not going to be doing anything super fancy.  Our
"neural network" will just be simple series of matrix multiplications, vector
additions, and activation functions.  We're going to make a neural network with
a single hidden layer using normal Haskell data types, parameterized by two
weight matrices and two bias vectors.

The purpose of the MNIST challenge is to take a vector of pixel data (28x28, so
784 elements total) and classify it as one of ten digits (0 through 9).  To do
this, we're going to be building and training a model that takes in a
784-vector of pixel data and produces a 10-item [one-hot][] vector of categorical
predictions (which is supposed to be 0 everywhere, except for 1 in the category
we predict the input picture to be in).

[one-hot]: https://en.wikipedia.org/wiki/One-hot

### Types

For our types, our imports are pretty simple:

```haskell
!!!backprop/intro-normal.hs "import"
```

Our `Net` type will just be a simple collection of all of the matrices and
vectors we want to optimize:

```haskell
!!!backprop/intro-normal.hs "data Net"
```

We're using the matrix types from [`Numeric.LinearAlgebra.Static`][static].  An
`L 250 784` is a $250 \times 784$ matrix -- or, as we are using it, a linear
transformation $\mathbb{R}^{784} \rightarrow \mathbb{R}^{250}$.  An `R 250` is a
250-vector, etc.

[static]: https://hackage.haskell.org/package/hmatrix/docs/Numeric-LinearAlgebra-Static.html

Via the *lens* library, four lenses are generated:

```haskell
weights1 :: Lens' Net (L 250 784)
bias1    :: Lens' Net (R 250)
weights2 :: Lens' Net (L 10  250)
bias2    :: Lens' Net (R 10)
```

These lenses give us ways to access components of our data type:

```haskell
myNet             :: Net
myNet ^. weights1 :: L 250 784  -- access the weights1 field in myNet
myNet ^. bias2    :: R  10      -- access the bias2 field in myNet
```

I'm also going to define `Num` and `Fractional` instances for our network,
which makes it really easy to write code to "gradient descend" our network (we
can just add and scale our networks with each other).  To do this, I'm going
to be using *[one-liner-instances][]* to make a `Num` instance automatically
using GHC Generics:

```haskell
!!!backprop/intro-normal.hs "instance Num Net" "instance Fractional Net"
```

Without Backprop
----------------

### Running

First, let's look at the picture if we just try to compute the error function
for our network directly.

Running our network is pretty textbook:

```haskell
!!!backprop/intro-normal.hs "runNet"
```

`runNet` takes a network and produces the `R 784 -> R 10` function it encodes.

`#> :: L m n -> R n -> R m` is the matrix-vector multiplication operator from
*hmatrix* (its [static][] module); we can also just use `+` (from `Num`) to add
vectors together.

We use the [logistic function][] as our internal activation function and
[softmax][] to normalize our outputs:

[logistic function]: https://en.wikipedia.org/wiki/Logistic_function
[softmax]: https://en.wikipedia.org/wiki/Softmax_function

```haskell
!!!backprop/intro-normal.hs "logistic" "softMax"
```

We can define the logistic function using only `Num` operations, which operate
component-wise for *hmatrix* types.  `softMax` requires us to `norm_1` (to get
the absolute sum of all items in a vector) from *hmatrix*, and also `konst` (to
generate a vector of a single item repeated). Still, though, pretty much a
straightforward implementation of the mathematical definitions.

### Error Function

This neural network now makes predictions.  However, in order to *train* a
network, we actually need a scalar *error function* that we want to minimize.
This is a function on the network that, given an input and its expected output,
computes how "bad" the currently network is. It computes the error between the
output of the network and the expected output, as a single number.

To do this, we will be using the [cross entropy][] between the target output
and the network output.  This is a standard error function for classification
problems; smaller cross-entropies indicate "better" predictions.

[cross entropy]: https://en.wikipedia.org/wiki/Cross_entropy

```haskell
!!!backprop/intro-normal.hs "crossEntropy" "netErr"
```

Computing the cross entropy involves using `<.>` (the dot product) from
*hmatrix*, but other than that we can just use `log` (from `Floating`) and
negation (from `Num`).

### Training

At this point, we are supposed to find a way to compute the *gradient* of our
error function. It's a function that computes the *direction of greatest
change* of all of the components in our network, with respect to our error
function.

The gradient will take our `Net -> Double` error function and, given a current
network, and produce a "gradient" `Net` whose components contain the derivative
of each component with respect to the error. It tells us how to "nudge" each
component to increase the error function. *Training* a neural network involves
moving in the opposite direction of the gradient, which causes the error to go
*down*.

However, given `netErr`'s definition, it is not obvious how to compute our
gradient function.  Doing so involves some careful multi-variable vector
calculus and linear algebra based on our knowledge of the operations we used.
For simple situations we often do it by hand, but for more complicated
situations, this becomes impractical.  That's where *automatic differentiation*
comes into play.

We've gone as far as we can go now, so let's drop into the world of *backprop*
and see what it can offer us!

With Backprop
-------------

Let's see what happens if we compute our error function using *backprop*,
instead!

We'll switch out our imports very slightly:

```haskell
!!!backprop/intro-backprop.hs "import"
```

First, we add `Numeric.Backprop`, the module where the magic happens.

Second, we switch from `Numeric.LinearAlgebra.Static` to
[`Numeric.LinearAlgebra.Static.Backprop`][static-backprop] (from *[hmatrix-backprop][]*), which exports the exact
same[^same] API as `Numeric.LinearAlgebra.Static`, except with numeric
operations that are "lifted" to work with *backprop*.  It's meant to act as a
drop-in replacement, and, because of this, most of our actual code will be more
identical.

[static-backprop]: https://hackage.haskell.org/package/hmatrix-backprop/docs/Numeric-LinearAlgebra-Static-Backprop.html

[^same]: More or less.  See module documentation for more information.

### Running

Writing functions that can be used with *backprop* involves tweaking the types
slightly -- instead of working directly with values of type `a`, we work with
`BVar`s (backpropagatable variables) *containing* `a`s: a `BVar s a`.

For example, let's look a version `softMax` that works with *backprop*:

```haskell
!!!backprop/intro-backprop.hs "softMax"
```

Instead of `R 10 -> R 10`, its type signature is now `BVar s (R 10) -> BVar s
(R 10)`.  Instead of working directly with `R 10`s (10-vectors), we work with
`BVar s (R 10)`s (`BVar`s containing 10-vectors).

`Numeric.LinearAlgebra.Static.Backprop` re-exports `konst` and `norm_1` (as
`norm_1V` --- `norm_1` for vectors only) lifted to work with `BVar`s:

```haskell
-- normal
konst   ::        Double ->         R 10
-- backprop
konst   :: BVar s Double -> BVar s (R 10)

-- normal
norm_1  ::         R 10  ->        Double
-- backprop
norm_1V :: BVar s (R 10) -> BVar s Double
```

`BVar`s also have `Num`, `Fractional`, and `Floating` instances, so `exp` and
`/` already work out-of-the-box.

With only a minimal and mechanical change in our code, `softMax` is now
automatically differentiable!

One neat trick --- because of `BVar`'s numeric instances, we can actually
re-use our original implementation of `logistic`:

```haskell
!!!backprop/intro-backprop.hs "logistic"
```

To *run* our network, things look pretty similar:

```haskell
!!!backprop/intro-backprop.hs "runNet"
```

Again, pretty much the same, except with the lifted type signature.  One
notable difference, however, is how we *access* the weights and biases. Instead
of using `^.` for lens access, we can use `^^.`, for lens access into a `BVar`:

```haskell
myNetVar                :: BVar s Net          -- a Net inside a BVar
myNetVar ^^. weights1   :: BVar s (L 250 784)  -- access the weights1 field in myNetVar
myNetVar ^^. bias2      :: BVar s (R  10    )  -- access the bias2 field in myNetVar
```

Some insight may be gleamed from a comparison of their type signatures:

```haskell
(^.)  ::        a -> Lens' a b ->        b
(^^.) :: BVar s a -> Lens' a b -> BVar s b
```

`^.` is access to a value using a lens, and `^^.` is access to a value inside a
`BVar` using a lens.

Using lenses like this gives us essentially frictionless usage of `BVar`s,
allowing us to access items inside data types in a natural way.  We can also
*set* items using `.~~` (to parallel `.~`), and access constructors in sum
types using `^^?` (which can be used to implement pattern matching) and get
matches for *multiple* targets using `^^..`:

```haskell
(^..)  ::        a -> Traversal' a b -> [       b]
(^^..) :: BVar s a -> Traversal' a b -> [BVar s b]
```

Because of these, our translation from our normal `runNet` to our *backprop*
`runNet` is more or less completely mechanical.

### Error Function

At this point, the implementation of our updated error function should not be
too surprising:

```haskell
!!!backprop/intro-backprop.hs "crossEntropy" "netErr"
```

Both of these implementations are are 100% lexicographically *identical* in
implementation to our original ones -- the only difference is that `<.>` comes
from `Numeric.LinearAlgebra.Static.Backprop`.  Other than that, we can simply
re-use `log` and negation.

### Training

Time to gradient descend!

```haskell
!!!backprop/intro-backprop.hs "stepNet"
```

And...that's it!

To break this down:

1.  To train our network, we move in the opposite direction of our gradient.
    That means `net0 - 0.02 * gr` -- we subtract the gradient (scaled by 0.02,
    a learning rate, to ensure we don't overshoot our goal) from our network.

    Recall that we implemented scaling and subtraction of `Net`s when we wrote
    its `Num` and `Fractional` instances earlier.

2.  To compute our gradient, we use `gradBP`:

    ```haskell
    gradBP :: (forall s. Reifies s W => BVar s a -> BVar s b) -> a -> a
    ```

    If we ignore the RankN type/`Reifies` syntax noise, this can be read as:

    ```haskell
    gradBP :: (BVar s a -> BVar s b) -> a -> a
    ```

    Which says "give a function from a `BVar` of `a` to a `BVar` of `b`, get
    the gradient function, from `a` to its gradient"

    This can be contrasted with `evalBP`:

    ```haskell
    evalBP :: (BVar s a -> BVar s b) -> a -> b
    ```

    Which "runs" the actual `a -> b` function that the `BVar s a -> BVar s b`
    encodes.

3.  We want to use `gradBP` with our `Net -> Double` error function (or more
    accurately our `BVar s Net -> BVar s Double` function).  That's exactly what
    `netErr` gives us.

    We use `constVar` to lift `x` and `targ`:

    ```haskell
    constVar :: a -> BVar s a
    ```

    `constVar` simply lifts a value into a `BVar`, knowing that we don't care
    about its gradient.

    This means that we have:

    ```haskell
    netErr (constVar x) (constVar targ) :: BVar s Net -> BVar s Double
    ```

    We can pass this function to `gradBP` to get the gradient of the network
    `Net` with respect to the `Double` error.

[one-liner-instances]: http://hackage.haskell.org/package/one-liner-instances

That's really the entire gradient computation and descent code!

Kind of anti-climactic, isn't it?

Taking it for a spin
--------------------

In the [source code][] I've included some basic code for loading the mnist data
set and training the network, with some basic evaluations.

!!![source code]:backprop/intro-backprop.hs

If you download it [here][source code], you can compile it using a
stack's self-compiling script feature (if *[stack][]* is installed on your
computer):

[stack]: https://docs.haskellstack.org/en/stable/README/

```bash
$ ./intro-backprop.hs    # compiles itself, managing dependencies automatically
```

The above command will cause the program to compile itself, installing the
necessary GHC (if needed) and also the automatically download the dependencies
from hackage.  *backprop* manages the automatic differentiation, and *stack*
manages the automatic dependency management :)

If you are following along at home, you can download the [mnist data set
files][MNIST] and uncompress them into a folder, and run it all with:

[MNIST]: http://yann.lecun.com/exdb/mnist/

```bash
$ ./intro-backprop PATH_TO_DATA
Loaded data.
[Epoch 1]
(Batch 1)
Trained on 5000 points.
Training error:   13.26%
Validation error: 13.44%
(Batch 2)
Trained on 5000 points.
Training error:   9.74%
Validation error: 11.08%
(Batch 3)
Trained on 5000 points.
Training error:   6.84%
Validation error: 8.71%
(Batch 4)
Trained on 5000 points.
Training error:   6.84%
Validation error: 8.53%
(Batch 5)
Trained on 5000 points.
Training error:   5.80%
Validation error: 7.55%
(Batch 6)
Trained on 5000 points.
Training error:   5.20%
Validation error: 6.77%
(Batch 7)
Trained on 5000 points.
Training error:   4.44%
Validation error: 5.85%
```

After about 35000 training points, we get down to 94% accuracy on our test set.
Neat!

A More Nuanced Look
-------------------

That's the high level overview -- now let's look a bit at the details that
might be helpful before you go strike it out on your own.

The main API revolves around writing a `BVar s a -> BVar s b` function
(representing an `a -> b` one), and then using one of the three runners:

```haskell
-- Return the result and gradient
backprop :: (Num a, Num b)
         => (forall s. Reifies s W => BVar s a -> BVar s b) -> a -> (a, b)

-- Return the result
evalBP   :: (forall s. Reifies s W => BVar s a -> BVar s b) -> a -> b

-- Return the gradient
gradBP   :: (Num a, Num b)
         => (forall s. Reifies s W => BVar s a -> BVar s b) -> a -> a
```

`evalBP` comes with virtually zero performance overhead (about 4%) over writing
your functions directly, so there's pretty much no harm in writing your entire
application or library in `BVar`-based code.

`gradBP`, however, carries measurable performance overhead over writing your
gradient code "manually", but this heavily depends on exactly how complex the
code you are backpropagating is.  The overhead comes from two potential
sources: the building of the function call graph, and also potentially from the
mechanical automatic differentiation process generating different operations
than what you might write by hand.  See the [README][] for a deeper analysis.

[README]: https://github.com/mstksg/backprop#readme

You might have also noticed the RankN type signature (the `forall s. ...`) that
I glossed over earlier.  This is here because *backprop* uses the RankN type
trick (from `Control.Monad.ST` and the *[ad][]* library) for two purposes:

1.  The prevent leakage of variables from the function.  You can't use `evalBP`
    to get a `BVar` out in the end, just like you can't use `runST` to get an
    `STRef` out in the end.  The type system prevents these
    variables from leaking out of the backprop/ST world.
2.  The `Reifies s W` constraint allows *backprop* to build a [Wengert Tape][]
    of your computation, which it uses internally to perform the reverse-mode
    automatic differentiation (The `W` stands for Wengert).

[Wengert Tape]: https://dl.acm.org/citation.cfm?doid=355586.364791

### Discussion on Num

Note that at the moment, `backprop`, `gradBP`, `(^^.)`, and most `BVar`-based
operations all require a `Num` instance on the things being backpropagated.
This is an API decision that is a compromise between different options, and the
[README][] has a deeper discussion on this.

For the most part, writing a `Num` instance for your types is some easy and
quick boilerplate if your type derives Generic (and we can use
*[one-liner-instances][]*), like we saw above with the `Num` instance for
`Net`.

One potential drawback is that requiring a `Num` instance means you can't
directly backpropagate tuples.  This can be an issue because of how pervasive
tuples are used for currying/uncurrying, and also because automatically
generated prisms use tuples for constructors with multiple fields.

To mitigate this issue, the library exports some convenient
tuples-with-Num-instances in `Numeric.Backprop.Tuple`.  If you are writing an
application, you can use the orphan instances in *[NumInstances][]*.

[NumInstances]: https://hackage.haskell.org/package/NumInstances

### Lifting your own functions

Of course, all of this would be useless unless you had a way to manipulate
`BVar`s.  The library does provide lens-based accessors/setters.  It also
provides `Num`, `Fractional`, and `Floating` instances for `BVar`s so you can
manipulate a `BVar s a` just like an `a` using its numeric instances.  We
leveraged this heavily by using `+`, `negate`, `log`, `/`, etc., and even going
as far as re-using our entire `logistic` implementation because it only relied
on numeric operations.

However, for our domain-specific operations (like matrix multiplication, norms,
and dot products), we needed to somehow lift those operations into
*backprop*-land, to work with `BVar`s.

This isn't something that end-users of the library should be expected to do --
ideally, this would be done by library maintainers and authors, so that users
can use their types and operations with *backprop*.  However, writing them is
not magical -- it just requires providing the result and the gradient with
respect to a final total derivative.  For example, let's look at the
implementation of the lifted `<.>`:

```haskell
import qualified Numeric.LinearAlgebra.Static as H

(<.>)
    :: Reifies s W
    => BVar s (R 10)
    -> BVar s (R 10)
    -> BVar s Double
(<.>) = liftOp2 . op2 $ \x y ->
    ( x H.<.> y
    , \d -> (H.konst d * y, x * H.konst d)
    )
```

To lift `(<.>)`, we provide a function that, given its inputs `x` and `y`,
gives the result (`x H.<.> y`), and also its gradient with respect to the total
derivative of the result.  For more details on the math, see the [documentation
for `Numeric.Backprop.Op`][op]!

[op]: https://hackage.haskell.org/package/backprop-0.1.2.0/docs/Numeric-Backprop-Op.html

If you're interested in writing your own lifted operations, take a look at the
[source of the lifted hmatrix module][hmatrix-lifted], which lifts (most) of
the functionality of *hmatrix* for backprop.  (And if you're good at computing
gradients, check out the module notes for some of the current unimplemented
operators -- any PR's would definitely be appreciated!)

[hmatrix-lifted]: https://github.com/mstksg/hmatrix-backprop/blob/master/src/Numeric/LinearAlgebra/Static/Backprop.hs

Conclusion
----------

The world is now your oyster!  Go out and feel emboldened to numerically
optimize everything you can get your hands on!

If you want to see an application to a more complex neural network type (and if
you're curious at how to implement the more "extensible" neural network types
like in my [blog series on extensible neural networks][neural]), I wrote [a
quick write-up][neural-backprop] on how to apply those type-level dependent
programming techniques to *backprop* (also available in [literate
haskell][neural-backprop-lhs]).

[neural]: https://blog.jle.im/entries/series/+practical-dependent-types-in-haskell.html
[neural-backprop]: https://github.com/mstksg/backprop/blob/master/renders/extensible-neural.pdf
[neural-backprop-lhs]: https://github.com/mstksg/backprop/blob/master/samples/extensible-neural.lhs

Really, though, the goal of backprop is to allow you to automatically
differentiate and optimize things you have *already* written (or plan to write,
if only you had the ability to optimize them).  Over the next few weeks I'll be
lifting operations from other libraries in the ecosystem. Let me know if there
are any that you might want me to look at first!  Be also on the lookout for
some other posts I'll be writing on applying *backprop* to optimize things
other than neural networks.

If you have any questions, feel free to leave a comment.  You can also give me
a shout on [twitter][] (I'm  *@mstk*), on freenode's *#haskell* (where I am
usually idling as *jle\`*), or on the [DataHaskell gitter][dataHaskell] (where
I hang out as *@mstksg*).

[twitter]: https://twitter.com/mstk "Twitter"
[dataHaskell]: https://gitter.im/dataHaskell/Lobby

Please let me know if you end up doing anything interesting with the library
--- I'd love to hear about it!  And, until next time, happy Haskelling!
