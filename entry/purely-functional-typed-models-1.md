A Purely Functional Typed Approach to Trainable Models (Part 1)

================================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on May 14, 2018.
> [Read online!](https://blog.jle.im/entry/purely-functional-typed-models-1.html)

With the release of [backprop](http://hackage.haskell.org/package/backprop),
I've been exploring the space of parameterized models of all sorts, from linear
and logistic regression and other statistical models to artificial neural
networks, feed-forward and recurrent (stateful). I wanted to see to what extent
we can really apply automatic differentiation and iterative gradient
decent-based training to all of these different models. Basically, I wanted to
see how far we can take *differentiable programming* (a la [Yann
LeCun](https://www.facebook.com/yann.lecun/posts/10155003011462143)) as a
paradigm for writing trainable models.

Building on other writers, I'm starting to see a picture unifying all of these
models, painted in the language of purely typed functional programming. I'm
already applying these to models I'm using in real life and in my research, and
I thought I'd take some time to put my thoughts to writing in case anyone else
finds these illuminating or useful.

As a big picture, I really believe that a purely functional typed approach to
differentiable programming is *the* way to move forward in the future for models
like artificial neural networks. In this light, the drawbacks of object-oriented
and imperative approaches becomes very apparent.

I'm not the first person to attempt to build a conceptual framework for these
types of models in a purely functional typed sense -- [Christopher Olah's famous
post](http://colah.github.io/posts/2015-09-NN-Types-FP/) wrote a great piece in
2015 that this post heavily builds off of, and is definitely worth a read! We'll
be taking some of his ideas and seeing how they work in real code!

This will be a three-part series, and the intended audience is people who have a
passing familiarity with statistical modeling or machine learning/deep learning.
The code in these posts is written in Haskell, using the
[backprop](http://hackage.haskell.org/package/backprop) and
[hmatrix](http://hackage.haskell.org/package/hmatrix) (with
[hmatrix-backprop](http://hackage.haskell.org/package/hmatrix-backprop))
libraries, but the main themes and messages won't be *about* haskell, but rather
about differentiable programming in a purely functional typed setting in
general. This isn't a Haskell post as much as it is an exploration, using
Haskell syntax/libraries to implement the points. The *backprop* library is
roughly equivalent to [autograd](https://github.com/HIPS/autograd) in python, so
all of the ideas apply there as well.

The source code for the written code in this module is available [on
github](https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs),
if you want to follow along!

## Essence of a Model

For the purpose of this post, a *parameterized model* is a function from some
input "question" (predictor, independent variable) to some output "answer"
(predictand, dependent variable)

Notationally, we might write it as a function:

$$
f_p(x) = y
$$

The important thing is that, for every choice of *parameterization* $p$, we get
a *different function* $f_p(x)$.

For example, you might want to write a model that, when given an email, outputs
whether or not that email is spam.

The parameterization *p* is some piece of data that we tweak to produce a
different $f_p(x)$. So, "training" (or "learning", or "estimating") a model is a
process of picking the $p$ that gives the "correct" function $f_p(x)$ --- that
is, the function that accurately predicts spam or whatever thing you are trying
to predict.

For example, for [linear
regression](https://en.wikipedia.org/wiki/Linear_regression), you are trying to
"fit" your $(x, y)$ data points to some function $f(x) = \beta + \alpha x$. The
*parameters* are $\alpha$ and $\beta$, the *input* is $x$, and the *output* is
$\beta + \alpha
x$.

As it so happens, a $f_p(x)$ is really just a "partially applied" $f(p,x)$.
Imagining that function, it has type:[^1]

$$
f : (P \times A) \rightarrow B
$$

If we [curry](https://en.wikipedia.org/wiki/Currying) this, we get the original
model representation we talked about:

$$
f : P \rightarrow (A \rightarrow B)
$$

### Optimizing Models with Observations

Something interesting happens if we flip the script. What if, instead of
$f_p(x)$, we talked about $f_x(p)$? That is, we fix the input and vary the
parameter, and see what type of outputs we get for the same output while we vary
the parameter?

If we have an "expected output" for our input, then one thing we can do is look
at $f_x(p)$ and see when the result is close to $y_x$ (the expected output of
our model when given $x$).

In fact, we can turn this into an optimization problem by trying to pick $p$
that minimizes the difference between $f_x(p)$ and $y_x$. We can say that our
model with parameter $p$ predicts $y_x$ the best when we minimize:

$$
(f_x(p) - y_x)^2
$$

If we minimize the squared error between the result of picking the parameter and
the expected result, we find the best parameters for that given input!

In general, picking the best parameter for the model involves picking the $p$
that minimizes the relationship

$$
\text{loss}(y_x, f_x(p))
$$

Where $\text{loss} : B \times B \rightarrow \mathbb{R}$ gives a measure of "how
badly" the model result differs from the expected target. Common loss functions
include squared error, cross-entropy, etc.

This gives us a supervised way to train any model: if we have enough
observations ($(x, y_x)$ pairs) we can just pick a $p$ that does its best to
make the loss between all observations as small as possible.

### Stochastic Gradient Descent

If our model is a *differentiable function*, then we have a nice tool we can
use: *stochastic gradient descent* (SGD).

That is, we can always calculate the *gradient* of the loss function with
respect to our parameters. This gives us the direction we can "nudge" our
parameters to make the loss bigger or smaller.

That is, if we get the *gradient* of the loss with respect to $p$ ($\nabla_p
\text{loss}(f_x(p), y_x)$), we now have a nice iterative way to "train" our
model:

1.  Start with an initial guess at the parameter
2.  Look at a random $(x, y_x)$ observation pair.
3.  Compute the gradient $\nabla_p \text{loss}(f_x(p), y_x)$ of our current $p$,
    which tells us a direction we can "nudge" $p$ in to make the loss smaller.
4.  Nudge $p$ in that direction
5.  Repeat from #2 until satisfied

With every new observation, we see how we can nudge the parameter to make the
model more accurate, and then we perform that nudge. At the end of it all, we
wind up just the right `p` to model the relationship between our observation
pairs.

## Functional Implementation

What I described naturally lends to a functional implementation. That's because,
in this light, a model is nothing more than a curried function (a function
returning a function). A model that is trainable using SGD is simply a
differentiable function.

Using the *[backprop](http://hackage.haskell.org/package/backprop)* library, we
can write these differentiable functions as normal functions.

Let's pick a type for our models. A model from type `a` to type `b` with
parameter `p` can be written as the type synonym

``` haskell
type Model p a b = p -> a -> b
```

Not normally differentiable, but we can make it a differentiable function by
having it work with `BVar z p` and `BVar z a` (`BVar`s containing those values)
instead:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L52-L55

type Model p a b = forall z. Reifies z W
                => BVar z p
                -> BVar z a
                -> BVar z b
```

This is a RankN *type synonym*, which is saying that a `Model p a b` is just a
type synonym for a differentiable `BVar z p -> BVar z a -> BVar z b`. The
`Reifies z W` is just a constraint that allows for backpropagation of `BVar`s.

We can write a simple linear regression model:

$$
f_{\alpha, \beta}(x) = \beta x + \alpha
$$

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L48-L369

data a :& b = !a :& !b

pattern (:&&) :: (Backprop a, Backprop b, Reifies z W)
              => BVar z a -> BVar z b -> BVar z (a :& b)

linReg :: Model (Double :& Double) Double Double
linReg (a :&& b) x = b * x + a
```

A couple things going on here to help us do things smoothly:

-   We define a custom tuple data type `:&`; backprop works with normal tuples,
    but using a custom tuple with a `Num` instance will come in handy later for
    training models.

-   We define a pattern synonym `:&&` that lets us "pattern match out" `BVar`s
    of that tuple type. So if we have a `BVar z (a :& b)` (a `BVar` containing a
    tuple), then matching on `(x :&& y)` will give us `x :: BVar z     a` and
    `y :: BVar z b`.

-   With that, we define `linReg`, whose parameters are a `Double :& Double`, a
    tuple the two parameters `a` and `b`. After pattern matching out the
    contents, we just write the linear regression formula --- `b * x + a`. We
    can use normal numeric operations like `*` and `+` because `BVar`s have a
    `Num` instance.

We can *run* `linReg` using `evalBP2`:

``` haskell
ghci> evalBP2 linReg (0.3 :& (-0.1)) 5
-0.2        -- (-0.1) * 5 + 0.3
```

But the neat thing is that we can also get the gradient of the parameters, too,
if we identify a loss function:[^2]

$$
\nabla_p (f(p, x) - y_x)^2
$$

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L60-L68

squaredErrorGrad
    :: (Backprop p, Backprop b, Num b)
    => Model p a b      -- ^ Model
    -> a                -- ^ Observed input
    -> b                -- ^ Observed output
    -> p                -- ^ Parameter guess
    -> p                -- ^ Gradient
squaredErrorGrad f x targ = gradBP $ \p ->
    (f p (auto x) - auto targ) ^ 2
```

We use `auto :: a -> BVar z a`, to lift a normal value to a `BVar` holding that
value, since our model `f` takes `BVar`s.

And finally, we can train it using stochastic gradient descent, with just a
simple fold over all observations:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L70-L76

trainModel
    :: (Fractional p, Backprop p, Num b, Backprop b)
    => Model p a b      -- ^ model to train
    -> p                -- ^ initial parameter guess
    -> [(a,b)]          -- ^ list of observations
    -> p                -- ^ updated parameter guess
trainModel f = foldl' $ \p (x,y) -> p - 0.1 * squaredErrorGrad f x y p
```

For convenience, we can define a `Random` instance for our tuple type using the
*[random](http://hackage.haskell.org/package/random)* library and make a wrapper
that uses `IO` to generate a random initial parameter:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L78-L85

trainModelIO
    :: (Fractional p, Backprop p, Num b, Backprop b, Random p)
    => Model p a b      -- ^ model to train
    -> [(a,b)]          -- ^ list of observations
    -> IO p             -- ^ parameter guess
trainModelIO m xs = do
    p0 <- (/ 10) . subtract 0.5 <$> randomIO
    return $ trainModel m p0 xs
```

Let's train our linear regression model to fit the points `(1,1)`, `(2,3)`,
`(3,5)`, `(4,7)`, and `(5,9)`! This should follow $f(x) = 2 x - 1$, or
$\alpha = -1,\, \beta = 2$:

``` haskell
ghci> samps = [(1,1),(2,3),(3,5),(4,7),(5,9)]
ghci> trainModelIO linReg $ take 5000 (cycle samps)
(-1.0000000000000024) :& 2.0000000000000036
-- roughly:
(-1.0) :& 2.0
```

Neat --- after going through all of those observations a thousand times, the
model nudges itself all the way to the right parameters to fit our model!

The important takeaway is that all we specified was the *function* of the model
itself. The training part all follows automatically.

### Feed-forward Neural Network

Here's another example: a fully-connected feed-forward neural network layer.

We can start with a single layer. The model here will also take two parameters
(a weight matrix and a bias vector), take in a vector, and output a vector.

``` haskell
import Numeric.LinearAlgebra.Static.Backprop
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L92-L103

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

feedForwardLog
    :: (KnownNat i, KnownNat o)
    => Model (L o i :& R o) (R i) (R o)
feedForwardLog (w :&& b) x = logistic (w #> x + b)
```

Here we use the `L n m` (an n-by-m matrix) and `R n` (an n-vector) types from
the *hmatrix* library, and `#>` for backprop-aware matrix-vector multiplication.

Let's try training a model to learn the simple [logical
"AND"](https://en.wikipedia.org/wiki/Logical_conjunction):

``` haskell
ghci> import qualified Numeric.LinearAlgebra.Static as H
ghci> samps = [(H.vec2 0 0, 0), (H.vec2 1 0, 0), (H.vec2 0 1, 0), (H.vec2 1 1, 1)]
ghci> trained <- trainModelIO feedForwardLog $ take 10000 (cycle samps)
```

We have our trained parameters! Let's see if they actually model "AND"?

``` haskell
ghci> evalBP2 feedForwardLog trained (H.vec2 0 0)
(7.468471910660985e-5 :: R 1)       -- 0.0
ghci> evalBP2 feedForwardLog trained (H.vec2 1 0)
(3.816205998697482e-2 :: R 1)       -- 0.0
ghci> evalBP2 feedForwardLog trained (H.vec2 0 1)
(3.817490115313559e-2 :: R 1)       -- 0.0
ghci> evalBP2 feedForwardLog trained (H.vec2 1 1)
(0.9547178031665701 :: R 1)         -- 1.0
```

Close enough for me!

If we inspect the arrived-at parameters, we can peek into the neural network's
brain:

``` haskell
ghci> trained
(matrix
 [ 4.652034474187562, 4.65355702367007 ] :: L 1 2) :& (-7.073724083776028 :: R 1)
```

It seems like there is a heavy negative bias, and that each of the inputs makes
some contribution that is slightly more than half of the negative bias; the end
goal is that one of the inputs alone makes no dent, but only if both inputs are
"on", the output can overcome the negative bias.

The network was able to arrive that this configuration just by exploring the
gradient of our differentiable function!

### Functional composition

Because our functions are simply just *normal functions*, we can create new,
complex models from simpler ones using just functional composition.

For example, we can map the result of a model to create a new model. Here, we
compose `linReg ab` (linear regression with parameter `ab`) with the logistic
function to create a *[logistic
regression](https://en.wikipedia.org/wiki/Logistic_regression)* model.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L117-L118

logReg :: Model (Double :& Double) Double Double
logReg ab = logistic . linReg ab
```

Here, we use function composition `(.)`, one of the most common combinators in
Haskell, saying that `(f . g) x = f (g x)`.

We could have even written our `feedForwardLog` without its activation function:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L95-L98

feedForward
    :: (KnownNat i, KnownNat o)
    => Model (L o i :& R o) (R i) (R o)
feedForward (w :&& b) x = w #> x + b
```

And now we can swap out activation functions using simple function composition:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L120-L123

feedForwardLog'
    :: (KnownNat i, KnownNat o)
    => Model (L o i :& R o) (R i) (R o)
feedForwardLog' wb = logistic . feedForward wb
```

Maybe even a [softmax](https://en.wikipedia.org/wiki/Softmax_function)
classifier!

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L125-L133

softMax :: (Reifies z W, KnownNat n) => BVar z (R n) -> BVar z (R n)
softMax x = konst (1 / sumElements expx) * expx
  where
    expx = exp x

feedForwardSoftMax
    :: (KnownNat i, KnownNat o)
    => Model (L o i :& R o) (R i) (R o)
feedForwardSoftMax wb = softMax . feedForward wb
```

We can even write a function to *compose* two models, keeping their two original
parameters separate:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L135-L141

(<~)
    :: (Backprop p, Backprop q)
    => Model  p       b c
    -> Model       q  a b
    -> Model (p :& q) a c
(f <~ g) (p :&& q) = f p . g q
infixr 8 <~
```

And now we have a way to chain models! Maybe even make a multiple-layer neural
network? Let's see if we can get a two-layer model to learn
[XOR](https://en.wikipedia.org/wiki/Exclusive_or) ...

Our model is two feed-forward layers with logistic activation functions, with 4
hidden layer units:

``` haskell
ghci> let twoLayer :: Model _ (R 2) (R 1)
          twoLayer = feedForwardLog' @4 <~ feedForwardLog'
```

Note we use type application syntax (the `@`) to specify the input/output
dimensions of `feedForwardLog'` to set our hidden layer size; when we write
`feedForwardLog' @4`, it means to set the `i` type variable to `4`. We also use
`_` type wildcard syntax because we want to just let the compiler infer the type
of the model parameter for us instead of explicitly writing it out ourselves.

We can train it on sample points:

``` haskell
ghci> let samps :: [(R 2, R 1)]
          samps = [(H.vec2 0 0, 0), (H.vec2 1 0, 1), (H.vec2 0 1, 1), (H.vec2 1 1, 0)]
ghci> trained <- trainModelIO twoLayer $ take 50000 (cycle samps)
```

Trained. Now, does it model "XOR"?

``` haskell
ghci> evalBP2 twoLayer trained (H.vec2 0 0)
(3.0812844350410647e-2 :: R 1)          -- 0.0
ghci> evalBP2 twoLayer trained (H.vec2 1 0)
(0.959153369985914 :: R 1)              -- 1.0
ghci> evalBP2 twoLayer trained (H.vec2 0 1)
(0.9834757090696419 :: R 1)             -- 1.0
ghci> evalBP2 twoLayer trained (H.vec2 1 1)
(3.6846467867668035e-2 :: R 1)          -- 0.0
```

Not bad!

## Just Functions

We just built a working neural network using normal function composition and
simple combinators. No need for any objects or mutability or fancy explicit
graphs. Just pure, typed functions! Why would you ever bring anything imperative
into this?

You can build a lot with just these tools alone. By using primitive models and
the various combinators, you can create autoencoders, nonlinear regressions,
convolutional neural networks, multi-layered neural networks, generative
adversarial networks...you can create complex "graphs" of networks that fork and
re-combine with themselves.

The nice thing is that these are all just regular (Rank-2) functions, so...you
have two models? Just compose their functions like normal functions!

It is tempting to look at something like

``` haskell
feedForwardLog @4 <~ feedForwardLog
```

and think of it as some sort of abstract, opaque data type with magic inside.
After all, "layers" are "data", right? But, at the end of the day, it's all
just:

``` haskell
\(p :&& q) -> feedForwardLog @4 p . feedForwardLog q
```

Just normal function composition -- we're really just defining the *function*
itself, and *backprop* turns that function into a trainable model.

In the past I've talked about [layers as
data](https://blog.jle.im/entry/practical-dependent-types-in-haskell-1.html),
and neural network libraries like
[grenade](http://hackage.haskell.org/package/grenade-0.1.0) let you manipulate
neural network layers in a composable way. My previous attempts at neural
networks like [tensor-ops](https://github.com/mstksg/tensor-ops) also force a
similar structure of composition of data types. Frameworks like
*[tensorflow](https://www.tensorflow.org/)* and
*[caffe](http://caffe.berkeleyvision.org/)* also treat [layer as
data](https://docs.google.com/presentation/d/1UeKXVgRvvxg9OUdh_UiC5G71UMscNPlvArsWER41PsU/edit#slide=id.gc2fcdcce7_216_264).
However, I feel this is a bit limiting.

You are forced to "compose" your layers in only the ways that the API of the
data type gives you. You have to use the data type's "function composition"
functions, or its special "mapping" functions...and for weird things like
forking compositions like `\x -> f (g x) (h x)` you have to learn how the data
type offers such an API.

However, such a crazy composition here is "trivial" -- it's all just normal
functions, so you can just literally write out code like `\x -> f (g x) (h x)`
(or something very close). You don't have to learn any rules of special "layer"
data types. Layers aren't matrices or "data" --- they're functions. Not just
abstractly, but literally. All your models are! And, with differentiable
programming, they become *trainable functions*.

### What Makes It Tick

My overall thesis of this series is about four essential properties of executing
effective differentiable programing-based models. All of these things, I feel,
have to come together seamlessly to make this all work.

1.  *Functional programming*, allowing us to write higher-order functions and
    combinators that take functions and return functions.

    This is the entire crux of this approach, and lets us not only draw from
    mathematical models directly, but also combine and reshape models in
    arbitrary ways just by using normal function composition and application,
    instead of being forced into a rigid compositional model.

    We were able to chain, fork, recombine simple model primitives to make *new*
    models by just writing normal higher-order functions. In fact, as we will
    see in the upcoming posts, we can actually re-use higher order functions
    like `foldl` and `map` that are already commonly used in functional
    programming.

    In the upcoming posts, we will take this principle to the extreme. We'll
    define more combinators like `(<~)` and see how many models we think are
    "fundamental" (like recurrent neural networks, autoregressive models) really
    are just combinators applied to even simpler models.

    The role of these combinators is not *essential*, but *helpful* --- we could
    always fall back on normal function composition, but higher-order functions
    and combinators let us encapsulate certain repeating design patterns and
    transformations.

2.  *Differentiable* programs, allowing us to write normal functions and have
    them be automatically differentiable for gradient descent.

    I'm not sure at this point if this is best when supported at the
    language/compiler level, or at the library level. Whatever it is, though,
    the combination of differentiable programming with higher-order functions
    and other functional programming fundamentals is what makes this
    particularly powerful.

3.  *Purely* functional programming. If *any* of these functions were
    side-effecting and impure functions, the correspondence between functions
    and mathematical models completely falls apart. This is something we often
    take for granted when writing Haskell, but in other languages, without
    purity, no model is sound. If we are writing in a non-pure language, we have
    to consider this as an explicit assumption.

4.  A *strong expressive static type system with type inference* makes this all
    reasonable to work with.

    A lot of the combinators in this approach (like `(<~)`) manipulate the
    *type* of model parameters, and if we use a lot of them, it becomes either
    impossible or unfeasible to manage it all in our heads. Without the help of
    a compiler, it would be impossible to sanely write complex programs. Having
    a statically type system with *type inference* allows the compiler to keep
    track of these for us and manage parameter shapes, and lets us ask questions
    about the parameters that our models have at compile-time.

    For example, note how in our `twoLayer` definition, we left a type wildcard
    so the compiler can fill in the type for us.

    We'll also see in later posts that if we pick the types of our combinators
    correctly, the compiler can sometimes basically write our code for us.

    In addition, having to think about types forces us to think, ahead of time,
    about how types interact. This thought process itself often yields important
    insight.

In the [next
post](https://blog.jle.im/entry/purely-functional-typed-models-2.html), we will
explore how to reap the surprising benefits of this purely functional typed
style when applying it to stateful and recurrent models.

--------------------------------------------------------------------------------

Hi, thanks for reading! You can reach me via email at <justin@jle.im>, or at
twitter at [\@mstk](https://twitter.com/mstk)! This post and all others are
published under the [CC-BY-NC-ND
3.0](https://creativecommons.org/licenses/by-nc-nd/3.0/) license. Corrections
and edits via pull request are welcome and encouraged at [the source
repository](https://github.com/mstksg/inCode).

If you feel inclined, or this post was particularly helpful for you, why not
consider [supporting me on Patreon](https://www.patreon.com/justinle/overview),
or a [BTC donation](bitcoin:3D7rmAYgbDnp4gp4rf22THsGt74fNucPDU)? :)

[^1]: Those familiar with Haskell idioms might recognize this type as being
    essentially `a -> Reader p b` (or `Kleisli (Reader p) a b`) which roughly
    represents the notion of "A function from `a` to `b` with an 'environment'
    of type `p`".

[^2]: Note that this is only sound as a loss function for a single "scalar
    value", like `Double` or a one-vector. In general, we'd have this take a
    loss function as a parameter.

