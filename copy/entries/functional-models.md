---
title: A Purely Functional Typed Approach to Trainable Models
categories: Haskell
tags: machine learning
create-time: 2018/04/04 18:37:57
identifier: functional-models
slug: purely-functional-typed-models
---

With the release of [backprop][], I've been exploring the space of
parameterized models of all sorts, from linear and logistic regression and
other statistical models to artificial neural networks, feed-forward and
recurrent (stateful).  I wanted to see to what extent we can really apply
automatic differentiation and iterative gradient decent-based training to all
of these different models.  Basically, I wanted to see how far we can take
*differentiable programming* (a la [Yann LeCun][]) as a paradigm for writing
trainable models.

[backprop]: http://hackage.haskell.org/package/backprop
[Yann LeCun]: https://www.facebook.com/yann.lecun/posts/10155003011462143

I'm starting to see a picture unifying all of these models, painted in the
language of purely typed functional programming.  I'm already applying these to
models I'm using in real life and in my research, and I thought I'd take some
time to put my thoughts to writing in case anyone else finds these illuminating
or useful.

As a big picture, I really believe that a purely functional typed approach to
differentiable programming is *the* way to move forward in the future for
models like artificial neural networks.  In this light, the drawbacks of
object-oriented and imperative approaches becomes very apparent.

I'm not the first person to attempt to build a conceptual framework for these
types of models in a purely functional typed sense -- [Christopher Olah's
famous post][colah] wrote a great piece in 2015 that this post heavily
builds off of, and is definitely worth a read!  We'll be taking some of his
ideas and seeing how they work in real code!

[colah]: http://colah.github.io/posts/2015-09-NN-Types-FP/

The code in this post is written in Haskell, using the [backprop][],
[hmatrix][] (with [hmatrix-backprop][]), and [vector-sized][] libraries.

[hmatrix]: http://hackage.haskell.org/package/hmatrix
[hmatrix-backprop]: http://hackage.haskell.org/package/hmatrix-backprop
[vector-sized]: http://hackage.haskell.org/package/vector-sized

Essence of a Model
------------------

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
different $f_p(x)$.  So, "training" (or "learning", or "estimating") a model is
a process of picking the $p$ that gives the "correct" function $f_p(x)$ ---
that is, the function that accurately predicts spam or whatever thing you are
trying to predict.

For example, for [linear regression][linreg], you are trying to "fit" your $(x, y)$ data
points to some function $f(x) = \beta + \alpha x$.  The *parameters* are
$\alpha$ and $\beta$, the *input* is $x$, and the *output* is $\beta + \alpha
x$.

[linreg]: https://en.wikipedia.org/wiki/Linear_regression

As it so happens, a $f_p(x)$ is really just a "partially applied" $f(p,x)$.
Imagining that function, it has type:[^reader]

$$
f : P \times A \rightarrow B
$$

[^reader]: Those familiar with Haskell idioms might recognize this type as
being essentially `a -> Reader p b` (or `Kleisli (Reader p) a b`) which roughly
represents the notion of "A function from `a` to `b` with an 'environment' of
type `p`".


If we [curry][] this, we get the original model representation we talked about:

[curry]: https://en.wikipedia.org/wiki/Currying

$$
f : P \rightarrow (A \rightarrow B)
$$

### Optimizing Models with Observations

Something interesting happens if we flip the script.  What if, instead of
$f_p(x)$, we talked about $f_x(p)$?  That is, we fix the input and vary the
parameter, and see what type of outputs we get for the same output while we
vary the parameter?

If we have an "expected output" for our input, then one thing we can do is look
at $f_p(x)$ and see when the result is close to $y_x$ (the expected output of
our model when given $x$).

In fact, we can turn this into an optimization problem by trying to pick $p$
that minimizes the difference between $f_x(p)$ and $y_x$.  We can say that our
model with parameter $p$ predicts $y_x$ the best when we minimize:

$$
(f_x(p) - y_x)^2
$$

If we minimize the squared error between the result of picking the parameter
and the expected result, we find the best parameters for that given input!

In general, picking the best parameter for the model involves picking the $p$
that minimizes the relationship

$$
\text{loss}(y_x, f_x(p))
$$

Where $\text{loss} : B \times B \rightarrow \mathbb{R}$ gives a measure of "how
badly" the model result differs from the expected target.  Common loss
functions include squared error, cross-entropy, etc.

This gives us a supervised way to train any model: if we have enough
observations ($(x, y_x)$ pairs) we can just pick a $p$ that does its best to
make the loss between all observations as small as possible.

### Stochastic Gradient Descent

If our model is a *differentiable function*, then we have a nice tool we can
use: *stochastic gradient descent* (SGD).

That is, we can always calculate the *gradient* of the loss function with
respect to our parameters.  This gives us the direction we can "nudge" our
parameters to make the loss bigger or smaller.

That is, if we get the gradient of the loss with respect to $p$:

$$
\nabla_p \text{loss}(f_x(p), y_x)
$$

We now have a nice way to "train" our model:

1.  Start with an initial guess at the parameter
2.  Look at a random $(x, y_x)$ observation pair.
3.  Compute the gradient $\nabla_p \text{loss}(f_x(p), y_x)$ of our current
    $p$, which tells us a direction we can "nudge" $p$ in to make the loss
    smaller.
4.  Nudge $p$ in that direction
5.  Repeat from #2 until satisfied

With every new observation, we see how we can nudge the parameter to make the
model more accurate, and then we perform that nudge.

Functional Implementation
-------------------------

This naturally lends itself well to a functional implementation.  That's
because, in this light, a model is nothing more than a function.  And a model
that is trainable using SGD is simply a differentiable function.

Using the *[backprop][]* library, we can easily write functions to be
differentiable.

Let's write the type of our models.  A model from type `a` to type `b` with
parameter `p` can be written as

```haskell
type Model p a b = p -> a -> b
```

Not normally differentiable, but we can make it a differentiable function by
having it work with `BVar z p` and `BVar z a` (`BVar`s containing those values)
instead:

```haskell
!!!functional-models/model.hs "type Model"
```

We can write a simple linear regression model:

$$
f_{\alpha, \beta}(x) = \beta x + \alpha
$$


```haskell
!!!functional-models/model.hs "data a :& b" "linReg"
```

(First we define a custom tuple data type; backprop works with normal tuples,
but using a custom tuple with a `Num` instance will come in handy later for
training models)

Here `Double :& Double` is a tuple of two `Double`s, which contains the
parameters (`a` and `b`).  We extract the first item using `^^. t1` and the
second item with `^^. t2` (`t1` and `t2` being lenses defined for the tuple
fields), and then talk about the actual function, whose result is `b * x + a`.
Note that, because `BVar`s have a `Num` instance, we can use all our normal
numeric operators, and the results are still differentiable.

We can *run* `linReg` using `evalBP2`:

```haskell
ghci> evalBP2 linReg (0.3 :& (-0.1)) 5
-0.2        -- (-0.1) * 5 + 0.3
```

But the neat thing is that we can also get the gradient of the parameters, too,
if we identify a loss function:

$$
\nabla_p (f(p, x) - y_x)^2
$$


```haskell
!!!functional-models/model.hs "squaredErrorGrad"
```

We use `auto :: a -> BVar z a`, to lift a normal value to a `BVar` holding
that value, since our model `f` takes `BVar`s.

And finally, we can train it using stochastic gradient descent, with just a
simple fold over all observations:

```haskell
!!!functional-models/model.hs "trainModel"
```

For convenience, we can define a `Random` instance for our tuple type using the
*[random][]* library and make a wrapper that uses `IO` to generate a random
initial parameter:

[random]: http://hackage.haskell.org/package/random

```haskell
!!!functional-models/model.hs "trainModelIO"
```

Let's train our linear regression model to fit the points `(1,1)`, `(2,3)`,
`(3,5)`, `(4,7)`, and `(5,9)`!  This should follow $f(x) = 2 x - 1$, or
$\alpha = -1,\, \beta = 2$:

```haskell
ghci> samps = [(1,1),(2,3),(3,5),(4,7),(5,9)]
ghci> trainModelIO linReg $ take 5000 (cycle samps)
(-1.0000000000000024) :& 2.0000000000000036
-- roughly:
(-1.0) :& 2.0
```

Neat!  After going through all of those observations a thousand times, the
model nudges itself all the way to the right parameters to fit our model!

The important takeaway is that all we specified was the *function* of the model
itself.  The training part all follows automatically.

### Feed-forward Neural Network

Here's another example: a feed-forward neural network.

We can start with a single layer.  The model here will also take two parameters
(a weight matrix and a bias vector), take in a vector, and output a vector.

```haskell
import Numeric.LinearAlgebra.Static.Backprop
!!!functional-models/model.hs "logistic" "feedForwardLog"
```

Here we use the `L n m` (an n-by-m matrix) and `R n` (an n-vector) types from
the *hmatrix* library, and `#>` for backprop-aware matrix-vector
multiplication.

Let's try training a model to learn the simple [logical "AND"][and]:

[and]: https://en.wikipedia.org/wiki/Logical_conjunction

```haskell
ghci> import qualified Numeric.LinearAlgebra.Static as H
ghci> samps = [(H.vec2 0 0, 0), (H.vec2 1 0, 0), (H.vec2 0 1, 0), (H.vec2 1 1, 1)]
ghci> trained <- trainModelIO feedForwardLog $ take 10000 (cycle samps)
```

We have our trained parameters!  Let's see if they actually model "AND"?

```haskell
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

If we inspect the arrived-at parameters, we can see what makes the network
tick:

```haskell
ghci> trained
(matrix
 [ 4.652034474187562, 4.65355702367007 ] :: L 1 2) :& (-7.073724083776028 :: R 1)
```

It seems like there is a heavy negative bias, and that each of the inputs
makes some contribution that is slightly more than half of the negative bias;
the end goal is that one of the inputs alone makes no dent, but only if both
inputs are "on", the output can overcome the negative bias.

The network was able to arrive that this configuration just by exploring the
gradient of our differentiable function!

### Functional composition

Because our functions are simply just *normal functions*, we can create new,
complex models from simpler ones using just functional composition.

For example, we can map the result of a model to create a new model.  Here, we
compose `linReg ab` (linear regression with parameter `ab`) with the logistic
function to create a *[logistic regression][logit]* model.

[logit]: https://en.wikipedia.org/wiki/Logistic_regression

```haskell
!!!functional-models/model.hs "logReg"
```

We could have even written our `feedForwardLog` without its activation function:

```haskell
!!!functional-models/model.hs "feedForward"
```

And now we can swap out activation functions using simple function composition:

```haskell
!!!functional-models/model.hs "feedForwardLog'"
```

Maybe even a [softmax][] classifier!

[softmax]: https://en.wikipedia.org/wiki/Softmax_function

```haskell
!!!functional-models/model.hs "softMax" "feedForwardSoftMax"
```

We can even write a function to *compose* two models, keeping their two
original parameters separate:

```haskell
!!!functional-models/model.hs "(<~)"
```

And now we have a way to chain models!  Maybe even make a multiple-layer neural
network?  Let's see if we can get a two-layer model to learn [XOR][]!

[XOR]: https://en.wikipedia.org/wiki/Exclusive_or

Our model is two feed-forward layers with logistic activation functions, with 4
hidden layer units:

```haskell
ghci> twoLayer = feedForwardLog' @4 @1 <~ feedForwardLog' @2 @4
```

Note we use type application syntax to specify the input/output dimensions of
`feedForwardLog'`.

We can train it on sample points:

```haskell
ghci> samps = [(H.vec2 0 0, 0), (H.vec2 1 0, 1), (H.vec2 0 1, 1), (H.vec2 1 1, 1)]
ghci> trained <- trainModelIO twoLayer $ take 10000 (cycle samps)
```

Trained.  Now, does it model "XOR"?

```haskell
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

### They're Just Functions

We just built a working neural network using normal function composition and
simple combinators.  No need for any objects or mutability or fancy explicit
graphs.  Just pure, typed functions!  Why would you ever bring anything
imperative into this?

You can build a lot with just these tools alone.  By using primitive models and
the various combinators, you can create autoencoders, nonlinear regressions,
convolutional neural networks, multi-layered neural networks...you can create
complex "graphs" of networks that fork and re-combine with themselves.

The nice thing is that these are all just regular (Rank-2) functions, so...you
have two models?  Just compose their functions like normal functions!

It is tempting to look at something like

```haskell
feedForwardLog @4 @1 <~ feedForwardLog @2 @4
```

and think of it as some sort of abstract, opaque data type with magic inside.
After all, "layers" are "data", right?  But, at the end of the day, it's all
just:

```haskell
\pq -> feedForwardLog @4 @1 (pq ^^. t1) . feedForwardLog @2 @4 (pq ^^. t2)
```

Just normal function composition -- we're really just defining the *function*
itself, and *backprop* turns that function into a trainable model.

In the past I've talked about [layers as data][dephask], and neural network
libraries like [grenade][] let you manipulate neural network layers in a
composable way.  My previous attempts at neural networks like [tensor-ops][]
also force a similar structure of composition of data types.  However, I feel
this is a bit limiting.

[dephask]: https://blog.jle.im/entry/practical-dependent-types-in-haskell-1.html
[grenade]: http://hackage.haskell.org/package/grenade-0.1.0
[tensor-ops]: https://github.com/mstksg/tensor-ops

You are forced to "compose" your layers in only the ways that the API of the
data type gives you.  You have to use the data type's "function composition"
functions, or its special "mapping" functions...and for weird things like
forking compositions like `\f g h x -> f (g x) (h x)` you have to learn how the
data type offers such an API.

However, such a crazy composition here is "trivial" -- it's all just normal
functions, so you can just literally write out code like `\f g h x -> f (g x)
(h x)` (or something very close).  You don't have to learn any rules of special
"layer" data types.  At the heart of it all, your model is *just a function*.
And, with differentiable programming, it's a *trainable function*.

Time Series Models
------------------

Not all models are "question and answer" models, however -- some models
represent a time series.  As a generalization, we can talk about time series
models as:

$$
f_p(x,t) = y
$$

Which says, given an input and a time, return an output based on both.  The
point of this is to let us have recurrent relationships, like for
autoregressive models:

$$
\text{AR}_{\phi_1, \phi_2, \ldots}(x,t)
  = \epsilon_t + \phi_1 \text{AR}_{\phi_1, \phi_2, \ldots}(x, t-1)
  + \phi_2 \text{AR}_{\phi_1, \phi_2, \ldots}(x, t-2)
  + \ldots
$$

However, this is a bad way to look at models on time serieses, because nothing
is stopping the result of a model from depending on a future value (the value
at time $t = 3$, for instance, might depend explicitly only the value at time $t
= 5$).  Instead, we can imagine time series models as explicitly "stateful"
models:

$$
f_p(x, s_{\text{old}}) = (y, s_{\text{new}})
$$

These have type:[^statet]

$$
f : P \times A \times S \rightarrow B \times S
$$

[^statet]: If you recognized our original stateless model type as `a -> Reader
p b`, then you might have also recognized that this is the Haskell idiom `a ->
StateT s (Reader p) b` (or `Kleisli (StateT s (Reader p)) a b`), which
represents the notion of a "function from `a` to `b` with environment `p`, that
takes and returns a modified version of some 'state' `s`".

This makes it clear that the output of our model can only depend on current and
*previously occurring* information, preserving causality.

### Examples

We can use this to represent an AR(2) model ([autoregressive model with degree
2][AR]), which is a model whose output
forecast is a linear regression on the *last two* most recent observed values.
We can do this by setting the "input" to be the last observed value, and the
"state" to be the second-to-last observed value:

[AR]: https://en.wikipedia.org/wiki/Autoregressive_model

$$
\begin{aligned}
s_t & = x_t \\
y_t & = c + \phi_1 s_t + \phi_2 s_{t - 1}
\end{aligned}
$$

Or, in our explicit state form:

$$
f_{c, \phi_1, \phi_2}(x, s) = (c + \phi_1 x + \phi_2 s, x)
$$

There's also the classic [fully-connected recurrent neural network
layer][fcrnn], whose output is a linear combination of the (logistic'd)
previous output and the current input, plus a bias:

[fcrnn]: http://karpathy.github.io/2015/05/21/rnn-effectiveness/

$$
\begin{aligned}
s_t & = W_x \mathbf{x}_t + W_s \mathbf{s}_{t-1} + \mathbf{b} \\
y_t & = \sigma(s_t)
\end{aligned}
$$

Or, in our explicit state form:

$$
f_{W_x, W_s, \mathbf{b}}(\mathbf{x}, \mathbf{s}) =
  ( W_x \mathbf{x} + W_s \mathbf{s} + \mathbf{b}
  , \sigma(W_x \mathbf{x} + W_s \mathbf{s} + \mathbf{b})
  )
$$

### The connection

This is nice and all, but these stateful models seem to be at odds with our
previous picture of models.

1.  They aren't stated in the same way.  They require specifying a state of
    some sort, and also a modified state
2.  These can't be *trained* in the same way (using stochastic
    gradient descent), and look like they require a different algorithm for
    training.

However, because these are all *just functions*, we can really just manipulate
them as normal functions and see that the two aren't too different at all.

Functional Stateful Models
--------------------------

Alright, so what does this mean, and how does it help us?

To help us see, let's try implementing this in Haskell:

```haskell
!!!functional-models/model.hs "type ModelS"
```

We can implement AR(2) as mentioned before by translating the math formula
directly:

```haskell
!!!functional-models/model.hs "ar2 ::"
```

Our implementation of a fully-connected recurrent neural network is a similar
direct translation:

```haskell
!!!functional-models/model.hs "fcrnn"
```

Because we again have normal functions, we can write a similar stateful model
composition function that combines both their parameters and their states:

```haskell
!!!functional-models/model.hs "(<*~*)"
```

(`(#&)` will take two `BVar`s of values and tuple them back up into a `BVar`
of a tuple, essentially the inverse of `^^. t1` and `^^. t2`)

And maybe even a utility function to map a function on the result of a
`ModelS`:

```haskell
!!!functional-models/model.hs "mapS"
```

With this we can do some neat things like define a two-layer fully-connected
recurrent neural network.

```haskell
ghci> twoLayerRNN = fcrnn @10 @5 <*~* mapS logistic (fcrnn @20 @10)
```

Hey, maybe even a three-layer one:

```haskell
ghci> threeLayers = fcrnn @10 @5
               <*~* mapS logistic (fcrnn @20 @10)
               <*~* mapS logistic (fcrnn @40 @20)
```

### Let there be State

Because these are all just normal functions, we can manipulate them just like
any other function using higher order functions.

For example, we can "upgrade" any non-stateful function to a stateful one, just
by returning a new normal function:

```haskell
!!!functional-models/model.hs "toS"
```

This means we can make a hybrid "recurrent" and "non-recurrent" neural network:

```haskell
ghci> hybrid = toS @_ @NoState (feedForwardLog' @20 @10)
          <*~* mapS logistic (fcrnn @20 @10)
          <*~* mapS logistic (fcrnn @40 @20)
```

We made a dummy type `NoState` to use for our stateless model

```haskell
!!!functional-models/model.hs "data NoState"
```

But we can also be creative with our combinators, as well, and write one to
compose a stateless model with a stateful one:

```haskell
!!!functional-models/model.hs "(<*~)"
```

```haskell
ghci> hybrid = feedForwardLog' @20 @10
          <*~  mapS logistic (fcrnn @20 @10)
          <*~* mapS logistic (fcrnn @40 @20)
```


Everything is just your simple run-of-the-mill function composition and higher
order functions that Haskellers use every day, so there are many ways to do
these things --- just like there are many ways to manipulate normal functions.

### Unrolling in the Deep (Learning)

There's something neat we can do with stateful functions --- we can
"[unroll][]" them by explicitly propagating their state through several inputs.

[unroll]: https://machinelearningmastery.com/rnn-unrolling/

This is illustrated very well by [Christopher Olah][colah], who made a diagram
that illustrates the idea very well:

![Christopher Olah's RNN Unrolling Diagram](/img/entries/functional-models/RNN-general.png "Unrolled RNN")

If we look at each one of those individual boxes, they all have two inputs
(normal input, and previous state) and two outputs (normal output, new state).

"Unrolling" a stateful model means taking a model that takes in an `X` and
producing a `Y` and turning it into a model that takes an `[X]` and produces a
`[Y]`, by feeding it each of the `X`s one after the other, propagating the
state, and collecting all of the `Y` responses.

The "type" of this sounds like:

```haskell
unroll :: Model p s a b -> Model p s [a] [b]
```

In writing this out as a type, we also note that the `p` parameter type is the
same, and the `s` state type is the same.  (Aren't types nice?  They force you
to have to think about subtle things like this)  If you're familiar with
category theory, this looks a little bit like a sort of "fmap" under a `Model p
s` category -- it takes a (stateful and backpropagatable) `a -> b` and turns it
into an `[a] -> [b]`.

Olah's post suggests that this is a `mapAccum`, in functional programming
parlance.  And, surely enough, we can actually write this as a `mapAccumL`.

`mapAccumL` is sort of like a combination of a `foldl` and a `map`:

```haskell
mapAccumL
    :: Traversable t
    => (a -> b -> (a, c))
    -> a
    -> t b
    -> (a, t c)
```

Compare to `foldl`:

```haskell
foldl
    :: Foldable t
    => (a -> b -> a)
    -> a
    -> t b
    -> a
```

You can see that `mapAccumL` is just `foldl`, except the folding function emits
an extra `c` for every item, so `mapAccumL` can return a new `t c` with all of
the emitted `c`s.

The *backprop* library has a "lifted" `mapAccumL` in in the
*[Prelude.Backprop][prelude]* module that we can use:

[prelude]: http://hackage.haskell.org/package/backprop/docs/Prelude-Backprop.html

```haskell
mapAccumL
    :: Traversable t
    => (BVar z a -> BVar z b -> (BVar z a, BVar z c))
    -> BVar z a
    -> BVar z (t b)
    -> (BVar z a, BVar z (t c))
```

It is lifted to work with `BVar`s of the items instead of directly on the
items.  With that, we can write `unroll`, which is just a thin wrapper over
`mapAccumL`:

```haskell
!!!functional-models/model.hs "unroll"
```

This reveals that `unroll` from the machine learning is really *just*
`mapAccumL` from functional programming.

We can also tweak `unroll`'s result a bit to get a version of `unroll` that
shows only the "final" result:

```haskell
!!!functional-models/model.hs "unrollLast"
```

To see how this applies to our `threeLayer`:

```haskell
threeLayers            :: ModelS _ _ (R 40) (R 5)
unroll threeLayers     :: ModelS _ _ [R 40] [R 5]
unrollLast threeLayers :: ModelS _ _ [R 40] (R 5)
```

Aren't statically typed languages great?

### State-be-gone

Did you enjoy the detour through stateful time series models?

Good!  Because the whole point of it was to talk about how we can *get rid of
state* and bring us back to our original models!

You knew this had to come, because all of our methods for "training" these
models and learn these parameters involves non-stateful models.  Let's see now
how we can turn our functional stateful models into functional non-stateful
models!

One way is to *fix the initial state and throw away the resulting one*. This is
very common in machine learning contexts, where many people simply fix the
initial state to be a zero vector.

```haskell
!!!functional-models/model.hs "fixState" "zeroState"
```

We use `auto :: a -> BVar z a` again to introduce a `BVar` of our initial
state, but to indicate that we don't expect to track its gradient.  `zeroState`
is a nice utility combinator for a common pattern.

Another way is to *treat the initial state as a trainable parameter* (and also
throw away the final state).  This is not done as often, but is still common
enough to be mentioned often.  And, it's just as straightforward!

```haskell
!!!functional-models/model.hs "trainState"
```

`trainState` will take a model with trainable parameter `p` and state `s`, and
turn it into a model with trainable parameter `p :& s`, where the `s` is the
(trainable) initial state.

We can now *train* our recurrent/stateful models, by **unrolling and
de-stating**:

```haskell
threeLayers                        :: ModelS _ _ (R 40) (R 5)
unrollLast threeLayers             :: ModelS _ _ [R 40] (R 5)
zeroState (unrollLast threeLayers) :: Model  _   [R 40] (R 5)
```

`zeroState (unrollLast threeLayers)` is now a normal stateless (and trainable)
model.  It takes a list of inputs `R 40`s and produces the "final output" `R
5`.  We can now train this by feeding it with `([R 40], R 5)` pairs: give a
history and an expected next output.

### Show me a Sine

Let's see if we can train a two-layer fully connected neural network with 30
hidden units, where the first layer is fully recurrent, to learn how to model
a sine wave:

```haskell
-- sine signal with period 25
ghci> series = [ sin (2 * pi * t / 25) | t <- [0..]              ]

-- chunks of runs and "next results"
ghci> samps  = [ (init c, last c)      | c <- chunksOf 19 series ]

-- first layer is RNN, second layer is normal ANN, 30 hidden units
ghci> let rnn :: ModelS _ _ (R 1) (R 1)
          rnn = feedForward @30 @1 <*~ mapS logistic (fcrnn @1 @30)

ghci> trained <- trainModelIO (zeroState (unrollLast rnn)) $ take 10000 samps
```


Trained!  `trained` is now the weight and bias matrices and vectors that will
simulate a sine wave of period 25.

We can run this model iteratively upon itself to test it; if we plot the
results, we can visually inspect it to see if it has learned things properly.

Let's define some helper functions to test our model.  First, a function
`prime` that takes a stateful model and gives a "warmed-up" state by running it
over a list of inputs.  This serves to essentially initialize the memory of the
model.

```haskell
!!!functional-models/model.hs "prime"
```

Then a function `feedback` that iterates a stateful model over and over
again by feeding its previous output as its next input:

```haskell
!!!functional-models/model.hs "feedback"
```

Now let's prime our trained model over the first 19 items in our sine wave and
start it running in feedback mode on the 20th item!

```haskell
ghci> let primed = prime    rnn trained 0      (take 19 series)
ghci> let output = feedback rnn trained primed (series !! 19)
ghci> mapM_ print $ take 200 output
(-0.9980267284282716 :: R 1)
(-0.9530599469923343 :: R 1)
(-0.855333250123637 :: R 1)
(-0.7138776465246676 :: R 1)
(-0.5359655931506458 :: R 1)
-- ...
```

Plotting the result against the "actual" sine wave of period 25, we see that it
approximates the process decently well, with a consistent period:

![FCRNN Sine Wave](/img/entries/functional-models/rnnsin.png "FCRNN Sine Wave")

Looks a bit "unreasonably effective", eh?

For kicks, let's see if we can do any better with the simpler AR(2) model from
before.  Applying all we just used to `ar2`, we see:

```haskell
ar2                        :: ModelS _ _  Double  Double
unrollLast ar2             :: ModelS _ _ [Double] Double
zeroState (unrollLast ar2) :: Model  _   [Double] Double
```

`zeroState (unrollLast ar2)` is now a trainable stateless model.  Will it model
a sine wave?

```haskell
ghci> trained <- trainModelIO (zeroState (unrollLast ar2)) $ take 10000 samps
ghci> let primed = prime    rnn trained 0      (take 19 series)
ghci> let output = feedback rnn trained primed (series !! 19)
ghci> mapM_ print $ take 200 output
(-0.9980267284282716 :: R 1)
(-0.9530599469923343 :: R 1)
(-0.855333250123637 :: R 1)
(-0.7138776465246676 :: R 1)
(-0.5359655931506458 :: R 1)
-- ...
```

We can plot the result and see that it more or less perfectly models the sine
wave of period 25:

![AR(2) Sine Wave](/img/entries/functional-models/ar2sin.png "AR Sine Wave")

You can't even visually see the difference!

We can peek inside the parameterization of our learned AR(2):

```haskell
ghci> trained
-2.4013298985824788e-12 :& (1.937166322256747 :& -0.9999999999997953)
-- approximately
0.00 :& (1.94 :& -1.00)
```

Meaning that the gradient descent has concluded that our AR(2) model is:

$$
y_t = 0 + 1.94 y_{t - 1} - y_{t - 2}
$$

This toy situation appears to do much better than our RNN model, but we have to
give the RNN a break --- all of the information has to be "squished" into
essentially 30 bits, which might impact the model's accuracy.

Functions all the way down
--------------------------

Again, it is very easy to look at something like

```haskell
feedForward @30 @1 <*~ mapS logistic (fcrnn @1 @30)
```

and write it off as some abstract API of opaque data types.  Some sort of
object keeps track of state, and the object has some nice abstracted
interface...right?

But, nope, again, it is all just normal functions that we wrote using normal
function composition.   We define our model as a *function*, and the backprop
library turns that function into a trainable model.

### Combinator Fun

I really like how we have pretty much free reign over how we can combine and
manipulate our models, since they are just functions.

Here's one example of how the freedom that "normal functions" gives you can
help reveal insight.  I stumbled upon an interesting way of defining recurrent
neural networks --- a lot of times, a "recurrent neural network" really just
means that some function of the *previous* output is used as an "extra input".

This sounds like we can really write a recurrent model as a "normal" model, and
then use a combinator to feed it back into itself.

To say in types:

```haskell
recurrently
    :: Model  p   (a :& b) b
    -> ModelS p b  a       b
```

A "normal, non-stateful model" taking an `a :& b` and returning a `b` can
really be turned into a stateful model with state `b` (the *previous output*)
and only taking in an `a` input.

This sort of combinator is a joy to write in Haskell because it's a "follow the
types" kinda deal --- you set up the function, and the compiler pretty much
writes it for you, because the types guide the entire implementation:

```haskell
!!!functional-models/model.hs "recurrently"
```

In general though, it'd be nice to have *some function* of the previous output
be stored as the state.  We can write this combinator as well, taking the
function that transforms the previous output into the stored state:

```haskell
!!!functional-models/model.hs "recurrentlyWith"
```

Again, once we figure out the *type* our combinator has...the function writes
itself.  The joys of Haskell!

`recurrentlyWith` takes a `c -> b` function and turns a pure model taking
an `a :& b` into a stateful model with state `b` taking in an `a`.  The `c ->
b` tells you how to turn the previous output into the new state.

To me, `recurrentlyWith` captures the "essence" of what a recurrent model or
recurrent neural network is --- the network is allowed to "see" its previous
output.

And the piece de resistance --- we can use this to define a fully connected
recurrent neural network layer as simply a recurrent version of a normal fully
connected feed-forward layer.

We can redefine a pre-mapped version of `feedForward` which takes a tuple of
two vectors and concatenates them before doing anything:

```haskell
-- | Concatenate two vectors
(#)          :: BVar s (R i) -> BVar s (R o) -> BVar s (R (i + o))

!!!functional-models/model.hs "ffOnSplit"
```

`ffOnSplit` is a feed-forward layer taking an `R (i + o)`, except we pre-map it
to take a tuple `R i :& R o` instead.

Now our fully connected recurrent layer is just `recurrentlyWith logistic
ffOnSplit`:

```haskell
fcrnn'
    :: (KnownNat i, KnownNat o)
    => ModelS _ (R o) (R i) (R o)
fcrnn' = recurrentlyWith logistic ffOnSplit

```

Basically just a recurrent version of `feedForward`!  If we abstract out some of
the manual uncurrying and pre-mapping, we get a nice functional definition:

```haskell
!!!functional-models/model.hs "fcrnn'"
```

There are many more such combinators possible!  Combinators like
`recurrentlyWith` just scratch the surface.  Best of all, they help reveal to
us that seemingly exotic things really are just simple applications of
combinators from other basic things.


<div class="note">
**Aside: Unified Representation**

This is a small aside for those familiar with Haskell techniques like DataKinds
and dependent types!

One ugly thing you might have noticed was that we had to give different "types"
for both our `Model` and `ModelS`, so we cannot re-use useful functions on
both.  For example, `mapS` only works on `ModelS`, but not `Model`.  `(<~)`
only works on `Model`s, `(<*~*)` only works on two `ModelS`s, and we had to
define a different combinator `(<*~)`.

This is not a fundamental limitation!  With *DataKinds* and dependent types we
can unify these both under a common type.  If we had:

```haskell
type Model (p :: Type) (a :: Type) (b :: Type) =
       forall z. Reifies z W
    => BVar z p
    -> BVar z a
    -> BVar z b

type ModelS (p :: Type) (s :: Type) (a :: Type) (b :: Type) =
       forall z. Reifies z W
    => BVar z p
    -> BVar z a
    -> BVar z s
    -> (BVar z b, BVar z s)
```

We can unify them by making `s` be optional, a `Maybe Type`, and using the
`Option` type from *[Data.Type.Option][]*, from the *[type-combinators][]*
package:

[Data.Type.Option]: https://hackage.haskell.org/package/type-combinators/docs/Data-Type-Option.html
[type-combinators]: https://hackage.haskell.org/package/type-combinators

```haskell
type Model' (p :: Type) (s :: Maybe Type) (a :: Type) (b :: Type) =
       forall z. Reifies z W
    => BVar z p
    -> BVar z a
    -> Option (BVar z) s
    -> (BVar z b, Option (BVar z) s)
```

`Option f a` contains a value if `a` is `'Just`, and does not if `a` is
`'Nothing`.

We can then re-define our previous types:

```haskell
type Model  p   = Model' p 'Nothing
type ModelS p s = Model' p ('Just s)
```

And now that we have unified everything under the same type, we can write
`mapS` that takes both stateful and non-stateful models, merge `(<~)`, `(<*~*)`
and `(<*~)`, etc., thanks to the power of dependent types.

Note that dependent types and DataKind shenanigans aren't necessary for any of
this to work --- it just has the possibility to make things even more seamless
and unified!
</div>

A Path Forward
--------------

Thank you for making it to the end!  I hope at this point you have been able to
gain some appreciation for differential programming in a purely functional
style, and see the sort of doors that this opens.

A lot of things come together to make all of this work:

1.  *Functional programming*, allowing us to write higher-order functions and
    combinators that take functions and return functions.  This is the entire
    crux of this approach, and lets us not only draw from mathematical models
    directly, but also combine and reshape models in arbitrary ways just by
    using normal function composition and application, instead of being forced
    into a rigid compositional model.

    We were able to chain, fork, recombine models by just writing normal
    higher-order functions.

2.  *Differentiable* programs, allowing us to write normal functions and have
    them be automatically differentiable for gradient descent.

3.  *Purely* functional programming.  If *any* of these functions were
    side-effecting and impure functions, the correspondence between functions
    and mathematical models completely falls apart.  This is something we often
    take for granted when writing Haskell, but in other languages, without
    purity, no model is sound.

4.  A *strong expressive static type system* makes this all reasonable to work
    with. A strong type system tells us how we are allowed to combine outputs
    and inputs of functions, how we can combine parameters, what values
    parameters contains, what parameters a given model contains, etc.; without
    this knowledge, it would be impossible to sanely write complex programs.

    This forces us to be aware of what parameters we have, how they
    combine, etc.; this is what makes combinators like `recurrent` and `unroll`
    and `zeroState` reasonable: the *compiler* is able to trace how we move
    around our parameter and state, so that we don't have to.  It lets us ask
    questions like "what is the state, now?" if we needed, or "what is the
    parameter now?".  Remember how we were able to trace out the unrolling and
    zeroing process:

    ```haskell
    ar2                        :: ModelS _ _  Double  Double
    unrollLast ar2             :: ModelS _ _ [Double] Double
    zeroState (unrollLast ar2) :: Model  _   [Double] Double
    ```

    The fact that this all exists within our language is very powerful.

    We sometimes even gained insight simply from thinking, in advance, what the
    types of our combinators were.  And, if we can phrase our combinators in
    terms of our types, the compiler will often be able to write our entire
    program for us --- something only possible for statically typed languages.

If you drop any one of these pieces, you are left with something very clumsy as
a result.  In an imperative or object-oriented setting with inexpressive or
dynamic type system would render this approach almost infeasible.  I really
feel like, after working with these types and these sorts of models, we are
peering into the future of machine learning's gradient-trainable models.
