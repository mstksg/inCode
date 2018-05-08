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
of these different models.

[backprop]: http://hackage.haskell.org/package/backprop

I'm starting to see a picture unifying all of these models, painted in the
language of purely typed functional programming.  I'm already applying these to
models I'm using in real life and in my research, and I thought I'd take some
time to put my thoughts to writing in case anyone else finds these illuminating
or useful.

As a big picture, I really believe that a purely functional typed approach is
*the* way to move forward in the future for models like artificial neural
networks -- and that one day, object-oriented and imperative approaches will
seem quaint.

I'm not the first person to attempt to build a conceptual framework for these
types of models in a purely functional typed sense -- [Christopher Olah's
famous post][colah] comes to mind, and is definitely worth a read.  However,
Olah's post is more of an abstract piece; the approach I am describing here can
be applied *today*, to start building and *discovering* effective models and
training them.  And I have code! :)

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
being isomorphic to `a -> Reader p b` (or `Kleisli (Reader p) a b`) which
roughly represents the notion of "A function from `a` to `b` with an
'environment' of type `p`".


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
5.  Pick a new $(x, y_x)$ observation pair.

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
!!!functional-models/model.hs "linReg"
```

Here `T2 Double Double` is a tuple of two `Double`s, which contains the
parameters (`a` and `b`).  We extract the first item using `^^. _1` and the
second item with `^^. _2`, and then talk about the actual function, whose
result is `b * x + a`.  Note that, because `BVar`s have a `Num` instance, we
can use all our normal numeric operators, and the results are still
differentiable.

We can *run* `linReg` using `evalBP2`:

```haskell
ghci> evalBP2 linReg (T2 0.3 (-0.1)) 5
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

We use `constVar :: a -> BVar z a`, to lift a normal value to a `BVar` holding
that value, since our model `f` takes `BVar`s.

And finally, we can train it using stochastic gradient descent, with just a
simple fold over all observations:

```haskell
!!!functional-models/model.hs "trainModel"
```

Let's train our linear regression model to fit the points `(1,1)`, `(2,3)`,
`(3,5)`, `(4,7)`, and `(5,9)`!  This should follow $f(x) = 2 x - 1$, or
$\alpha = -1,\, \beta = 2$:

```haskell
ghci> samps = [(1,1),(2,3),(3,5),(4,7),(5,9)]
ghci> trainModel linReg (T2 0 0) (concat (replicate 1000 samps))
T2 (-1.0000000000000024) 2.0000000000000036
```

Neat!  After going through all of those observations a thousand times, the
model nudges itself all the way to the right parameters to fit our model!

The important takeaway is that all we specified was the *function* of the model
itself.  The training part all follows automatically!

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
ghci> trained = trainModel feedForwardLog (T2 0 0) (concat (replicate 10000 samps))
```

We have our trained parameters!  Let's see if they actually model "AND"?

```haskell
ghci> evalBP2 feedForwardLog trained (H.vec2 0 0)
(7.468471910660985e-5 :: R 1)
ghci> evalBP2 feedForwardLog trained (H.vec2 1 0)
(3.816205998697482e-2 :: R 1)
ghci> evalBP2 feedForwardLog trained (H.vec2 0 1)
(3.817490115313559e-2 :: R 1)
ghci> evalBP2 feedForwardLog trained (H.vec2 1 1)
(0.9547178031665701 :: R 1)
```

Close enough!

### Functional composition

Because our functions are simply just normal functions, we can create new,
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

Our model is simple:

```haskell
ghci> twoLayer = feedForwardLog' @4 @1 <~ feedForwardLog' @2 @4
```

Note we use type application syntax to specify the input/output dimensions of
`feedForwardLog'`.

We can train it on sample points:

```haskell
ghci> samps = [(H.vec2 0 0, 0), (H.vec2 1 0, 1), (H.vec2 0 1, 1), (H.vec2 1 1, 1)]
ghci> trained = trainModel twoLayer p0 (concat (replicate 10000 samps))
```

Trained.  Now, does it model "XOR"?

```haskell
ghci> evalBP2 twoLayer trained (H.vec2 0 0)
(3.0812844350410647e-2 :: R 1)
ghci> evalBP2 twoLayer trained (H.vec2 1 0)
(0.959153369985914 :: R 1)
ghci> evalBP2 twoLayer trained (H.vec2 0 1)
(0.9834757090696419 :: R 1)
ghci> evalBP2 twoLayer trained (H.vec2 1 1)
(3.6846467867668035e-2 :: R 1)
```

Not bad!

### Possibilities

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

Time Series Models
------------------

Not all models are "question and answer" models, however -- some models
represent a time series.  This is usually notated as:

As a generalization, we can talk about models that are intended to represent
time series:

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
p b`, then you might see too that this is the common Haskell idiom `a -> StateT
s (Reader p) b` (or `Kleisli (StateT s (Reader p)) a b`), which represents the
notion of a "function from `a` to `b` with environment `p`, that takes and
returns a modified version of some 'state' `s`".

This makes it clear that the output of our model can only depend on current and
*previously occurring* information, preserving causality.

### Examples

We can use this to implement a "rolling mean" model (different from the "Moving
Average" model), who sees an input and outputs the weighted average of the
input with the previous input:

$$
f_\lambda(x, s) = (\frac{x + \lambda s}{1 + \lambda}, x)
$$

This is a model parameterized by how much to weight the current input with the
previous input.

There's also the classic fully-connected recurrent neural network layer, whose
output is a combination of the previous output and the current input:

$$
f_{W_x, W_s, \mathbf{b}}(\mathbf{x}, \mathbf{s}) =
  ( W_x \mathbf{x} + W_s \mathbf{s} + \mathbf{b}
  , \sigma(W_x \mathbf{x} + W_s \mathbf{s} + \mathbf{b})
  )
$$

### The connection

These stateful models seem to be at odds with our previous picture of models.

1.  They aren't stated in the same way.  They require specifying a state of
    some sort, and also a modified state
2.  These can't be *trained* in the same way (using stochastic
    gradient descent), and look like they require a different algorithm for
    training.

However, because we just have functions, it's easy to transform non-stateful
models into stateful models, and stateful models to non-stateful models.
That's just the point of 

### Functional Stateful Models

Alright, so what does this mean, and how does it help us?

To help us, let's try implementing this in Haskell:

```haskell
!!!functional-models/model.hs "type ModelS"
```

<!-- We can also consider the AR(1) ("Autoregressive") model, whose output is a -->
<!-- function of the *previous* output: -->

<!-- $$ -->
<!-- f_\phi(x, s) = (\frac{x + \lambda s}{1 + \lambda}, x) -->
<!-- $$ -->



<!-- ### Gradient Descent -->









<!-- For the purpose of this post, a *parameterized model* is a *function* from type -->
<!-- `a` to type `b`, parameterized by *parameter* of type `param`. -->

<!-- In types, this can look like: -->

<!-- ```haskell -->
<!-- type Model param a b = param -> (a -> b) -->
<!-- ``` -->

<!-- That is, a `Model param a b` is a thing that, when given its parameters, -->
<!-- returns a function `a -> b`.  In Haskell, we often write this as: -->

<!-- ```haskell -->
<!-- type Model param a b = param -> a -> b -->
<!-- ``` -->

<!-- The idea is that you ask a *question* (the `a`) and you get an *answer* (the -->
<!-- `b`).  We call the `a` the "predictor" (or "independent variable") and the `b` -->
<!-- the "predictand" (the "independent variable"). -->

<!-- For example, a `Model P Email Double` would be a way to, given an e-mail, -->
<!-- return the probability if it is spam or not.  A `Model P Image (R 10)` would be -->
<!-- a way to, given an image, return a 10-vector of probabilities of a handwritten -->
<!-- digit the image might contain. -->

<!-- The "trick" is to find the *correct parameters* that will return the *correct -->
<!-- function*.  Every `P` you give will give a different `Email -> Double`, and -->
<!-- it's a matter of finding which `P` gives the correct `Email -> Double` to -->
<!-- predict spam reliably. -->

<!-- ### Differentiable Models -->

<!-- One way to do this is using *gradient descent*, which is: -->

<!-- 1.  Start with a guess of the parameters -->
<!-- 2.  Figure out a way to measure how "bad" your parameters are (by testing it -->
<!--     out on known input-output pairs): the *loss function*. -->
<!-- 3.  Find the *gradient* of your parameters with respect to the loss function, -->
<!--     which tells you how to "nudge" your parameters to make the loss lower. -->
<!--     (Usually done using backpropagation) -->
<!-- 4.  Nudge your parameters according to the gradient. -->
<!-- 5.  Repeat. -->

<!-- In order for this to work, we need the `param -> a -> b` function to be -->
<!-- *differentiable*.  Luckily, with the backprop library, you can specify a -->
<!-- function to be differentiable by manipulating `BVar`s of the values, instead of -->
<!-- the values directly: -->


<!-- ```haskell -->
<!-- type Model param a b = forall s. Reifies s W => -->
<!--     BVar s param -> BVar s a -> BVar s b -->
<!-- ``` -->


<!-- A `BVar s param -> BVar s a -> BVar s b` is a *differentiable* `param -> a -> -->
<!-- b`.  We can use it to find the gradient: -->

<!-- ```haskell -->
<!-- evalBP -->
<!--     :: (forall s. Reifies s W => BVar s a -> BVar s b)  -- ^ differentiable function -->
<!--     -> a                -- ^ input -->
<!--     -> b                -- ^ output -->

<!-- gradBP -->
<!--     :: (forall s. Reifies s W => BVar s a -> BVar s b)  -- ^ differentiable function -->
<!--     -> a                -- ^ input -->
<!--     -> a                -- ^ gradient of input -->
<!-- ``` -->

<!-- And, to properly do gradient descent, we also need a way to "initialize" our -->
<!-- parameters, randomly, to start the algorithm. -->

<!-- ```haskell -->
<!-- data Model param a b = Model -->
<!--     { initParam :: forall m. PrimMonad m -->
<!--                 => MWC.Gen (PrimState m) -->
<!--                 -> m param -->
<!--     , runModel  :: forall s. Reifies s W -->
<!--                 => BVar s param -->
<!--                 -> BVar s a -->
<!--                 -> BVar s b -->
<!--     } -->
<!-- ``` -->

<!-- Here we use the [mwc-random][] library to allow us to specify a random function -->
<!-- using a mutable generator seed. -->

<!-- [mwc-random]: http://hackage.haskell.org/package/mwc-random -->

<!-- ### Stateful models -->

<!-- Finally, some models don't simply predict "question and answer" -- some models -->
<!-- operate as a *time series*.  This is the basis behind recurrent neural networks -->
<!-- and ARIMA-like models.  These models can be specified as a state machine: -->

<!-- ```haskell -->
<!-- type Model state param a b = param -> (a -> state -> (b, state)) -->
<!-- ``` -->

<!-- That is, the model's "predictor" is a `(a, state) -> (b, state)`: given an -->
<!-- initial state and input, return a modified state and output. -->

<!-- State here is an abstract concept, since in real life, it's not directly -->
<!-- observable. -->

<!-- Our final data type representing models will be: -->

<!-- ```haskell -->
<!-- !!!functional-models/model.hs "data Model" -->
<!-- ``` -->

<!-- Note that we recover the original definition with -->

<!-- ```haskell -->
<!-- !!!functional-models/model.hs "type NoState" "type StatelessModel" -->
<!-- ``` -->

<!-- Where `T0` is `data T0 = T0`, a unit (`()`) type with a `Num` instance for -->
<!-- *backprop*. -->

