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
second item with `^^. t2`, and then talk about the actual function, whose
result is `b * x + a`.  Note that, because `BVar`s have a `Num` instance, we
can use all our normal numeric operators, and the results are still
differentiable.

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

We use `constVar :: a -> BVar z a`, to lift a normal value to a `BVar` holding
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
y_t & = c + \phi_1 x_t + \phi_2 s_{t - 1}
\end{aligned}
$$

Or, in our explicit state form:

$$
f_{c, \phi_1, phi_2}(x, s) = (c + \phi_1 x + \phi_2 s, x) 
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

(`reTup` will take two `BVar`s of values and tuple them back up into a `BVar`
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

This is illustrated very well by [Christopher Olah][colah], who made a nice
diagram:

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

In writing this out as a type, we also note that the `p` parameter is the same,
and the `s` state type is the same.  If you're familiar with category theory,
this looks a little bit like a sort of "fmap" under a `Model p s` category --
it takes a (stateful and backpropagatable) `a -> b` and turns it into an `[a]
-> [b]`.

Olah's post suggests that this is a `mapAccum`, in functional programming
parlance.  And, surely enough, we can actually write this as a `mapAccumL`:

```haskell
!!!functional-models/model.hs "unroll"
```

This is *exactly* the just the normal functional programming `mapAccumL` of a
stateful function over a container.  And, `mapAccumL` is general enough to be
definable for all `Traversable` containers (not just lists)!  (We use
`mapAccumL` "lifted" for `BVar`s from the *[Prelude.Backprop][prelude]* module)

[prelude]: http://hackage.haskell.org/package/backprop/docs/Prelude-Backprop.html

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

We use `constVar :: a -> BVar s a` again to introduce a `BVar` of our initial
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

Let's see this play out with our AR(2) model:

```haskell
ar2                        :: ModelS _ _  Double  Double
unrollLast ar2             :: ModelS _ _ [Double] Double
zeroState (unrollLast ar2) :: Model  _   [Double] Double
```

`zeroState (unrollLast ar2)` is now a trainable stateless model.  Let's see if
we can use it to learn how to model a sine wave:

```haskell
-- sine signal with period 25
ghci> series = [ sin (2 * pi * t / 25) | t <- [0..]              ]
-- chunks of runs and "next results"
ghci> samps  = [ (init c, last c)      | c <- chunksOf 19 series ]
ghci> trained <- trainModelIO (zeroState (unrollLast ar2)) $ take 10000 samps
```

Trained!  `trained` is the parameterization of `ar2` that will simulate a
sine wave of period 25.

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
ghci> let primed = prime    ar2 trained 0      (take 19 series)
ghci> let output = feedback ar2 trained primed (series !! 19)
ghci> mapM_ print $ take 30 output
-0.9980267284282716
-0.9510565162972417
-0.8443279255081759
-0.6845471059406962
-0.48175367412103653
-0.24868988719256901
-3.673766846290505e-11
0.24868988711894977
0.4817536740469978
0.6845471058659982
0.8443279254326351
0.9510565162207472
0.9980267283507953
0.9822872506502898
0.9048270523889208
0.7705132427021685
0.5877852522243431
0.3681245526237731
0.12533323351198067
-0.1253332336071494
-0.36812455271766376
-0.5877852523157643
-0.7705132427900961
-0.9048270524725681
-0.9822872507291605
-0.9980267284247174
-0.9510565162898851
-0.844327925497479
-0.6845471059273313
-0.48175367410584324
```

Looks like a beautiful sine wave!  It starts out at -0.998, gradually rolls back
towards 0, cross over and peaks out at positive 0.998, then swings back around
past zero and reaches a minimum at -0.998 before swinging back again.  Pretty
much a perfect sine wave with period 25.  This is pretty much as good as it
gets, so it seems like AR(2) works pretty well!

For kicks, let's try it with a two-layer fully connected neural network with 30
hidden units, where the first layer is fully recurrent:

```haskell
-- first layer is RNN, second layer is normal ANN, 30 hidden units
ghci> let rnn :: ModelS _ _ (R 1) (R 1)
          rnn = feedForward @30 @1 <*~ mapS logistic (fcrnn @1 @30)
ghci> trained <- trainModelIO (trainZero (unrollLast rnn)) $ take 10000 samps
ghci> let primed = prime    rnn trained 0      (take 19 series)
ghci> let output = feedback rnn trained primed (series !! 19)
ghci> mapM_ print $ take 30 output
(-0.9980267284282716 :: R 1)
(-0.9530599469923343 :: R 1)
(-0.855333250123637 :: R 1)
(-0.7138776465246676 :: R 1)
(-0.5359655931506458 :: R 1)
(-0.3276007378757607 :: R 1)
(-9.49789925462907e-2 :: R 1)
(0.15326329240850092 :: R 1)
(0.4036006817890014 :: R 1)
(0.6365988256374424 :: R 1)
(0.8297644575358999 :: R 1)
(0.9644601077595601 :: R 1)
(1.0322337479560069 :: R 1)
(1.0354328415838387 :: R 1)
(0.98271304349553 :: R 1)
(0.8838820861246679 :: R 1)
(0.7469384572032421 :: R 1)
(0.5774599954294803 :: R 1)
(0.37953522246889254 :: R 1)
(0.1576543900562724 :: R 1)
(-8.079295377239147e-2 :: R 1)
(-0.32316184922616614 :: R 1)
(-0.5509792378000917 :: R 1)
(-0.7428726769386842 :: R 1)
(-0.8804772463971613 :: R 1)
(-0.9537270792131795 :: R 1)
(-0.9620288708442922 :: R 1)
(-0.9114012854243098 :: R 1)
(-0.8104104643872705 :: R 1)
(-0.6672968115106706 :: R 1)
```

Also nice, but not quite as perfect as AR(2).  It seems to overshoot the
positive peak slightly (hitting 1.03) and undershoot the negative peak (only
reaching -0.96)...but it still seems pretty nice considering that its memory
units are all sigmoidally squashed, while AR(2) gets to have a continuous
memory space.  At this point we're picking hairs of 1% difference, though!
Sounds like these RNNs have proven to be quite "unreasonably effective", eh?
