A Purely Functional Typed Approach to Trainable Models (Part 2)

================================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on May 14, 2018.
> [Read online!](https://blog.jle.im/entry/purely-functional-typed-models-2.html)

Welcome back! We're going to be jumping right back into describing a vision of a
purely functional typed approach to writing trainable models using
differentiable programming. If you're just joining us, be sure to check out
[Part 1](https://blog.jle.im/entry/purely-functional-typed-models-1.html) first!

In the last post, we looked at models as "question and answer" systems. We
described them as essentially being functions of type

$$
f : P \rightarrow (A \rightarrow B)
$$

Where, for $f_p(x) = y$, you have a "question" $x : A$ and are looking for an
"answer" $y : B$. Picking a *different* $p : P$ will give a *different* $A
\rightarrow B$ function. We claimed that training a model was finding just the
right $p$ to use with the model to yield the right $A \rightarrow B$ function
that models your situation.

We then noted that if you have a set of `(a, b)` observations, and your function
is differentiable, you can find the *gradient* of `p` with respect to the error
of your model on each observation, which tells you how to nudge a given `p` in
order to reduce how wrong your model is for that observation. By repeatedly
making observations and taking those nudges, you can arrive at a suitable `p` to
model any situation.

This is great if we consider a model as "question and answer", but sometimes
things don't fit so cleanly. Today, we're going to be looking at a whole
different type of model ("time series" models) and see how they are different,
but also how they are really the same.

For following along, the source code for the written code in this module is all
available [on
github](https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs).

## Time Series Models

In the wild, many models are not simple "question and answer", but rather
represent a "time series". As a generalization, we can talk about time series
models as:

$$
f_p(x,t) = y
$$

Which says, given an input and a time, return an output based on both. The point
of this is to let us have recurrent relationships, like for [autoregressive
models](https://en.wikipedia.org/wiki/Autoregressive_model) found in statistics:

$$
\text{AR}_{\phi_1, \phi_2, \ldots}(x,t)
  = \epsilon_t + \phi_1 \text{AR}_{\phi_1, \phi_2, \ldots}(x, t-1)
  + \phi_2 \text{AR}_{\phi_1, \phi_2, \ldots}(x, t-2)
  + \ldots
$$

However, this is a bad way of *implementing* models on time series, because
nothing is stopping the result of a model from depending on a future value (the
value at time $t = 3$, for instance, might depend explicitly only the value at
time $t = 5$). Instead, we can imagine time series models as explicitly
"stateful" models:

$$
f_p(x, s_{\text{old}}) = (y, s_{\text{new}})
$$

These have type:[^1]

$$
f : (P \times A \times S) \rightarrow (B \times S)
$$

This makes it clear that the output of our model can only depend on current and
*previously occurring* information, preserving causality.

### Examples

We can use this to represent an AR(2) model ([autoregressive model with degree
2](https://en.wikipedia.org/wiki/Autoregressive_model)), which is a model whose
output forecast is a linear regression on the *last two* most recent observed
values. We can do this by setting the "input" to be the last observed value, and
the "state" to be the second-to-last observed value:

$$
\begin{aligned}
s_t & = x_t \\
y_t & = c + \phi_1 s_t + \phi_2 s_{t - 1}
\end{aligned}
$$

Or, in our function form:

$$
f_{c, \phi_1, \phi_2}(x, s) = (c + \phi_1 x + \phi_2 s, x)
$$

There's also the classic [fully-connected recurrent neural network
layer](http://karpathy.github.io/2015/05/21/rnn-effectiveness/), whose output is
a linear combination of the (logistic'd) previous output and the current input,
plus a bias:

$$
\begin{aligned}
s_t & = \sigma(y_t) \\
y_t & = W_x \mathbf{x}_t + W_s \mathbf{s}_{t-1} + \mathbf{b}
\end{aligned}
$$

Or, in our function form:

$$
f_{W_x, W_s, \mathbf{b}}(\mathbf{x}, \mathbf{s}) =
  ( W_x \mathbf{x} + W_s \mathbf{s} + \mathbf{b}
  , \sigma(W_x \mathbf{x} + W_s \mathbf{s} + \mathbf{b})
  )
$$

### The connection

This is nice and all, but these stateful models seem to be at odds with our
previous picture of models.

1.  They aren't stated in the same way. They require specifying a state of some
    sort, and also a modified state
2.  These can't be *trained* in the same way (using stochastic gradient
    descent), and look like they require a different algorithm for training.

However, because these are all *just functions*, we can really just manipulate
them as normal functions and see that the two aren't too different at all.

## Functional Stateful Models

Alright, so what does this mean, and how does it help us?

To help us see, let's try implementing this in Haskell. Remember our previous
`Model` type:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L52-L55

type Model p a b = forall z. Reifies z W
                => BVar z p
                -> BVar z a
                -> BVar z b
```

which represented a differentiable $f : (P \times A) \rightarrow B$. We can
directly translate this to a new `ModelS` type:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L157-L161

type ModelS p s a b = forall z. Reifies z W
                   => BVar z p
                   -> BVar z a
                   -> BVar z s
                   -> (BVar z b, BVar z s)
```

which represents a differentiable $f : (P \times A \times S) \rightarrow (B
\times S)$.

We can implement AR(2) as mentioned before by translating the math formula
directly:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L163-L165

ar2 :: ModelS (Double :& (Double :& Double)) Double Double Double
ar2 (c :&& (φ1 :&& φ2)) yLast yLastLast =
    ( c + φ1 * yLast + φ2 * yLastLast, yLast )
```

Our implementation of a fully-connected recurrent neural network is a similar
direct translation:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L167-L172

fcrnn
    :: (KnownNat i, KnownNat o)
    => ModelS ((L o i :& L o o) :& R o) (R o) (R i) (R o)
fcrnn ((wX :&& wS) :&& b) x s = ( y, logistic y )
  where
    y  = (wX #> x) + (wS #> s) + b
```

Because we again have normal functions, we can write a similar stateful model
composition function that combines both their parameters and their states:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L174-L183

(<*~*)
  :: (Backprop p, Backprop q, Backprop s, Backprop t)
    => ModelS  p        s       b c
    -> ModelS       q        t  a b
    -> ModelS (p :& q) (s :& t) a c
(f <*~* g) (p :&& q) x (s :&& t) = (z, s' :&& t')
  where
    (y, t') = g q x t
    (z, s') = f p y s
infixr 8 <*~*
```

(Here we use our handy `(:&&)` pattern to construct a tuple, taking a `BVar z a`
and a `BVar z b` and returning a `BVar z (a :& b)`)

And maybe even a utility function to map a function on the result of a `ModelS`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L185-L189

mapS
    :: (forall z. Reifies z W => BVar z b -> BVar z c)
    -> ModelS p s a b
    -> ModelS p s a c
mapS f g p x = first f . g p x
```

With this we can do some neat things like define a two-layer fully-connected
recurrent neural network.

``` haskell
ghci> let twoLayerRNN :: ModelS _ _ (R 20) (R 5)
          twoLayerRNN = fcrnn @10 <*~* mapS logistic fcrnn
```

(Again using type application syntax with `@10` to specify our hidden layer
size, and the type wildcard syntax `_` to let the compiler fill in the parameter
and state type for us)

Hey, maybe even a three-layer one:

``` haskell
ghci> let threeLayers :: ModelS _ _ (R 40) (R 5)
          threeLayers = fcrnn @10
                   <*~* mapS logistic (fcrnn @20)
                   <*~* mapS logistic fcrnn
```

### Let there be State

Because these are all just normal functions, we can manipulate them just like
any other function using higher order functions.

For example, we can "upgrade" any non-stateful function to a stateful one, just
by returning a new normal function:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L191-L193

toS :: Model  p   a b
    -> ModelS p s a b
toS f p x s = (f p x, s)
```

This means we can make a hybrid "recurrent" and "non-recurrent" neural network,
by making `feedForwardLog'` a model with some dummy state (like `()` perhaps),
and re-using `(<*~*)`.

But we can also be creative with our combinators, as well, and write one to
compose a stateless model with a stateful one:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L195-L201

(<*~)
  :: (Backprop p, Backprop q)
    => Model   p         b c
    -> ModelS       q  s a b
    -> ModelS (p :& q) s a c
(f <*~ g) (p :&& q) x = first (f p) . g q x
infixr 8 <*~
```

``` haskell
ghci> let hybrid :: ModelS _ _ (R 40) (R 10)
          hybrid = feedForwardLog' @20
              <*~  mapS logistic (fcrnn @20)
              <*~* mapS logistic fcrnn
```

Everything is just your simple run-of-the-mill function composition and higher
order functions that Haskellers use every day, so there are many ways to do
these things --- just like there are many ways to manipulate normal functions.

## Unrolling in the Deep (Learning)

There's something neat we can do with stateful functions --- we can
"[unroll](https://machinelearningmastery.com/rnn-unrolling/)" them by explicitly
propagating their state through several inputs.

This is illustrated very well by [Christopher
Olah](http://colah.github.io/posts/2015-09-NN-Types-FP/), who made a diagram
that illustrates the idea very well:

![Christopher Olah's RNN Unrolling
Diagram](/img/entries/functional-models/RNN-general.png "Unrolled RNN")

If we look at each one of those individual boxes, they all have two inputs
(normal input, and previous state) and two outputs (normal output, new state).

"Unrolling" a stateful model means taking a model that takes in an `X` and
producing a `Y` and turning it into a model that takes an `[X]` and produces a
`[Y]`, by feeding it each of the `X`s one after the other, propagating the
state, and collecting all of the `Y` responses.

The "type" of this sounds like:

``` haskell
unroll :: Model p s a b -> Model p s [a] [b]
```

In writing this out as a type, we also note that the `p` parameter type is the
same, and the `s` state type is the same. (Aren't types nice? They force you to
have to think about subtle things like this) If you're familiar with category
theory, this looks a little bit like a sort of "fmap" under a `Model p s`
category -- it takes a (stateful and backpropagatable) `a -> b` and turns it
into an `[a] -> [b]`.

Olah's post suggests that this is some sort of `mapAccum`, in functional
programming parlance. And, surely enough, we can actually write this as a
`mapAccumL`.

`mapAccumL` is sort of like a combination of a `foldl` and a `map`:

``` haskell
mapAccumL
    :: (a -> b -> (a, c))
    -> a
    -> [b]
    -> (a, [c])
```

Compare to `foldl`:

``` haskell
foldl
    :: (a -> b -> a)
    -> a
    -> [b]
    -> a
```

You can see that `mapAccumL` is just `foldl`, except the folding function emits
an extra `c` for every item, so `mapAccumL` can return a new `[c]` with all of
the emitted `c`s.

The *backprop* library has a "lifted" `mapAccumL` in in the
*[Prelude.Backprop](http://hackage.haskell.org/package/backprop/docs/Prelude-Backprop.html)*
module that we can use:

``` haskell
Prelude.Backprop.mapAccumL
    :: (BVar z a -> BVar z b -> (BVar z a, BVar z c))
    -> BVar z a
    -> BVar z [b]
    -> (BVar z a, BVar z [c])
```

It is lifted to work with `BVar`s of the items instead of directly on the items.
With that, we can write `unroll`, which is just a thin wrapper over
`mapAccumL`:[^2]

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L203-L211

unroll
    :: (Backprop a, Backprop b)
    => ModelS p s  a   b
    -> ModelS p s [a] [b]
unroll f p xs s0 = swap $ B.mapAccumL f' s0 xs
  where
    -- we have to re-arrange the order of arguments and tuple a bit to
    -- match what `mapAccumL` expects
    f' s x = swap (f p x s)
```

This reveals that `unroll` from the machine learning is really *just*
`mapAccumL` from functional programming.

We can also tweak `unroll`'s result a bit to get a version of `unroll` that
shows only the "final" result. All we do is `mapS`
`last . sequenceVar :: BVar s [a] -> BVar a`, which gets the last item in a
`BVar` of a sequence.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L213-L217

unrollLast
    :: (Backprop a, Backprop b)
    => ModelS p s  a  b
    -> ModelS p s [a] b
unrollLast f = mapS (last . sequenceVar) (unroll f)
```

Alternatively, we can also recognize that `unrollLast` is really just an awkward
left fold (`foldl`) in disguise:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L219-L225

unrollLast'
    :: Backprop a
    => ModelS p s  a  b
    -> ModelS p s [a] b
unrollLast' f p xs s0 = foldl' go (undefined, s0) (sequenceVar xs)
  where
    go (_, s) x = f p x s
```

To see how this applies to our `threeLayer`:

``` haskell
threeLayers            :: ModelS _ _ (R 40) (R 5)
unroll     threeLayers :: ModelS _ _ [R 40] [R 5]
unrollLast threeLayers :: ModelS _ _ [R 40] (R 5)
```

Nice that we can trace the evolution of the types within our langage!

### State-be-gone

Did you enjoy the detour through stateful time series models?

Good --- because the whole point of it was to talk about how we can *get rid of
state* and bring us back to our original models!

You knew this day had to come, because all of our methods for "training" these
models and learn these parameters involves non-stateful models. Let's see now
how we can turn our functional stateful models into functional non-stateful
models!

One way is to *fix the initial state and throw away the resulting one*. This is
very common in machine learning contexts, where many people simply fix the
initial state to be a zero vector.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L228-L238

fixState
    :: s
    -> ModelS p s a b
    -> Model  p   a b
fixState s0 f p x = fst $ f p x (auto s0)

zeroState
    :: Num s
    => ModelS p s a b
    -> Model  p   a b
zeroState = fixState 0
```

We use `auto :: a -> BVar z a` again to introduce a `BVar` of our initial state,
but to indicate that we don't expect to track its gradient. `zeroState` is a
nice utility combinator for a common pattern.

Another way is to *treat the initial state as a trainable parameter* (and also
throw away the final state). This is not done as often, but is still common
enough to be mentioned often. And, it's just as straightforward!

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L240-L244

trainState
    :: (Backprop p, Backprop s)
    => ModelS  p    s  a b
    -> Model  (p :& s) a b
trainState f (p :&& s) x = fst $ f p x s
```

`trainState` will take a model with trainable parameter `p` and state `s`, and
turn it into a model with trainable parameter `p :& s`, where the `s` is the
(trainable) initial state.

We can now *train* our recurrent/stateful models, by **unrolling and
de-stating**:

``` haskell
threeLayers                        :: ModelS _ _ (R 40) (R 5)
unrollLast threeLayers             :: ModelS _ _ [R 40] (R 5)
zeroState (unrollLast threeLayers) :: Model  _   [R 40] (R 5)
```

`zeroState (unrollLast threeLayers)` is now a normal stateless (and trainable)
model. It takes a list of inputs `R 40`s and produces the "final output" `R 5`.
We can now train this by feeding it with `([R 40], R 5)` pairs: give a history
and an expected next output.

It's again nice here how we can track the evolution of the types of out model's
inputs and outputs within the language. Unrolling and zeroing is a non-trivial
interaction, so the ability to have the language and compiler track the
resulting shapes of our models is a huge advantage.

### The Unreasonably Effective

Let's see if we can train a two-layer fully connected neural network with 30
hidden units, where the first layer is fully recurrent, to learn how to model a
sine wave:

``` haskell
-- sine signal with period 25
ghci> series = [ sin (2 * pi * t / 25) | t <- [0..]              ]

-- chunks of runs and "next results"
ghci> samps  = [ (init c, last c)      | c <- chunksOf 19 series ]

-- first layer is RNN, second layer is normal ANN, 30 hidden units
ghci> let rnn :: ModelS _ _ (R 1) (R 1)
          rnn = feedForward @30 <*~ mapS logistic fcrnn

ghci> trained <- trainModelIO (zeroState (unrollLast rnn)) $ take 10000 samps
```

Trained! `trained` is now the weight and bias matrices and vectors that will
simulate a sine wave of period 25.

We can run this model iteratively upon itself to test it; if we plot the
results, we can visually inspect it to see if it has learned things properly.

Let's define some helper functions to test our model. First, a function `prime`
that takes a stateful model and gives a "warmed-up" state by running it over a
list of inputs. This serves to essentially initialize the memory of the model.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L246-L253

prime
    :: Foldable t
    => ModelS p s a b     -- ^ model
    -> p                  -- ^ parameterization
    -> s                  -- ^ initial state
    -> t a                -- ^ priming input
    -> s                  -- ^ primed state
prime f p = foldl' $ evalBP2 (\s x -> snd $ f (auto p) x s)
```

Then a function `feedback` that iterates a stateful model over and over again by
feeding its previous output as its next input:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functional-models/model.hs#L255-L267

feedback
    :: (Backprop a, Backprop s)
    => ModelS p s a a     -- ^ model
    -> p                  -- ^ parameterization
    -> s                  -- ^ initial state
    -> a                  -- ^ initial input
    -> [a]                -- ^ inifinite feedback loop
feedback f p s0 x0 = unfoldr go (x0, s0)
  where
    go (x, s) = Just (x, (y, s'))
      where
        -- 'T2' tuples up a pair of 'BVar's into a 'BVar' of a tuple
        (y, s') = evalBP (uncurry T2 . f (auto p) (auto x)) s
```

Now let's prime our trained model over the first 19 items in our sine wave and
start it running in feedback mode on the 20th item!

``` haskell
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
approximates the process decently well, with a consistent period (that is
slightly slower than the reference period):

![FCRNN Sine Wave](/img/entries/functional-models/rnnsin.png "FCRNN Sine Wave")

For kicks, let's see if we can do any better with the simpler AR(2) model from
before. Applying all we just used to `ar2`, we see:

``` haskell
ar2                        :: ModelS _ _  Double  Double
unrollLast ar2             :: ModelS _ _ [Double] Double
zeroState (unrollLast ar2) :: Model  _   [Double] Double
```

`zeroState (unrollLast ar2)` is now a trainable stateless model. Will it model a
sine wave?

``` haskell
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

``` haskell
ghci> trained
-2.4013298985824788e-12 :& (1.937166322256747 :& -0.9999999999997953)
-- approximately
0.0000 :& (1.9372 :& -1.0000)
```

Meaning that the gradient descent has concluded that our AR(2) model is:

$$
y_t = 0 + 1.9372 y_{t - 1} - y_{t - 2}
$$

The power of math!

In this toy situation, the AR(2) appears to do much better than our RNN model,
but we have to give the RNN a break --- all of the information has to be
"squished" into essentially 30 bits, which might impact the model's accuracy.

## Functions all the way down

Again, it is very easy to look at something like

``` haskell
feedForward @10 <*~ mapS logistic fcrnn
```

and write it off as some abstract API of opaque data types. Some sort of object
keeps track of state, and the object has some nice abstracted interface...right?

But, nope, again, it is all just normal functions that we wrote using normal
function composition. We define our model as a *function*, and the backprop
library turns that function into a trainable model.

### What Makes It Tick

Let's again revisit the four things I mentioned that are essential to making
this all work at the end of the last post, but update it with new observations
that we made in this post:

1.  *Functional programming* is the paradigm that allowed us to treat everything
    as normal functions, so that our combinators are all just normal
    higher-order functions.

    Our stateful models can also be combined and reshaped seamlessly in
    arbitrary ways, just like our non-stateful ones. And the fact that they are
    both normal functions means that they are built on the same underlying
    mechanic.

    We can *write* combinators like `(<*~*)` and `mapS`, but they are never
    *necessary*. They are always just *convenient*. But by writing such
    combinators, we open our mind to different ways that we can construct new
    models by simply transforming old ones.

    The revelation that an unrolled model was simply a combinator application
    came about by simply looking at the types and applying a model to a simple
    higher order function `mapAccumL` and `foldl`, which was *already written
    for us*. We were able to use *common functional programming tools* that are
    provided in standard libraries. This is only possible because our models are
    themselves functions in the same shape that those common tools already are
    built to work on.

    In addition, functional programming forces us to have *first-class state*.
    The "state" in our stateful models wasn't a property of the runtime system
    --- they were things we explicitly defined and carried. This allows us to
    write combinators that *manipulate how state works*. We can transform a
    function's state arbitrarily because the function's state is always
    something we can explicitly manipulate.

2.  *Differentiable* programs --- again, made more powerful through how well it
    integrates with functional programming techniques.

3.  *Purely* functional programming. One might have thought that writing
    "recurrent" or "stateful" models were something that imperative models
    excelled in, but we see now that in a functional setting, forcing ourselves
    to use explicit state allows us to manipulate state and state manipulation
    as a first-class citizen of our language, instead of something built-in and
    implicit and rigid.

4.  A *strong expressive static type system* ties all of this together and makes
    it possible to work in.

    This forces us to be aware of what parameters we have, how they combine,
    etc.; this is what makes combinators like `recurrent` and `unroll` and
    `zeroState` reasonable: the *compiler* is able to trace how we move around
    our parameter and state, so that we don't have to. It lets us ask *the
    compiler* questions like "what is the state type, now?" if we needed, or
    "what is the parameter type now?".

    We sometimes even gained insight simply from thinking, in advance, what the
    types of our combinators were. We had to make conscious decisions when
    writing the type of `unroll` and `zeroState`. And, if we can phrase our
    combinators in terms of our types, the compiler will often be able to write
    our entire program for us --- something only possible for statically typed
    languages.

In the [next and final
post](https://blog.jle.im/entry/purely-functional-typed-models-3.html), we'll
wrap this up by peeking into the wonderful world of functional combinators and
look at powerful ones that allow us to unify many different model types as
really just different combinator applications of the same thing. I'll also talk
about what I think are essential in building a usable framework for working with
this in practice.

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

[^1]: If you recognized our original stateless model type as `a -> Reader p b`,
    then you might have also recognized that this is the Haskell idiom
    `a -> StateT s (Reader p) b` (or `Kleisli (StateT s (Reader p)) a b`), which
    represents the notion of a "function from `a` to `b` with environment `p`,
    that takes and returns a modified version of some 'state' `s`".

[^2]: In truth, `mapAccumL` can work with any `Traversable` container, so we
    really can `unroll` over any `Traversable` container and not just lists. One
    of my favorite is the sized vectors from the
    [vector-sized](http://hackage.haskell.org/package/vector-sized) library,
    since they can enforce that the network always gets unrolled over the same
    number of items.

