Machines (Part 1)
=================

Categories
:   Haskell
:   Ramblings
Tags
:   haskell
:   functional reactive programming
:   machines
CreateTime
:   2014/01/19 22:55:57
PostDate
:   never
Series
:   Machines
Identifier
:   machines-1

So I'm going to be running a series soon on computation and (physical)
simulations using AFRP (Arrowized Functional Reactive Programming) principles.

I consider (A)FRP to actually be a pretty game changing paradigm.  It provides
us with semantics by which to compose and build time-varying, reactive
behaviors and completely changes the way we approach any sort of
simulation/state-like project.

However, it's fairly difficult to approach the subject (and not dumb things
down to the point of magic) without a solid foundation on the basic building
blocks of AFRP: **machines**.  And so before I get started on my simulations
in FRP series, here is hopefully a practical foundation on the practical usage
and construction of the machine design pattern that we can build on to get
started on our Physics simulation!

This post will assume a somewhat basic knowledge of Haskell.  I'll try
explaining concepts here and there if I feel that they might not be very
commonly known.  But if you have any questions, feel free to leave a comment
or stop by freenode's #haskell on irc!

Composability
-------------

Why do we even bother with FRP?  Why not just wrap everything in a giant
global state monad and program imperatively?

To get to the bottom of this, we must remember why we even bother with
functional programming in the first place.  Think very hard about this
question, because without a real answer, all of this will be a waste.  Is it
just because it's cool?

The (a?) reason why (pure) functional programming is powerful is because is a
subset of what can be called **compositional programming**.  Almost all of the
benefits of functional programming can be drawn from its unmatched power of
composability.  Think of the power of unix pipes.  We can build programs by
the free composition of smaller, simpler self-contained programs and concepts
that don't have to be aware of any other part.  Recognizing this completely
changes the way we approach problems.

So why FRP?  FRP provides for us meaningful semantics by which to *compose*
time-varying and reactive behaviors from smaller ones.  You reason about a
self-contained behavior "wire"...and then use tools to build complex behaviors
from smaller ones.  You don't specify what happens when, you specify how
things *should behave*, from a top-down level.  And then compose those
behaviors.

Yadda yadda.  I am probably boring you at this point, let's just get started
and jump into machines!

Streams
-------

Let's start with streams, one of the simpler of machines.

Streams are basically infinitely long linked lists.

~~~haskell
data Stream b = SCons (b, Stream b)
~~~

(`SCons` for "Stream cons")

Compare that with the linked list data type, which is a Stream with an Ending
(`Nil`):

~~~haskell
data List a = Cons (a, List a) | Nil
  deriving (Show)
~~~

or, as is more traditionally written:

~~~haskell
data [a] = (:) a [a] | []
~~~

It's pretty easy to build lists:

~~~haskell
myList :: List Int
myList = Cons ( 1, Cons ( 2, Cons (3, Nil) ) )
~~~

which is just, in the more traditional (infix) form:

~~~haskell
myList' :: [Int]
myList' = 1:(2:(3:[]))
~~~

Let's see if `myList` does what we want: (a list from 1 to 3):

~~~haskell
λ: let (Cons (x,xs)) = myList
λ: x
1
λ: :t xs
xs :: List Int
λ: let (Cons (y,ys)) = myList
λ: y
2
λ: let (Cons (z,zs)) = myList
λ: z
3
λ: zs
Nil
~~~

Yes!  Perfect.  We can "traverse" down our linked list by repeatedly pattern
matching out the "head" (the `x`, the first part of the tuple) and the "tail"
(the `xs`, the second part of the tuple).

But how are we going to build a stream...?  We don't have access to `Nil`, so
do we have to manually type out an infinite stream?

Let's try defining the stream `[1..]` --- a stream that contains every natural
number starting from 1.

~~~haskell
myStream' :: Stream Int
myStream' = SCons ( 1, SCons ( 2, SCons ( 3, ... ) ) )
~~~

Hm.  This probably isn't going to work.  We can't type out all the numbers
from 1 to infinity.

We can take advantage of Haskell's "lazy-by-default"-ness and leave the "rest"
of the stream as an unevaluated function call.  And then we can recurse!

~~~haskell
streamFrom :: Int -> Stream Int
streamFrom n = SCons ( n, streamFrom (n+1) )

myStream :: Stream Int
myStream = streamFrom 1
~~~

Cool!  Let's see if this `myStream` really does what we want, the same way we
tested `myList`:

~~~haskell
λ: let (SCons (x, xs)) = myStream
λ: x
1
λ: :t xs
xs :: Stream Int
λ: let (SCons (y, ys)) = xs
λ: y
2
λ: let (SCons (z, zs)) = ys
λ: z
3
λ: let (SCons (j,js)) = zs
λ: j
4
λ: let (SCons (k,ks)) = js
λ: k
5
~~~

Yes, it works perfectly!  Just like in the case of List, we can "traverse"
down the stream by pattern matching out the "head" of the stream (the first
part of the tuple) and the "tail" of the stream (the second part of the
tuple).

Note that we can use some syntax trickery with our data definition of
`Stream` by adding a label to the first (and only) field:

~~~haskell
data Stream b = SCons { runStream :: (b, Stream b) }
~~~

and we can do fancy things like:

~~~haskell
λ: :t runStream
runStream :: Stream b -> (b, Stream b)
λ: let (x, xs) = runStream myStream
λ: x
1
~~~

Basically, we get for free the function `runStream`, a function that yanks the
tuple out of the stream.

One more final step --- because `Stream` has only one constructor and one
field, we can make it a `newtype`, which has the exact same semantics/usage as
a `data`, but which the compiler can more easily optimize:

~~~haskell
newtype Stream b = SCons { runStream :: (b, Stream b) }
~~~

### Streams are nice

Streams are nice!  If you've been using Haskell for any stretch of time,
you'll know that we use infinite lists all the time and to great usefulness
for the ends of expressiveness and abstraction.

In math, streams are known as a form of [Moore machines][] (albeit with
potentially infinite sets of state and output values).  They are machines that
basically progress from state to state to state to state. They just keep on
marching on...like a machine.  In `myStream`, the initial state is 1.  The
next state is 2; the next is 3, etc.  What is important is that the next state
*is a function of the current state*.  

[Moore machines]: http://en.wikipedia.org/wiki/Moore_machine

This is made very apparent in our definition of `streamFrom`:

~~~haskell
streamFrom :: Int -> Stream Int
streamFrom n = SCons ( n, streamFrom (n+1) )
~~~

The "current state" whenever we call `streamFrom n` is `n`...the "next state"
(the "initial state" of the "tail") is `n+1`.

So `myStream` is a Moore-like machine whose "next state" function is "the current
state plus one".

The problem with streams, however, is that you don't really affect their
progress once they start.  Once you start `myStream`, it'll keep on marching
on, and on, and on...you have no way to "influence" its progression *during*
its march.  The *behavior* of our stream *can't be influenced* by the
outside world in any way, once it has started.  This is a bit limiting,
because we want behaviors that can compose and interact with eachother.

And so, we have the natural generalization of streams:

Auto
----

Let's upgrade our streams, and introduce a way to affect how they progress.
Let's call it an Auto.

~~~haskell
newtype Auto a b = ACons { runAuto :: a -> (b, Auto a b) }
~~~

Now, instead of an `SCons` being just a tuple (a head-tails), an `ACons` is a
*function* that *produces* your head-tails tuple.  Before, all of our
`runStreams` produced the same tuple no matter what.  Now, our `runAuto` can
produce a different tuple based on an outside input.

This is cool!

Let's look at the type signature of Auto before we go too much futher.

In `Auto a b`, `b` is the type of your "head" and the type of the items in
your "tail".

`a` is the type of the "influencing input".

So now, we basically have a `Stream b`, except at every "step", we can
"influence" the Stream with something of type `a`.

### The Trivial Auto

Let's look at a direct "port" of our `myStream`:

~~~haskell
streamAutoFrom :: Int -> Auto a Int
streamAutoFrom n = ACons $ \_ -> ( n, streamAutoFrom (n+1) )

myStreamAuto :: Auto a Int
myStreamAuto = streamAutoFrom 1
~~~

This is kind of a dumb example, but `myStreamAuto` is just the exact same as
`myStream`.  It's an Auto, but it *ignores its influencing input*.

Let's try it out.

~~~haskell
λ: :t runAuto
runAuto :: Auto a b -> a -> (b, Auto a b)
λ: let (x, xs) = runAuto myStreamAuto undefined
λ: x
1
λ: :t xs
xs :: Auto a Int
λ: let (y, ys) = runAuto xs undefined
λ: y
2
λ: let (z, zs) = runAuto ys undefined
λ: z
3
~~~

Remember that we are really doing `(runAuto myStreamAuto) undefined`, but
because of how Haskell associates function calls, the parentheses are
unnecessary.  And besides, it kind of looks like `runAuto` is a two-parameter
function with an Auto as the first parameter and the "influence"/"input" as
its second.  Which, due to the magic of currying-by-default, it really is!

### A Non-trivial Auto

Okay, that was fun I guess.  But now let's take a first look at an auto which
"can" be influenced.

Let's have a *resettable counter*.  Kind of like `myStreamAuto`, but at every
step, you can choose to "reset" the count back to zero.  

Actually, let's just jump to something even bigger.  At every step, we can
choose to "set" the counter to whatever we like.

We can do this by having the influence/input be a `Maybe Int`.  If we want the
counter to progress normally, we pass in a `Nothing`.  If we want the counter
to reset to a number `n` of our choosing, we pass in a `Just n`

If you have been following along, this should be a pretty straightforward
upgrade of `myStreamAuto`, and I recommend trying to implement it yourself
before reading on.

Welp, here we go!

~~~haskell
settableCounterFrom n :: Int -> Auto (Maybe Int) Int
settableCounterFrom n = ACons $ \reset ->
  let c = fromMaybe n reset
  in  ( c, settableCounterFrom (c + 1) )

settableAuto :: Auto (Maybe Int) Int
settableAuto = settableCounterFrom 1
~~~

Remember that `fromMaybe :: a -> Maybe a -> a` takes a "default" value, a
Maybe value, and then returns the value inside the Maybe if it's a `Just`, or
the default value if it's a `Nothing`.

So basically, when you `runAuto` with an Auto, if you give it a `Nothing`,
it'll give you `( n, settableCounterFrom (n+1) )`.  If you give it `Just m`,
it'll give you `( m, settableCounterFrom (m+1) )`.


Cool, let's try it out.


~~~haskell
λ: let (x, xs) = runAuto settableAuto Nothing
λ: x
1
λ: let (y, ys) = runAuto xs Nothing
λ: y
2
λ: let (z, zs) = runAuto ys (Just 10)
λ: z
10
λ: let (j, js) = runAuto zs Nothing
λ: j
11
λ: let (k, ks) = runAuto js Nothing
λ: k
12
λ: let (l, ls) = runAuto ks (Just (-1))
λ: l
-1
λ: let (m, ms) = runAuto ls Nothing
λ: m
0
~~~

And there ya go.

### A Shift

Let's shift our thinking a bit.  Instead of seeing Autos as "streams you can
influence", we can think about them as "functions with state".  That is, they
are functions that carry self-contained encapsulated state *inside*
themselves.  They are still *functions* of some kind --- you put in an "input"
of type `a` and get in an "output" (the head) of type `b`.  However, every
"time" you do this, you get a different output depending on what you have
already passed in and what the internal state is.  As we will see, this
internal state is completely opaque to the world.  The world only has access
to the "output", the result.

(Remember that, because we're in a functional language, nothing is actually
really "mutable".  When we say that we have a stateful function, we really
mean that every time we "call" the function, we get back an "updated" function
with the new state that behaves differently when "called").

To put it in terms of `settableAuto`:

*   The "input" of `settableAuto` is our `Maybe Int` by which we specify to
    reset or allow to increment by one as normal.
*   The "output" of `settableAuto` is the "head" of the `ACons` that is
    returned --- the `x`, `y`, etc.  It's the `Int`, the counter.
*   The "state" of `settableAuto` is, in essense, the `n` of
    `settableCounterFrom n`.  It's the internal value by which the behavior is
    determined.  The behavior of `runAuto` depends on the `n` --- it either
    yields `n` itself and increments `n`, or ignores it.

#### The opaque state

It's a little tricky because the "output" and the "state" in our example
function seem to be exactly the same, but let's whip up a quick example where
it's a little more obvious that the state and the output are different things,
and that the state is completely opaque and encapsulated.

~~~haskell
settableCounterFromIsEven n :: Int -> Auto (Maybe Int) Bool
settableCounterFromIsEven n = ACons $ \reset ->
  let c = fromMaybe n reset
  in  ( even c, settableCounterFromIsEven (c + 1) )

settableAutoIsEven :: Auto (Maybe Int) Bool
settableAutoIsEven = settableCounterFromEven 1
~~~

So `settableCounterFromIsEven` (yeah, it has a horrible name) is the same as
`settableCounterFrom`, except instead of "yielding"/"outputting" `n`, it
outputs `even n` --- `True` if `n` is even and `False` if `n` is odd.

Here is a demonstration of its behavior.  I've abandoned the `(x,xs)` naming
in favor of some more meaningful names.

~~~haskell
λ: let (out1, auto1) = runAuto settableAutoIsEven Nothing
λ: out1
False
λ: let (out2, auto2) = runAuto auto1 Nothing
λ: out2
True
λ: let (out3, auto3) = runAuto auto2 (Just 10)
λ: out3
True
λ: let (out4, auto4) = runAuto auto3 Nothing
λ: out4
False
λ: let (out5, auto5) = runAuto auto4 Nothing
λ: out5
True
~~~

Not that there is in general really no way to ever access the `n` internally.
We only "offer" a way to "set" it using our input.

Now it should be clear the three distinct concepts --- the input, output, and
state.

*   The "input" again is a `Maybe Int` where we can choose to reset the march
    of the state.
*   The "output" here is now a `Bool` that says whether or not the internal
    state is even.
*   The "state" here is still that `n` (an `Int`), and was the same as in the
    last example.  But here it is more clear that the state is inaccessible in
    general.  We can only modify it in ways that the Auto *itself* allows our
    "input" (in this case, a setter) to modify it.  And we certainly can't
    arbitrarily "read" it.

### Autos are nice!

It should be clear now that Autos are a more or less straightforward extension
of Streams.

In fact, you might see that every `Stream b` is equivalent to `Auto () b`,
where the input is always unit.

Autos correspond loosely to the mathematical [Mealy machine][], but again with
technically potentially infinitely many possible states, input, and output
values.

[Mealy machine]: http://en.wikipedia.org/wiki/Mealy_machine

Now, we have a way to model behaviors that can somehow interact with the
outside world.

### The Accumulator

Let's try our hand at another Auto, but instead of looking at things as an
influencable and eternally marching stream, we're going to try to look at
things as a function with state that affects its output.

How about an Auto that "accumulates" and "sums up" all of its incoming inputs,
starting at 0?

~~~haskell
summer :: Auto Int Int
summer = sumFrom 0
  where
    sumFrom :: Int -> Auto Int Int
    sumFrom n = ACons $ \input ->
      let s = n + input
      in  ( s , sumFrom s )
~~~

~~~haskell
λ: let (out1, auto1) = runAuto summer 10
λ: out1
10
λ: let (out2, auto2) = runAuto auto1 3
λ: out1
13
λ: let (out3, auto3) = runAuto auto2 15
λ: out3
28
λ: let (out4, auto4) = runAuto auto3 (-17)
λ: out4
11
~~~

*   The "input" is our incoming `Int` --- 10, 3, 15, -17, etc.
*   The "output" is the accumulated sum/integral -- 10, 13, 28, 11, etc.
*   The "state" in this case is the accumulator, and stays in sync with the
    output. But remember that this is not in general the case.

Just for kicks, let's generalize this and make an Auto version of `foldl`:
give us an operator and an initial value, and we'll "fold up" all of our
inputs.

~~~haskell
autoFold :: (b -> a -> b) -> b -> Auto a b
autoFold op initial = foldFrom initial
  where
    foldFrom :: b -> Auto a b
    foldFrom x = ACons $ \input ->
      let y = x `op` input
      in  ( y, foldFrom y )
~~~

Note that `summer` then is just `autoFold (+) 0`.

You can probably imagine lots of different folds you can turn into
`autoFold`s...and indeed a lot of practical Autos are just `autoFold`s.  Here
are some cute ones:

~~~haskell
accumulateIntoList :: Auto a [a]
accumulateIntoList = autoFold (flip (:)) []

productor :: Num a => Auto a a
productor = autoFold (*) 1

accumulateStrings :: Auto String String
accumulateStrings = autoFold (++) ""

monoidAccum :: Monoid a => Auto a a
monoidAccum = autoFold mappend mempty
~~~

Cool, huh?

#### Parallel with list folds

Let's look very carefully at a comparison between the type signature of
Prelude's `foldl` and the type signature of `autoFold`:

~~~haskell
foldl      :: (b -> a -> b) -> b -> ([a] -> b)
autoFold   :: (b -> a -> b) -> b ->  Auto a b
~~~

Hm.  Let's do some rearranging.  Remember that in Haskell, `(->)` is just an
infix type operator.  So we can always rewrite `a -> b` as `(->) a b`

~~~haskell
foldl      :: (b -> a -> b) -> b -> ( (->) [a] b )
autoFold   :: (b -> a -> b) -> b -> ( Auto  a  b )
~~~

Let's get rid of some of the points, too:

~~~haskell
foldl    op initial :: (->) [a] b
autoFold op initial :: Auto  a  b
~~~

So both `foldl` and `autoFold` have very similar behaviors:

Give `foldl` or `autoFold` an *accumulating function* and an *initial value*,
and they return *a new "function thing"*.

For `foldl op initial`, this "function thing" takes a *list* of `a` values and
condenses them into a `b` value.

For `autoFold op initial`, the "function thing" takes *one* `a` value and
returns a `b` value based on the previous `a`'s it has seen.

The main point here is that `autoFold` is a sort of "function" in a way...just
like the others before it.  It's a...."function-like thing".

"Function Things"
-----------------

Which leads us to our next point.

*Autos are function-like things*.

They are functions...with state.

Let's do the same analysis for `settableAutoIsEven`.  Our "input" was a `Maybe
Int` and our "output" was a `Bool`.

You can think of `settleAutoIsEven` as a "function thing" from `Maybe Int` to
`Bool`.

Here's another function from `Maybe Int` to `Bool`: (I'm going to be using the
prefix form of `(->)` a lot now for the "final" return functions)

~~~haskell
maybeIsEven :: (->) (Maybe Int) Bool
maybeIsEven = even . fromMaybe 1
~~~

`maybeIsEven` returns `True` when value inside the `Just` is even, or `False`
if the value is odd or it's a `Nothing`.

Compare that type signature to that of our `settableAutoIsEven`

~~~haskell
maybeIsEven         :: (->) (Maybe Int) Bool
settableAutoIsEven  :: Auto (Maybe Int) Bool
~~~

`maybeIsEven` and `settableAutoIsEven` are *both* "function-like things".  But
whereas `maybeIsEven` is "memoryless" (it's the same every time you call it),
`settableAutoIsEven` *has memory* --- it returns a different Boolean based on
its history.

### "Function...things?"

You should be able to guess where I am going with this.

You should be able to guess that, after vaguely using the phrase "function
things" several times...I'm going to surprise you all with the revelation that
these "function things" have a name!  And maybe even...a typeclass?

But I'll leave this for the next part.

For now, think of what it means to be "function like".  Not only do you have
inputs and outputs...but there are also certain things about being a function
that are key to its nature.  Things like...function composition, maybe?  What
would that even look like with our autos?



<!-- ### Categories and Arrows -->

<!-- Okay, I think I've belabored the point enough.  It should come as no surprise -->
<!-- to you that this "function like" concept has a name in Haskell. -->

<!-- These "function things" are all members of the `Category` typeclass. -->
<!-- Technically, they represent morphisms in the mathematical concept of a -->
<!-- category.  To prevent confusion, I will usually refer to them as morphisms or -->
<!-- as "arrows". -->

<!-- <aside> -->
<!--     ###### Aside -->

<!-- Technically, there is actually a separate `Arrow` typeclass.  This typeclass -->
<!-- more or less provides convenient functions and combinators for chaining and -->
<!-- composing categories, so for now and forever, I will mostly use the words -->
<!-- "arrow" and "morphism" interchangeably.  And sometimes, if I slip, "category". -->
<!-- However, this distinction should be made very clear. -->
<!-- </aside> -->

<!-- #### Semantics of categories -->

<!-- So what does it mean to be a category morphism? -->

<!-- First of all, and most importantly of all, morphisms can be **composed**. -->

<!-- For example, if you have two functions `f :: (->) b c` and `g :: (->) a b`, -->
<!-- you should be able to have some way of "chaining" them --- first apply `g`, -->
<!-- then apply `f`.  Baiscally, `f . g` produces a *new* function from `a` to `c`. -->
<!-- It takes an `a` to `b` functiona and a `b` to `c` function and composes them -->
<!-- into a brand new `a` to `c` function. -->

<!-- Apart from composition, categories must also provide *identity morphisms*. -->
<!-- That is, given any morphism `f :: Category r => r a b`, your category has to -->
<!-- provide for you a left-hand "post-apply" identity `id :: r b b` such that `id -->
<!-- . f` is the same as just `f` alone, as well as a right-hand "pre-apply" -->
<!-- identity `id :: r a a` such that `f . id` is the same as just `f` alone. -->

<!-- The Category typeclass in Haskell exactly provides just the composition `(.)` -->
<!-- and the "identity generator" `id`. -->


