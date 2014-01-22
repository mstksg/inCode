Intro to Machines & Arrows (Part 1)
===================================

Categories
:   Haskell
:   Ramblings
Tags
:   haskell
:   functional reactive programming
:   machines
:   arrows
CreateTime
:   2014/01/19 22:55:57
PostDate
:   Never
Series
:   Intro to Machines and Arrows
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

In this post we are going to introduce Streams and (more importantly) Autos.
In the next, we will be looking at Autos as a member of the powerful Category
and Arrow typeclasses.  Finally, we will bring in the final machine, the Wire,
to transition into the popular AFRP library [netwire][].

[netwire]: http://hackage.haskell.org/package/netwire

Why FRP?
--------

So why do we even bother with FRP?  Why not just wrap everything in a giant
global state monad and program imperatively?

To get to the bottom of this, we must remember why we even bother with
functional programming in the first place.  Think very hard about this
question, because without a real answer, all of this will be a waste.  Is it
just because it's cool?

The (a?) reason why (pure) functional programming is powerful is because is a
subset of what can be called **[compositional programming][]**.  Almost all of the
benefits of functional programming can be drawn from its unmatched power of
composability.  Think of the power of unix pipes.  We can build programs by
the free composition of smaller, simpler self-contained programs and concepts
that don't have to be aware of any other part.  Recognizing this completely
changes the way we approach problems.

[compositional programming]: http://www.haskellforall.com/2012/08/the-category-design-pattern.html

So why FRP?  FRP provides for us meaningful semantics by which to *compose*
time-varying and reactive behaviors, and create complex ones from smaller
ones.  You reason about a self-contained behavior "wire"...and then use tools
to build complex behaviors from simple ones.  You don't specify what happens
when, you specify how things *should behave*, from a top-down level.  And then
compose those behaviors.

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
λ: let (Cons (y,ys)) = xs
λ: y
2
λ: let (Cons (z,zs)) = ys
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

Hm.  This is going to take a while.  I wonder if there's an easier way.

We can take advantage of Haskell's "lazy-by-default"-ness and leave the "rest"
of the stream as an unevaluated function call.  And then we can recurse!

~~~haskell
myStream :: Stream Int
myStream = streamFrom 1
  where
    streamFrom :: Int -> Stream Int
    streamFrom n = SCons ( n, streamFrom (n+1) )
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

One minor final touch --- because `Stream` has only one constructor and one
field, we can make it a `newtype`, which has similar usage patterns/syntax as
a `data`, but which the compiler can more easily optimize:

~~~haskell
newtype Stream b = SCons { runStream :: (b, Stream b) }
~~~

#### Automating Traversal

The repeated pattern matching is actually kind of tedious, and it'll only get
more annoying over time, so let's make a function that can automate the
pattern matching for us really quickly so that we can test it more easily.

`streamToList` will take a Stream and perform the very straightforward
conversion into an infinite list.

~~~haskell
streamToList :: Stream b -> [b]
streamToList (SCons (x, xs)) = x : streamToList xs
~~~

So now we can do:

~~~haskell
λ: take 10 $ streamToList myStream
[1,2,3,4,5,6,7,8,9,10]
~~~

Alternatively (and for reasons which will later become clear), we can also
define `testStream`, which takes a specified amount of elements and returns
also the "resulting" stream after all of those steps, and `testStream_`, which
is the same thing except that we throw away the modified stream.

~~~haskell
testStream :: Stream b -> Int -> ([b], Stream b)
testStream strm 0 = ([]  , strm )
testStream strm n = (y:ys, final)
  where
    (y , next )   = runStream  strm
    (ys, final)   = testStream next (n-1)

testStream_ :: Stream b -> Int -> [b]
testStream_ = (fst .) . testStream
~~~

~~~haskell
λ: testStream_ myStream 10
[1,2,3,4,5,6,7,8,9,10]
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
(the "initial state" of the "tail") is `n+1`.  We could have provided any
function `:: Int -> Int` there (say, `n+2`), that would be for us our "next
state" function.

So `myStream` is a Moore-like machine whose "next state" function is "the
current state plus one".

#### on State

Before we move on, let's take a slight diversion --- you can skip this whole
section if you wish, to get onto the main point.

Notice that in our last example, our "state" `n` was the same type as our
"output" `n+1`.  Is this in general the case?

The type of our stream is `Stream Int`...does `Int` refer to the state or the
output?

As it turns out, this `Int` refers to the output --- it is the type of the
"head" of the stream, and all values we will eventually grab from it.

Note however that the type of the state is not actually included in the type
signature, so it can be whatever you want!  As a trivial example, let's say
that `streamFrom` actually takes a `x :: Double` instead of an `n :: Int`, and
rounds it before it pops it out as the "head":

~~~haskell
myStream' :: Stream Int
myStream' = streamFrom' 1.0
  where
    streamFrom' :: Double -> Stream Int
    streamFrom' x = SCons ( round x, streamFrom' (x+1) )
~~~

This function now sorta behaves similarly to our original `streamFrom`...
`myStream'` would still be `Stream Int` and output ints, and we might not ever
be able to tell that the internal state of `myStream'` was "actually" a
double!

Now also observe that the internal state is actually in general *inaccessible*
and *opaque* to the outside world.  What if we had `streamFrom` simply return
whether or not `n` was even?

~~~haskell
myBoolStream :: Stream Bool
myBoolStream = boolStreamFrom 1
  where
    boolStreamFrom :: Int -> Stream Bool
    boolStreamFrom n = SCons ( even n, boolStreamFrom (n+1) )
~~~

~~~haskell
λ: take 5 $ streamToList myBoolStream
[False,True,False,True,False]
~~~

`myBoolStream` simply cycles between `False` and `True`.  Yet, it has an
internal state that is completely closed off to us that is an `Int` counting
from `1` to infinity.  We might not have ever even known.

This property --- that the states of these types of machines are hidden from
the world --- is actually going to be very useful.  Like I said before, every
machine can really be considered self-contained.  This is unlike using a State
monad-based loop, where all internal state is more or less freely viewable
manipulatable by anyone.  Here, every machine is truly its own little world.

In fact, because the type of the state is unknown and unpredictable...even if
we could "force" the state out of a stream somehow, we wouldn't even be able
to work with it in a type safe way.  The type is truly dynamic and the type of
the nth state of a stream is unknowable at compile time.

Here is a stream whose state switches from an `Int` to a `Bool` dynamically.

~~~
wackyStateStream :: Stream (Maybe Int)
wackyStateStream = wackyStateBool True
  where
    wackyStateBool :: Bool -> Stream (Maybe Int)
    wakcyStateBool False  = SCons (Nothing , wackyStateBool True)
    wackyStateBool True   = SCons (Just 100, wackyStateInt 8)

    wackyStateInt :: Int -> Stream (Maybe Int)
    wackyStateInt n
        | n `mod` 7 == 0  = SCons (Just n, wackyStateBool True)
        | otherwise       = SCons (Just (n+2), wackyStateInt (n+3))
~~~

~~~
λ: take 7 $ streamToList wackyStateStream
[Nothing, Just 100, Just 8, Just 11, Just 16, Nothing, Just 100]
~~~

### Continuing on

The problem with streams, as you might have guessed, is that you can't really
affect their progress once they start.  Once you start `myStream`, it'll keep
on marching on, and on, and on...you have no way to "influence" its
progression *during* its march.  The *behavior* of our stream *can't be
influenced* by the outside world in any way, once it has started. This is a
bit limiting, because we want behaviors that we can have interact with each
other.

And so, we have the natural generalization of streams (and the machine we will
be spending the most time looking at): Auto.

Auto
----

Let's upgrade our streams, and introduce a way to affect how they progress.
Let's call it an Auto.

~~~haskell
newtype Auto a b = ACons { runAuto :: a -> (b, Auto a b) }
~~~

Now, instead of an `SCons` containing just a tuple (a head-tails), an `ACons`
contains a *function* that *produces* your head-tails tuple.  Before, all of
our `runStreams` produced the same tuple no matter what.  Now, our `runAuto`
can produce a different tuple based on an outside input.

This is cool!

Let's look at the type signature of Auto before we go too much further.

In `Auto a b`, `b` is the type of your "head" and the type of the items in
your "tail".  It's the type of your "stream".

`a` is the type of the "influencing input".

So now, we basically have a `Stream b`, except at every "step", we can
"influence" the Stream with something of type `a`.

### A Trivial Auto

Let's look at a direct "port" of our `myStream`:

~~~haskell
myStreamAuto :: Auto a Int
myStreamAuto = streamAutoFrom 1
  where
    streamAutoFrom :: Int -> Auto a Int
    streamAutoFrom n = ACons $ \_ -> ( n, streamAutoFrom (n+1) )
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
unnecessary.  And hey, it kind of looks like `runAuto` is a two-parameter
function with an Auto as the first parameter and the "influence"/"input" as
its second.  Which, due to the magic of currying-by-default, it really is!

### A Non-trivial Auto

Okay, that was fun I guess.  But now let's take a first look at an auto which
"can" be influenced.

Let's have a *resettable counter*.  Kind of like `myStreamAuto`, but at every
step, you can choose to "reset" the count back to zero.

Actually, let's just jump to something even bigger.  At every step, we can
choose to "set" the counter to whatever int we like.

We can do this by having the influence/input be a `Maybe Int`.  If we want the
counter to progress normally, we pass in a `Nothing`.  If we want the counter
to reset to a number `n` of our choosing, we pass in a `Just n`

~~~haskell
settableAuto :: Auto (Maybe Int) Int
settableAuto = settableCounterFrom 1
  where
    settableCounterFrom :: Int -> Auto (Maybe Int) Int
    settableCounterFrom n = ACons $ \reset ->
      let c = fromMaybe n reset
      in  ( c, settableCounterFrom (c + 1) )
~~~

Remember that `fromMaybe :: a -> Maybe a -> a` takes a "default" value, a
Maybe value, and then returns the value inside the Maybe if it's a `Just`, or
the default value if it's a `Nothing`.

So basically, when you `runAuto` with the Auto, if you give it a `Nothing`,
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

#### Automatic traversals for our Autos

Again, the manual pattern matching is a little tedious so let's write us a
function to automate "progressing" down an Auto.

Like our `testStream`, `testAuto` takes an Auto.  But because every "step"
needs an input, `testAuto auto` takes a *list* that specifies the input for
every step.  `testAuto` returns the resulting collection of results, and also
the modified Auto.  `testAuto_` throws away the new Auto and just gives us the
collection.

~~~haskell
testAuto :: Auto a b -> [a] -> ([b], Auto a b)
testAuto auto []      = ([]  , auto )
testAuto auto (x:xs)  = (y:ys, final)
  where
    (y,  next ) = runAuto  auto x
    (ys, final) = testAuto next xs

testAuto_ :: Auto a b -> [a] -> [b]
testAuto_ = (fst .) . testAuto
~~~

Trying it out on `settableAuto`:

~~~
λ: testAuto settableAuto [ Nothing, Nothing, Just 10
                         , Nothing, Nothing, Just (-1)
                         , Nothing ]
[1,2,10,11,12,-1,0]
~~~

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

(Remember that, because we're in a functional language, nothing is technically
actually really "mutable".  When we say that we have a stateful function, we
really mean that every time we "call" the function, we get back an "updated"
function with the new state that behaves differently when "called").

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
function seem to be exactly the same (just like for `myStream`), but let's
whip up a quick example where it's a little more obvious that the state and
the output are different things, and that the state is completely opaque and
encapsulated.

~~~haskell
isEvenAuto :: Auto (Maybe Int) Bool
isEvenAuto = isEvenAuto 1
  where
    isEvenAutoFrom :: Int -> Auto (Maybe Int) Bool
    isEvenAutoFrom n = ACons $ \reset ->
      let c = fromMaybe n reset
      in  ( even c, isEvenAutoFrom (c + 1) )
~~~

So `isEvenAuto` is the same as `settableCounterFrom`, except instead of
"yielding"/"outputting" `n`, it outputs `even n` --- `True` if `n` is even and
`False` if `n` is odd.

Here is a demonstration of its behavior ---

~~~
λ: testAuto isEvenAuto  [ Nothing, Nothing, Just 10
                        , Nothing, Nothing, Just (-1)
                        , Nothing ]
[False,True,True,False,True]
~~~

Note that there is in general really no way to ever access the `n` internally.
It is completely sealed off from the world, except by explicit design.  Here,
we choose to only "offer" a way to "set" it using our input.

Now it the three distinct concepts --- the input, output, and state --- should
be clear.

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

#### More on state

Another short diversion with concerning state!  As we have seen, Auto's carry
all of the hidden-internal-state features of Streams.  The type of an Auto
(`Auto a b`) reveals the type of the "input" and the "ouput"...but it never
reveals nor fixes the type of the "state".  The type of the state is not only
unknown, by possibly dynamically changing over the course of the Auto's
progression.

What Auto offers over Stream is then a way for the outside world to access and
modify the state *if the Auto wants it to*.  Now, we can design Autos that,
like we have seen with `settableAuto`, we can offer limited ways to allow the
world to modify the state on our own terms.

### The Accumulator

Let's try our hand at another Auto, but instead of looking at things as an
influencable and eternally marching stream, we're going to try to look at
things as a function with state that affects its output.

How about an Auto that "accumulates" and "sums up" all of its incoming inputs,
starting at 0?  More correctly, an Auto that, given any int, "returns" the sum
of that int with all of the previous ints it has received in its lifetime.

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
λ: testAuto_ auto2 [15,-17,6,0,-1]
[28,11,17,17,16]
~~~

*   The "input" is our incoming `Int` --- 10, 3, 15, -17, etc.
*   The "output" is the accumulated sum/integral -- 10, 13, 28, 11, etc.
*   The "state" in this case is the accumulator, which in this case stays in
    sync with the output.  But remember that this is not the case in general.

Just for kicks, let's generalize this and make an Auto version of `foldl`:
give us an operator and an initial value, and we'll "fold up" all of our
inputs.

~~~haskell
autoFold :: forall a b. (b -> a -> b) -> b -> Auto a b
autoFold op init = foldFrom init
  where
    foldFrom :: b -> Auto a b
    foldFrom x = ACons $ \input ->
      let y = x `op` input
      in  ( y, foldFrom y )
~~~

(the `forall` is used with the [Scoped Type Variables][stv] extension to let
us say that the `b` we mention in the type of `foldFrom` is the same as the
`b` in the type of `autoFold`.  If we leave off the type signature of
`foldFrom`, this is not necessary)

[stv]: http://www.haskell.org/haskellwiki/Scoped_type_variables

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

#### Parallels with list folds

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
foldl    op init  :: (->) [a] b
autoFold op init  :: Auto  a  b
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

<aside>
###### Aside

Here is a quick diversion, if you're up for it.  This doesn't really have too
much to do with the rest of the post, but it'll help you test your intuition a
bit with Autos.

As an exercise, compare (and contrast) these three functions of identical type
signatures:

~~~haskell
map       f       :: [a] -> [b]
scanl     op init :: [a] -> [b]
testAuto_ auto    :: [a] -> [b]
~~~

(Assume that `scanl` does not include the initial accumulator...that is, we
are really talking about `drop 1 . scanl op init`)

Compare what they do conceptually.  Then, for fun, try implementing some of
them in terms of the other.  Which re-implementations are possible?  Which
ones arent?
</aside>

### More Auto examples

Here is an Auto that outputs a rolling average of the values it accumulates,
with a given window size.

~~~haskell
rollingAverage :: forall a. Fractional a
    => Int          -- length of the window
    -> Auto a a     -- an Auto taking an `a` and returning an average `a`
rollingAverage window = roll []
  where
    roll :: [a] -> Auto a a
    roll xs = ACons $ \val ->
      let xs' = take window $ val:xs  -- pop on the new value, drop all
                                      --   values past the window
          ave = sum xs' / genericLength xs'  -- the average
      in  ( ave, roll xs' )
~~~

~~~haskell
λ: testAuto_ (rollingAverage 4) [2,8,4,5,1,8,3,5,1,1,8,3,5,9,2]
[2.0 ,5.0 ,4.67,4.75,4.5
,4.5 ,4.25,4.25,4.25,2.5
,3.75,3.25,4.25,6.25,4.75]
~~~

Here is an Auto that is always `False`...except whenever it receives a signal
matching a given predicate (if it is "triggered"), it remains `True` for a
specified amount of time.

~~~haskell
onFor :: forall a.
     (a -> Bool)  -- test to see if an input 'triggers'
  -> Int          -- amount of time to stay True for
  -> Auto a Bool  -- An Auto that takes an `a` and returns a `Bool`
onFor p hold = wait
  where
    wait :: Auto a Bool                 -- the "waiting" state
    wait = ACons $ \input ->
      if p input                        -- if triggered,
        then (True, countdown (hold-1)) -- jump to "countdown" state
        else (False, wait)              -- otherwise, stay waiting

    countdown :: Int -> Auto a Bool     -- the "countdown" state
    countdown n = ACons $ \input ->
      if p input                        -- if re-triggered
        then (True, countdown (hold-1)) -- countdown all over again
        else
          if n == 1
            then (False, wait)          -- If counted down, go wait again
            else (True, countdown (n-1))  -- otherwise, count down.
~~~

~~~haskell
λ: :t onFor even 3
onFor even 3 :: Auto Int Bool
λ: testAuto_ (onFor even 3) [1,1,2,1,1,1,1,4,1,6,1,1,1,1]
[ False, False, True , True,True
, False, True , True , True,True
, True , False, False ]
~~~

Here is an auto that keeps a Map as its state, and takes as input a data type
`data Command k v = Insert k v | Lookup k | Delete k` allows you to either
insert, lookup, or delete an item.  However, the map has a "maximum size".  If
you insert past the maximum size, the insert will fail and you'll receive a
`Nothing`.  Otherwise, you'll receive a `Just val` with the value you just
inserted.  Successful lookups will return `Just val`, or `Nothing` on a
failure.  Same for deletions.

~~~haskell
import qualified Data.Map.Strict as Map

data Command k v = Insert k v | Lookup k | Delete k

autoMap :: forall k v. Ord k
    => Int              -- the maximum capacity of the map
    -> Auto (Command k v) (Maybe v)
autoMap cap = go Map.empty
  where
    go :: Map.Map k v -> Auto (Command k v) (Maybe v)
    go m = ACons $ \command ->
      case command of
        Insert key val ->
          if Map.size m >= cap && key `Map.notMember` m
            then
              ( Nothing, go m )                 -- Map is full, no go!
            else
              let m' = Map.insert key val m     -- go for it!
              in  ( Just val, go m' )
        Lookup key ->
          ( key `Map.lookup` m, go m )
        Delete key ->
          let result  = key `Map.lookup` m
              m'      = key `Map.delete` m
          in  ( result, go m' )
~~~

~~~haskell
λ: testAuto_ (autoMap 3)
  |  [ Insert "hello" 7
  |  , Insert "world" 10
  |  , Insert "foo" 12
  |  , Insert "bar" 15
  |  , Delete "baz"
  |  , Delete "world"
  |  , Insert "haskell" 19
  |  , Lookup "world"
  |  , Lookup "hello"
  |  ]
[ Just 7 , Just 10, Just 12
, Nothing, Nothing, Just 10
, Just 19, Nothing, Just 7  ]
~~~

The main thing to note here is that you get to completely encapsulate your
"state", and allow it only to be "modified" or "viewed" under your own terms.
In OOP terms, it is like exposing only a few public methods to modify your
private state with discrimination.  If you were passed an `autoMap` with items
already inside, you would have no way to have full "access" to the map --- you
would never be able to perform general operations (such as getting a list of
all of the keys).


## "Function Things"

Anyways, back to our main point of emphasis:

*Autos are function-like things*.

They are functions...with state.

Let's do an analysis for `isEvenAuto` like the one we did with `foldAuto`. Our
"input" was a `Maybe Int` and our "output" was a `Bool`.

You can think of `isEvenAuto` as a "function thing" from `Maybe Int` to
`Bool`.

Here's another function from `Maybe Int` to `Bool`: (I'm going to be using the
prefix form of `(->)` a lot now for the "final" return functions)

~~~haskell
maybeIsEven :: (->) (Maybe Int) Bool
maybeIsEven = even . fromMaybe 1
~~~

`maybeIsEven` returns `True` when value inside the `Just` is even, or `False`
if the value is odd or it's a `Nothing`.

Compare that type signature to that of our `isEvenAuto`

~~~haskell
maybeIsEven :: (->) (Maybe Int) Bool
isEvenAuto  :: Auto (Maybe Int) Bool
~~~

`maybeIsEven` and `isEvenAuto` are *both* "function-like things".  But
whereas `maybeIsEven` is "memoryless" (it's the same every time you call it),
`isEvenAuto` *has memory* --- it returns a different Boolean based on
its history.

Contrast this with a Stream, which as we have seen is just an `Auto () b`.
Streams are then "function like things" analgous to some `(->) () b`, or `()
-> b`.  We can call functions like `() -> b` "constants", or "producers". They
are the same every time you call them.  Streams, however, "return" a
potentially different `b` value every time they are "called".  So, just like
an Auto is a "function" that has memory, a Stream is like a *constant* that
has memory.  A stateful generator.  A "constant" that returns something
different every time you ask for it.

You should be able to guess that, after vaguely using the phrase "function
things" several times...I'm going to surprise you all with the revelation that
these "function things" have a name!  And maybe even...a typeclass?

Onward
------

So far we haven't really made too convincing of an argument for the advantages
of using machines (like Auto and the related Wire).  Yeah, they provide
encapsulation and a changing state...but these things come for free in most
good Object-Oriented Programming languages.  So what gives?

As it turns out, as we suggested before, Autos are much more "composable" than
the objects of OOP.  That is because, at their heart, they are just functions.
And what do functions do best?  They compose!  Complex object built seamlessly
from simpler ones.

Now, I haven't really been able to back this up so far.  But in the next post,
as we explore more the function-like nature of these things, we will be able
to witness the full power of machine composition.  And we'll even be able to
re-implement *many* of the complex machines of this post with compositions of
smaller, simpler Autos.



