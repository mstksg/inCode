Intro to Machines & Arrows (Part 1: Stream and Auto)

=====================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on February 3, 2014.
> [Read online!](https://blog.jle.im/entry/intro-to-machines-arrows-part-1-stream-and.html)

So I'm going to be running a series soon on computation and (physical)
simulations using AFRP (Arrowized Functional Reactive Programming) principles.

I consider (A)FRP to actually be a pretty game changing paradigm. It provides us
with semantics by which to *compose* and build time-varying, reactive behaviors
and completely changes the way we approach any sort of simulation/state-like
project.

AFRP has its own elegant way of approaching problems, but to be able to properly
use it for simulations, we're going to have to start by learning about the
fundamental abstraction behind its **implementation**: machines.[^1]

(This post will assume a somewhat basic knowledge of Haskell. I'll try
explaining concepts here and there if I feel that they might not be very
commonly known. But if you have any questions, feel free to leave a comment or
stop by freenode's #haskell on irc!)

(A short disclaimer: this article has not too much to do with the great
[machines](http://hackage.haskell.org/package/machines) library by Rúnar
Bjarnason and Edward Kmett)

## Why FRP?

This series doesn't directly deal with FRP, but implementing FRP is one of the
motivating factors for what we are discussing.

So why do we even bother with FRP? Why not just just program imperatively and
mutate a state? It's just as easy in a functional/pure language (with a trivial
State monad wrapper) as it is in an imperative language.

The answer is the full power of functional programming:
**[composition](http://www.haskellforall.com/2012/08/the-category-design-pattern.html)**.
Anyone who has dabbled in functional programming (or who has even used Unix
pipes) has had a glimpse into the power of composition. We start with simple,
fundamental, self-contained behaviors and compose them piece-by-piece into a
complex one.

So why FRP? FRP provides for us meaningful semantics by which to *compose*
time-varying and reactive behaviors, and create complex ones from smaller ones.
You reason about a self-contained behavior "wire"...and then use tools to build
complex behaviors from simple ones. You don't specify what happens when, you
specify how things *should behave*, from a top-down level. And then compose
those behaviors.

So to proceed, in this post we are going to introduce Streams and (more
importantly) Autos. In the next, we will be looking at Autos as a member of the
powerful Category and Arrow typeclasses. In the final post, we'll really be
throwing in a lot of useful enhancements to the `Auto` type in hopes that you
may be prepared for anything you encounter in the real world involving it,
including the popular AFRP library
[netwire](http://hackage.haskell.org/package/netwire).

## Streams

(All the code for Streams can be downloaded [from
github](https://github.com/mstksg/inCode/tree/master/code-samples/machines/Stream.hs)
and tried interactively online [at
FPComplete](https://www.fpcomplete.com/user/jle/machines))

Let's start with streams, one of the simpler of machines.

Streams are basically infinitely long linked lists.

``` haskell
data Stream b = SCons (b, Stream b)
```

(`SCons` for "Stream cons")

Compare that with the linked list data type, which is a Stream with an Ending
(`Nil`):

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Stream.hs#L7-L7
-- interactive: https://www.fpcomplete.com/user/jle/machines

data List a = Cons (a, List a) | Nil
```

or, as is more traditionally written:

``` haskell
data [a] = (:) a [a] | []
```

It's pretty easy to build lists:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Stream.hs#L36-L37
-- interactive: https://www.fpcomplete.com/user/jle/machines

myList :: List Int
myList = Cons ( 1, Cons ( 2, Cons (3, Nil) ) )
```

which is just, in the more traditional (infix) form:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Stream.hs#L40-L41
-- interactive: https://www.fpcomplete.com/user/jle/machines

myList' :: [Int]
myList' = 1:(2:(3:[]))
```

Let's see if `myList` does what we want: (a list from 1 to 3):

``` haskell
ghci> let (Cons (x,xs)) = myList
ghci> x
1
ghci> :t xs
xs :: List Int
ghci> let (Cons (y,ys)) = xs
ghci> y
2
ghci> let (Cons (z,zs)) = ys
ghci> z
3
ghci> zs
Nil
```

Yes! Perfect. We can "traverse" down our linked list by repeatedly pattern
matching out the "head" (the `x`, the first part of the tuple) and the "tail"
(the `xs`, the second part of the tuple).

But how are we going to build a stream...? We don't have access to `Nil`, so do
we have to manually type out an infinite stream?

Let's try defining the stream `[1..]` --- a stream that contains every natural
number starting from 1.

``` haskell
myStream' :: Stream Int
myStream' = SCons ( 1, SCons ( 2, SCons ( 3, ... ) ) )
```

Hm. This is going to take a while. I wonder if there's an easier way?

We can take advantage of Haskell's "lazy-by-default"-ness and leave the "rest"
of the stream as an unevaluated function call. And then we can recurse!

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Stream.hs#L46-L50
-- interactive: https://www.fpcomplete.com/user/jle/machines

myStream :: Stream Int
myStream = streamFrom 1
  where
    streamFrom :: Int -> Stream Int
    streamFrom n = SCons ( n, streamFrom (n+1) )
```

Cool! Let's see if this `myStream` really does what we want, the same way we
tested `myList`:

``` haskell
ghci> let (SCons (x, xs)) = myStream
ghci> x
1
ghci> :t xs
xs :: Stream Int
ghci> let (SCons (y, ys)) = xs
ghci> y
2
ghci> let (SCons (z, zs)) = ys
ghci> z
3
ghci> let (SCons (j,js)) = zs
ghci> j
4
ghci> let (SCons (k,ks)) = js
ghci> k
5
```

Yes, it works perfectly! Just like in the case of List, we can "traverse" down
the stream by pattern matching out the "head" of the stream (the first part of
the tuple) and the "tail" of the stream (the second part of the tuple).

Note that we can have some fun with Haskell syntax `Stream` by adding a record
label to the first (and only) field:

``` haskell
data Stream b = SCons { runStream :: (b, Stream b) }
```

so that we can do fancy things like:

``` haskell
ghci> :t runStream
runStream :: Stream b -> (b, Stream b)
ghci> let (x, xs) = runStream myStream
ghci> x
1
```

Basically, we get for free the function `runStream`, a function that yanks the
tuple out of the stream.

One minor final touch --- because `Stream` has only one constructor and one
field, we can make it a `newtype`, which has similar usage patterns/syntax as a
`data`, but which the compiler can more easily optimize:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Stream.hs#L10-L10
-- interactive: https://www.fpcomplete.com/user/jle/machines

newtype Stream b = SCons { runStream :: (b, Stream b) }
```

#### Automating Traversal

The repeated pattern matching we've been doing is kind of tedious, and it'll
only get more annoying over time, so let's make a function that can automate the
pattern matching for us really quickly so that we can test it more easily.

`streamToList` will take a Stream and perform the very straightforward
conversion into an infinite list.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Stream.hs#L15-L16
-- interactive: https://www.fpcomplete.com/user/jle/machines

streamToList :: Stream b -> [b]
streamToList (SCons (x, xs)) = x : streamToList xs
```

So now we can do:

``` haskell
ghci> take 10 $ streamToList myStream
[1,2,3,4,5,6,7,8,9,10]
```

Alternatively (and for reasons which will later become clear), we can also
define `testStream`, which takes a specified amount of elements and returns also
the "resulting" stream after all of those steps, and `testStream_`, which is the
same thing except that we throw away the modified stream.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Stream.hs#L20-L30
-- interactive: https://www.fpcomplete.com/user/jle/machines

testStream :: Stream b -> Int -> ([b], Stream b)
testStream strm 0 = ([]  , strm )
testStream strm n = (y:ys, final)
  where
    (y , next )   = runStream  strm
    (ys, final)   = testStream next (n-1)

testStream_ :: Stream b -> Int -> [b]
testStream_ = (fst .) . testStream
```

``` haskell
ghci> testStream_ myStream 10
[1,2,3,4,5,6,7,8,9,10]
```

### Streams are nice

Streams are nice! If you've been using Haskell for any stretch of time, you'll
know that we use infinite lists all the time and to great usefulness for the
ends of expressiveness and abstraction.

In mathematics, streams are known as a form of [Moore
machines](http://en.wikipedia.org/wiki/Moore_machine) (albeit with potentially
infinite sets of state and output values). They are machines that basically
internally progress from state to state to state to state. They just keep on
marching on...like a machine. In `myStream`, the initial state is 1. The next
state is 2; the next is 3, etc. What is important is that the next state *is a
function of the current state*.

This is made very apparent in our definition of `streamFrom`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Stream.hs#L49-L50
-- interactive: https://www.fpcomplete.com/user/jle/machines

    streamFrom :: Int -> Stream Int
    streamFrom n = SCons ( n, streamFrom (n+1) )
```

The "current state" whenever we call `streamFrom n` is `n`...the "next state"
(the "initial state" of the "tail") is `n+1`. We could have provided any
function `:: Int -> Int` there (say, `n+2`), that would be for us our "next
state" function.

So `myStream` is a Moore-like machine whose "next state" function is "the
current state plus one", and whose output is just the state itself.

#### State your purpose!

Did you catch that last sentence? It's a subtle point. In general, streams can
have outputs that are different than their states. As a trivial example, let's
have a stream whose state is an integer, yet whose output is a character:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Stream.hs#L53-L57
-- interactive: https://www.fpcomplete.com/user/jle/machines

charStream :: Stream Char
charStream = charStreamFrom 65
  where
    charStreamFrom :: Int -> Stream Char
    charStreamFrom n = SCons ( chr n, charStreamFrom (n+1) )
```

``` haskell
ghci> take 10 $ streamToList charStream
"ABCDEFGHIJ"
```

Wait, this is kind of weird. The type of our stream is `Stream Char`...`Char` is
the type of output/elements in the stream, the "head" when we pattern match. But
where is the `Int` that is the state of our stream in the type `Stream Char`...?

Can we even write a function `getState :: Stream b -> s` that works in general
for all streams?

Hm. If the state of our stream can have a type totally unrelated to the type of
the stream...that means that we probably can't even know what it type is. And
that even if we could "force" it out somehow, we would not even be able to work
with it in a type-safe way!

In fact...couldn't the state even *[vary
dynamically](https://github.com/mstksg/inCode/tree/master/code-samples/machines/Stream.hs#L60-L70)*
as the stream progresses?

### Continuing on

The problem with streams, as you might have guessed, is that you can't really
affect their progress once they start. Once you start `myStream`, it'll keep on
marching on, and on, and on...you have no way to "influence" its progression
*during* its march. The *behavior* of our stream *can't be influenced* by the
outside world in any way, once it has started. This is a bit limiting, because
we want behaviors that we can have interact with each other.

And so, we have the natural generalization of streams (and the machine we will
be spending the most time looking at): Auto.

## Auto

(All the code for Autos can be downloaded [from
github](https://github.com/mstksg/inCode/tree/master/code-samples/machines/Auto.hs)
and tried interactively online [at
FPComplete](https://www.fpcomplete.com/user/jle/machines))

Let's upgrade our streams, and introduce a way to affect how they progress.
Let's call it an Auto.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Auto.hs#L12-L12
-- interactive: https://www.fpcomplete.com/user/jle/machines

newtype Auto a b = ACons { runAuto :: a -> (b, Auto a b) }
```

Now, instead of an `SCons` containing just a tuple (a head-tails), an `ACons`
contains a *function* that *produces* your head-tails tuple. Before, all of our
`runStreams` produced the same tuple no matter what. Now, our `runAuto` can
produce a different tuple based on an outside input.

This is cool!

Let's look at the type signature of Auto before we go too much further.

In `Auto a b`, `b` is (just like for a Stream) the type of your "head" and the
type of the items in your "tail". It's the type of your "stream".

`a` is the type of the "influencing input".

So now, we basically have a `Stream b`, except at every "step", we can
"influence" the Stream with something of type `a`.

### A Trivial Auto

Let's look at a direct "port" of our `myStream`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Auto.hs#L40-L44
-- interactive: https://www.fpcomplete.com/user/jle/machines

myStreamAuto :: Auto a Int
myStreamAuto = streamAutoFrom 1
  where
    streamAutoFrom :: Int -> Auto a Int
    streamAutoFrom n = ACons $ \_ -> ( n, streamAutoFrom (n+1) )
```

This is kind of a dumb example, but `myStreamAuto` is just the exact same as
`myStream`. It's an Auto, but it *ignores its influencing input*.

Let's try it out.

``` haskell
ghci> :t runAuto
runAuto :: Auto a b -> (a -> (b, Auto a b))
ghci> let (x, xs) = runAuto myStreamAuto undefined
ghci> x
1
ghci> :t xs
xs :: Auto a Int
ghci> let (y, ys) = runAuto xs undefined
ghci> y
2
ghci> let (z, zs) = runAuto ys undefined
ghci> z
3
```

Remember that we are really doing `(runAuto myStreamAuto) undefined`, but
because of how Haskell associates function calls, the parentheses are
unnecessary. And hey, it kind of looks like `runAuto` is a two-parameter
function with an Auto as the first parameter and the "influence"/"input" as its
second. Which, due to the magic of currying-by-default, it basically is, in
Haskell!

### A Non-trivial Auto

Okay, that was fun I guess. But now let's take a first look at an auto which
*can* be influenced.

Let's have a stream where at every step, we can choose to "reset" the counter to
whatever integer we like.

We can do this by having the influence/input be a `Maybe Int`. If we want the
counter to progress normally, we pass in a `Nothing`. If we want the counter to
reset to a number `n` of our choosing, we pass in a `Just n`

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Auto.hs#L48-L54
-- interactive: https://www.fpcomplete.com/user/jle/machines

settableAuto :: Auto (Maybe Int) Int
settableAuto = counterFrom 1
  where
    counterFrom :: Int -> Auto (Maybe Int) Int
    counterFrom n = ACons $ \reset ->
      let c = fromMaybe n reset
      in  ( c, counterFrom (c + 1) )
```

Remember that `fromMaybe :: a -> Maybe a -> a` takes a "default" value, a Maybe
value, and then returns the value inside the Maybe if it's a `Just`, or the
default value if it's a `Nothing`.

So basically, when you `runAuto` with the Auto, if you give it a `Nothing`,
it'll give you `( n, counterFrom (n+1) )`. If you give it `Just m`, it'll give
you `( m, counterFrom (m+1) )`.

Cool --- let's try it out.

``` haskell
ghci> let (x, xs) = runAuto settableAuto Nothing
ghci> x
1
ghci> let (y, ys) = runAuto xs Nothing
ghci> y
2
ghci> let (z, zs) = runAuto ys (Just 10)
ghci> z
10
ghci> let (j, js) = runAuto zs Nothing
ghci> j
11
ghci> let (k, ks) = runAuto js Nothing
ghci> k
12
ghci> let (l, ls) = runAuto ks (Just (-1))
ghci> l
-1
ghci> let (m, ms) = runAuto ls Nothing
ghci> m
0
```

And there ya go.

#### Automatic traversals for our Autos

Again, the manual pattern matching is a little tedious so let's write us a
function to automate "progressing" down an Auto.

Like our `testStream`, `testAuto` takes an Auto. But because every "step" needs
an input, `testAuto auto` takes a *list* that specifies the input for every
step. `testAuto` returns the resulting collection of results, and also the
modified Auto. `testAuto_` throws away the new Auto and just gives us the
collection.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Auto.hs#L17-L25
-- interactive: https://www.fpcomplete.com/user/jle/machines

testAuto :: Auto a b -> [a] -> ([b], Auto a b)
testAuto auto []      = ([]  , auto )
testAuto auto (x:xs)  = (y:ys, final)
  where
    (y,  next ) = runAuto  auto x
    (ys, final) = testAuto next xs

testAuto_ :: Auto a b -> [a] -> [b]
testAuto_ a = fst . testAuto a
```

Trying it out on `settableAuto`:

    ghci> testAuto_ settableAuto [ Nothing, Nothing, Just 10
                                 , Nothing, Nothing, Just (-1)
                                 , Nothing ]
    [1,2,10,11,12,-1,0]

Alternatively, here is a fun way to test an Auto interactively, where you ask
for input at each iteration from the user ---

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Auto.hs#L27-L32
-- interactive: https://www.fpcomplete.com/user/jle/machines

interactAuto :: (Read a, Show b) => Auto a b -> IO ()
interactAuto a0 = do
    inp <- getLine
    let (x,a1) = runAuto a0 (read inp)
    print x
    interactAuto a1
```

``` haskell
ghci> interactAuto settableAuto
> Nothing
1
> Nothing
2
> Just 10
10
> Nothing
11
```

### A Shift

Let's shift our thinking a bit. Instead of seeing Autos as "streams you can
influence", we can think about them as "functions with state". That is, they are
functions that carry self-contained encapsulated state *inside* themselves. They
are still *functions* of some kind --- you put in an "input" of type `a` and get
in an "output" (the head) of type `b`. However, every "time" you do this, you
get a different output depending on what you have already passed in and what the
internal state is. As we will see, this internal state is completely opaque to
the world. The world only has access to the "output", the result.

(Remember that, because we're in a functional language, nothing is technically
actually really "mutable". When we say that we have a stateful function, we
really mean that every time we "call" the function, we get back an "updated"
function with the new state that behaves differently when "called").

To put it in terms of `settableAuto`:

-   The "input" of `settableAuto` is our `Maybe Int` by which we convey or
    decision to reset or allow to increment by one as normal.
-   The "output" of `settableAuto` is the "head" of the `ACons` that is returned
    --- the `x`, `y`, etc. It's the `Int`, the counter.
-   The "state" of `settableAuto` is, in essence, the `n` of `counterFrom n`.
    It's the internal value by which the behavior is determined. The behavior of
    `runAuto` depends on the `n` --- it either yields `n` itself and increments
    `n`, or ignores it.

#### The opaque state

It's a little tricky because the "output" and the "state" in our example
function seem to be exactly the same (just like for `myStream`), but let's whip
up a quick example where it's a little more obvious that the state and the
output are different things, and that the state is completely opaque and
encapsulated.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Auto.hs#L58-L64
-- interactive: https://www.fpcomplete.com/user/jle/machines

isEvenAuto :: Auto (Maybe Int) Bool
isEvenAuto = isEvenAutoFrom 1
  where
    isEvenAutoFrom :: Int -> Auto (Maybe Int) Bool
    isEvenAutoFrom n = ACons $ \reset ->
      let c = fromMaybe n reset
      in  ( even c, isEvenAutoFrom (c + 1) )
```

So `isEvenAuto` is the same as `settableCounterFrom`, except instead of
"yielding"/"outputting" `n`, it outputs `even n` --- `True` if `n` is even and
`False` if `n` is odd.

Here is a demonstration of its behavior ---

``` haskell
ghci> testAuto isEvenAuto  [ Nothing, Nothing, Just 10
                           , Nothing, Nothing, Just (-1)
                           , Nothing ]
[False,True,True,False,True]
```

Note that there is in general really no way to ever access the `n` internally
(in fact, like we said before, it is in theory possible because we can't even
know its type). It is completely sealed off from the world, except by our
explicit design. Here, we choose to only "offer" a way to "set" it using our
input.

Now it the three distinct concepts --- the input, output, and state --- should
be clear.

-   The "input" again is a `Maybe Int` where we can choose to reset the march of
    the state.
-   The "output" here is now a `Bool` that says whether or not the internal
    state is even.
-   The "state" here is still that `n` (an `Int`), and was the same as in the
    last example. But here it is more clear that the state is inaccessible in
    general. We can only modify it in ways that the Auto *itself* allows our
    "input" (in this case, a setter) to modify it. And we certainly can't
    arbitrarily "read" it.

### Autos are nice!

It should be clear now that Autos are a more or less straightforward extension
of Streams.

In fact, you might see that every `Stream b` is equivalent to `Auto () b`, where
the input is always unit.

Autos correspond loosely to the mathematical [Mealy
machine](http://en.wikipedia.org/wiki/Mealy_machine), but again with technically
potentially infinitely many possible states, input, and output values.

Now, we have a way to model behaviors that can somehow interact with the outside
world.

#### More on state

As we have seen, Auto's carry all of the hidden-internal-state features of
Streams. The type of an Auto (`Auto a b`) reveals the type of the "input" and
the "ouput"...but it never reveals nor fixes the type of the "state". The type
of the state is not only unknown, by possibly dynamically changing over the
course of the Auto's progression.

What Auto offers over Stream is then a way for the outside world to access and
modify the state *if the Auto wants it to*. Now, we can design Autos that, like
we have seen with `settableAuto`, we can offer limited ways to allow the world
to modify the state on our own terms.

### The Accumulator

Let's try our hand at another Auto, but instead of looking at things as an
influencable and eternally marching stream, we're going to try to look at things
as a function with state that affects its output.

How about an Auto that "accumulates" and "sums up" all of its incoming inputs,
starting at 0? More correctly, an Auto that, given any int, "returns" the sum of
that int with all of the previous ints it has received in its lifetime.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Auto.hs#L67-L73
-- interactive: https://www.fpcomplete.com/user/jle/machines

summer :: Num a => Auto a a
summer = sumFrom 0
  where
    sumFrom :: Num a => a -> Auto a a
    sumFrom n = ACons $ \input ->
      let s = n + input
      in  ( s , sumFrom s )
```

``` haskell
ghci> let (out1, auto1) = runAuto summer 10
ghci> out1
10
ghci> let (out2, auto2) = runAuto auto1 3
ghci> out2
13
ghci> testAuto_ auto2 [15,-17,6,0,-1]
[28,11,17,17,16]
```

-   The "input" is our incoming `Int` --- 10, 3, 15, -17, etc.
-   The "output" is the accumulated sum/integral -- 10, 13, 28, 11, etc.
-   The "state" in this case is the accumulator, which in this case stays in
    sync with the output. But remember that this is not the case in general.

Just for kicks, let's generalize this and make an Auto version of `foldl`
(technically, more like `scanl`): give us an operator and an initial value, and
we'll "fold up" all of our inputs.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Auto.hs#L78-L84
-- interactive: https://www.fpcomplete.com/user/jle/machines

autoFold :: forall a b. (b -> a -> b) -> b -> Auto a b
autoFold op init = foldFrom init
  where
    foldFrom :: b -> Auto a b
    foldFrom x = ACons $ \input ->
      let y = x `op` input
      in  ( y, foldFrom y )
```

(the `forall` is used with the [Scoped Type
Variables](http://www.haskell.org/haskellwiki/Scoped_type_variables) extension
to let us say that the `b` we mention in the type of `foldFrom` is the same as
the `b` in the type of `autoFold`. If we leave off the type signature of
`foldFrom`, this is not necessary)

Note that `summer` then is just `autoFold (+) 0`.

You can probably imagine lots of different folds you can turn into
`autoFold`s...and indeed a lot of practical Autos are just `autoFold`s. Here are
some cute ones:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Auto.hs#L92-L107
-- interactive: https://www.fpcomplete.com/user/jle/machines

accumulateIntoList :: Auto a [a]
accumulateIntoList = autoFold (flip (:)) []

productor :: Num a => Auto a a
productor = autoFold (*) 1

accumulateStrings :: Auto String String
accumulateStrings = autoFold (++) ""

monoidAccum :: Monoid a => Auto a a
monoidAccum = autoFold mappend mempty
```

Cool, huh?

#### Parallels with list folds

Let's look very carefully at a comparison between the type signature of
Prelude's `foldl` and the type signature of `autoFold`:

``` haskell
foldl      :: (b -> a -> b) -> b -> ([a] -> b)
autoFold   :: (b -> a -> b) -> b ->  Auto a b
```

Hm. Let's do some rearranging. Remember that in Haskell, `(->)` is just an infix
type operator. So we can always rewrite `a -> b` as `(->) a b`

``` haskell
foldl      :: (b -> a -> b) -> b -> ( (->) [a] b )
autoFold   :: (b -> a -> b) -> b -> ( Auto  a  b )
```

Let's get rid of some of the points, too:

``` haskell
foldl    op initial  :: (->) [a] b
autoFold op initial  :: Auto  a  b
```

So both `foldl` and `autoFold` have very similar behaviors:

Give `foldl` or `autoFold` an *accumulating function* and an *initial value*,
and they return *a new "function thing"*.

For `foldl op initial`, this "function thing" takes a *list* of `a` values and
condenses them into a `b` value.

For `autoFold op initial`, the "function thing" takes *one* `a` value and
returns a `b` value based on the previous `a`'s it has seen.

The main point here is that `autoFold` is a sort of "function" in a way...just
like the others before it. It's a...."function-like thing".

::: note
**Aside**

Here is a quick diversion, if you're up for it. This doesn't really have too
much to do with the rest of the post, but it'll help you test your intuition a
bit with Autos.

As an exercise, compare (and contrast) these three functions of identical type
signatures:

``` haskell
map       f          :: [a] -> [b]
scanl     op initial :: [a] -> [b]
testAuto_ auto       :: [a] -> [b]
```

(Assume that `scanl` does not include the initial accumulator...that is, we are
really talking about `drop 1 . scanl op init`)

Compare what they do conceptually. Then, for fun, try implementing some of them
in terms of the other. Which re-implementations are possible? Which ones aren't?
:::

### More Auto examples

[I've thrown up a few auto
examples](https://github.com/mstksg/inCode/tree/master/code-samples/machines/Auto.hs#L111-L176)
[that you can run online](https://www.fpcomplete.com/user/jle/machines) so you
can try it out and see how different ones work. Don't get too attached to them,
because we will later be re-implementing them as compositions of smaller,
simpler building blocks.

[rollingAverage](https://github.com/mstksg/inCode/tree/master/code-samples/machines/Auto.hs#L111-L122)
:   `rollingAverage n :: Fractional a => Auto a a` outputs a rolling average of
    the last `n` values it has encountered

``` haskell
ghci> testAuto_ (rollingAverage 4) [2,8,4,5,1,8,3,5,1,1,8,3,5,9,2]
[2.0 ,5.0 ,4.67,4.75,4.5
,4.5 ,4.25,4.25,4.25,2.5
,3.75,3.25,4.25,6.25,4.75]
```

[onFor](https://github.com/mstksg/inCode/tree/master/code-samples/machines/Auto.hs#L125-L146)
:   `onFor p i :: Auto a Bool` normally outputs `False`...except whenever the
    input matches the given predicate `p :: a -> Bool`. Then it stays "on"
    (`True`) for `i` steps.

``` haskell
ghci> :t onFor even 3
onFor even 3 :: Auto Int Bool
ghci> testAuto_ (onFor even 3) [1,1,2,1,1,1,1,4,1,6,1,1,1,1]
[ False, False, True , True , True
, False, False, True , True , True
, True , True , False, False ]
```

[autoMap](https://github.com/mstksg/inCode/tree/master/code-samples/machines/Auto.hs#L149-L176)

:   `autoMap cap :: Auto (Command k v) (Maybe v)` is a neat one. It internally
    holds a
    [Map](http://hackage.haskell.org/package/containers-0.5.4.0/docs/Data-Map.html)
    (a key-value store) --- you can give it `[Command][]` data types that tell
    it to insert, lookup, and delete values. However, it enforces a maximum of
    items.

    The main thing to note here is that you get to completely encapsulate your
    "state", and allow it only to be "modified" or "viewed" under your own
    terms. In OOP terms, it is like exposing only a few public methods to modify
    your private state with discrimination. If you were passed an `autoMap` with
    items already inside, you would have no way to have full "access" to the map
    --- you would never be able to perform general operations (such as getting a
    list of all of the keys).

``` haskell
ghci> testAuto_ (autoMap 3)
    |   [ Insert "hello" 7
    |   , Insert "world" 10
    |   , Insert "foo" 12
    |   , Insert "bar" 15
    |   , Delete "baz"
    |   , Delete "world"
    |   , Insert "haskell" 19
    |   , Lookup "world"
    |   , Lookup "hello"
    |   ]
[ Just 7 , Just 10, Just 12
, Nothing, Nothing, Just 10
, Just 19, Nothing, Just 7  ]
```

## "Function Things"

Anyways, back to our main point of emphasis:

*Autos are function-like things*.

They are functions...*with (self-contained) state*.

Let's do an analysis for `isEvenAuto` like the one we did with `foldAuto`. Our
"input" was a `Maybe Int` and our "output" was a `Bool`.

You can think of `isEvenAuto` as a "function thing" from `Maybe Int` to `Bool`.

Here's another function from `Maybe Int` to `Bool`: (I'm going to be using the
prefix form of `(->)` a lot from now on)

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/machines/Auto.hs#L181-L182

maybeIsEven :: (->) (Maybe Int) Bool
maybeIsEven = even . fromMaybe 1
```

`maybeIsEven` returns `True` when value inside the `Just` is even, or `False` if
the value is odd or it's a `Nothing`.

Compare that type signature to that of our `isEvenAuto`

``` haskell
maybeIsEven :: (->) (Maybe Int) Bool
isEvenAuto  :: Auto (Maybe Int) Bool
```

`maybeIsEven` and `isEvenAuto` are *both* "function-like things". But whereas
`maybeIsEven` is "memoryless" (it's the same every time you call it),
`isEvenAuto` *has memory* --- it returns a different Boolean based on its
history.

Contrast this with a Stream, which as we have seen is just an `Auto () b`.
Streams are then "function like things" analogous to some `(->) () b`, or
`() -> b`. We can call functions like `() -> b` "constants", or "producers".
They are the same every time you call them or ask for them. Streams, however,
"return" a potentially different `b` value every time they are "asked for". So,
just like an Auto is a "function" that has memory, a Stream is like a "constant"
that has memory. A stateful generator. A "constant" that returns something
different every time you ask for it.

Anyway, you should be able to guess that, after vaguely using the phrase
"function things" several times...I'm going to surprise you all with the
revelation that the general class of these "function things" have a name! And
maybe even...a typeclass?

## Onward

So far we haven't really made too convincing of an argument for the advantages
of using machines (like Auto and the related Wire). Yeah, they provide
encapsulation and a changing state...but these things come for free in most good
Object-Oriented Programming languages. So what gives?

As it turns out, as we suggested before, Autos are potentially more "composable"
than the objects of OOP. That is because, at their heart, they are just
functions. And what do functions do best (as every functional programmer knows)?
They compose! Complex object built seamlessly from simpler ones.

Now, I haven't really been able to back this up so far. We've only scratched the
surface, and we haven't even considered function-like things that can return
other function-like things. But in the next post, as we explore more the
function-like nature of these things, we will be able to witness more of the
full power of machine composition. And we'll even be able to re-implement *many*
of the complex machines of this post with compositions of smaller, simpler
Autos.

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

[^1]: It is somewhat important to note here that the semantics of FRP do not
    inherently involve machines. We'll learn more about this later. For now,
    remember that this series will chiefly study the low-level *implementation*
    of AFRP, which may or may not be related to the *semantics*/abstractions of
    FRP --- in an ideal world we wouldn't even have to worry about
    implementation and just work on the level of the abstractions.
    Unfortunately, we don't live in an ideal world :(

