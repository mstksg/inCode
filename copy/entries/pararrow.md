A (Semi-)Failed Project: An Arrow-based Data Parallelism Interface
==================================================================

Categories
:   Haskell
Tags
:   haskell
:   parallelism
:   arrows
CreateTime
:   2014/03/29 02:37:56
PostDate
:   Never
Identifier
:   pararrow

So I've been having several 'dead end' projects in Haskell recently that I've
sort of just scrapped and move from, but I decided that it might be nice to
document some of them :)  Here is my most recent semi-failure --- implicit
data parallelism through an Arrow interface.

The Vision
----------

So what do I mean?

### Data Parallelism

By "data parallelism", i refer to structuring parallel computations by "what
depends on what".  If two values *can* be computed in parallel, then that is
taken advantage of.  Consider something like:

~~~haskell
sum (map f xs)
~~~

Normally, this would:

1. One by one step over `xs` and apply `f` to each one, building a new list as
   you go along one at a time.
2. One by one step over your new mapped list, adding each element to an
   accumulator and outputting it at the end.

But note that there are some easy places to parallelize this --- because none
of the results the mapped list depend on eachother, you can apply `f` to every
element in parallel, and re-collect everything back at the end.  And this is a
big deal if `f` takes a long time.  This is an example of something commonly
refered to as "embarassingly parallel".

Note that because `sum` is associative, we could even sum something like
`[1,2,3,4]` in parallel --- we can do `sum [1,2]` and `sum [3,4]` in parallel,
and add them up later.

### Arrows

So what kind of Arrow interface am I imagining with this?

Haskell has some nice syntax for composing "functions":

~~~haskell
proc x -> do
    y <- f -< x
    z <- g -< x
    q <- h -< y
    returnA -< y * z + q
~~~

A `proc` statement is a fancy lambda, which takes an input `x` and "funnels"
`x` through several different "functions" --- in our case, `f`, `g`, and `h`
--- and lets you name the results so that you can use them later.

While this looks like you are 'performing' `f`, then `g`, then `h`, what is
actually happening is that you are *composing* and synthesizing a *new
function*.  You are "assembling" a new function that, when given an `x`,
collects the results of `x` run through `f`, `g`, and `h`, and pops out a
function of what comes out of those functions.

Except...`f`, `g`, and `h` don't have to be normal functions.  They are
"generalized" functions; functions that could perhaps even have side-effects,
or trigger special things, or be evaluated in special ways.  They are
instances of the `Arrow` typeclass.

Proc notation allows us to assemble a giant new arrow, from sequencing smaller
arrows.

An `Arrow a b` just represents, abstractly, a way to get some `b` from some
`a`, equipped with combinators that allow you to compose them in powerful
ways.  A normal function `(->)` is an arrow in the most obvious way, as `a ->
b` represents a way to get a `b` from an input `a`.

### Forking Arrows

Look at the proc statement and tell me that that doesn't scream "data
parallelism" to you.  Because every arrow `f`, `g`, and `h` can do
side-effecty, stateful, IO things...what if `f`, `g`, and `h` represented "a
way to get a `b` from an `a`...in its own separate thread"?

So if I were to "run" this special arrow, a `ParArrow a b`, I would do

~~~haskell
runArrow :: ParArrow a b -> a -> IO b
~~~

Where if i gave `runArrow` a `ParArrow a b`, and an `a`, It would fork itself
into its own thread and give you an `IO b` in response to your `a`.

Okay, because of Arrow's ability to "separate out" and "side-chain"
compositions (note that `q` in the previous example does not depend on `z` at
all, and can clearly be launched in parallel alongside the calculation of
`z`), it looks like from a `proc` notation statement, we can easily write
arrows that all 'fork themselves' under composition.

You should also be able to "join together" parallel computations.  That is, if
you have an `a -> b` and a `c -> d`, you could make a "parallel" `(a,c) ->
(b,d)`.  If I wanted to sequence after that a `(d,f) -> (e,g)`, I *should* be
able to "join" my `(a,c) -> (b,d)` and `(d,f) -> (e,g)` into one "giant"
`(a,c) -> (e,g)` --- and I shouldn't have to re-fork during the middle of the
computation.

Okay, well...if it sounds stupid to you now, *I* thought it was a nice
endeavor back then :)  Let's try implementing it, and let's see where things
go wrong.

