A (Semi-)Failed Project: An Arrow-based Dataflow Parallelism Interface
======================================================================

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
dataflow parallelism through an Arrow interface.

The Vision
----------

So what do I mean?

### Dataflow Parallelism

By "dataflow parallelism", I refer to structuring parallel computations by "what
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

![The above proc statement, diagrammed.](/img/entries/pararrow/proc1.png "proc")

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
parallelism" to you.  Because every arrow `f`, `g`, and `h` can potentially do
side-effecty, stateful, IO things, depending on how we implemented the
arrow...what if `f`, `g`, and `h` represented "a way to get a `b` from an
`a`...in its own separate thread"?

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

Using this, in the above proc example with the fancy diagram, we should be
able to see that `z` is completely independent of `y` and `q`, so the `g`
arrow could really compute itself "in parallel", forked-off, from the `f` and
`h` arrows.

You should also be able to "join together" parallel computations.  That is, if
you have an `a -> c` and a `b -> d`, you could make a "parallel" `(a,b) ->
(c,d)`.  But what if I also had a `c -> e` and a `d -> f`?  I could chain the
entire `a`-`c`-`e` chain and the `b`-`d`-`f` chain, and perform both chains in
parallel and re-collect things at the end.  That is, a `(a,b) -> (c,d)` and a
`(c,d) -> (e,f)` should meaningfully compose into a `(a,b) -> (e,f)`, where
the left and right sides (the `a -> e` and the `b -> f`) are performed "in
parallel" from eachother.

With that in mind, we could even do something like `parMap`:

~~~haskell
parMap :: ParArrow a b -> ParArrow [a] [b]
parMap f = proc input -> do
    case input of
      []     ->
          returnA        -< []
      (x:xs) -> do
          y  <- f        -< x
          ys <- parMap f -< xs
          returnA        -< y:ys
~~~

And because "what depends on what" is so *obviously clear* from proc/do
notation --- you know exactly what depends on what, and the graph is already
laid out there for you --- and because `f` is actaully a "smart" function,
with "smart" semantics which can do things like fork threads to solve
itself...this should be great way to structure programs and take advantage of
implicit data parallelism.

Okay, well...if the downfall to this idea is obvious to you know...in my
defense, *I* thought it was a worthwhile endeavor back then :)

In any case, let's try implementing it, and let's see where things go wrong.

ParArrow
--------

### Data and Instances

Let's start out with our arrow data type:

~~~haskell
data ParArrow a b =                     Pure  (a -> b)
                  | forall z.           Seq   (ParArrow a z)
                                              (ParArrow z b)
                  | forall a1 a2 b1 b2. Par   (a -> (a1, a2))
                                              (ParArrow a1 b1)
                                              (ParArrow a2 b2)
                                              ((b1, b2) -> b)
~~~

So a `ParArrow a b` represents a (pure) paralleizable, forkable computation
that returns a `b` (as `IO b`) when given an `a`.[^unsafepio]

[^unsafepio]: Technically, all `ParArrow` computations are pure, so you might
not loose too much by just returning a `b` instead of an `IO b` with
`unsafePerformIO`, but...

*   `Pure f` wraps a pure function in a `ParArrow` that computes that function
    in a fork when necessary.

*   `Seq f g` sequences a `ParArrow a z` and a `ParArrow z b` into a big
    `ParArrow a b`.  It reprensents composing two forkable functions into one
    big forkable function, sequentially.

*   `Par l f g r` takes two `ParArrow`s `f` and `g` of different types and
    represents the idea of performing them in parallel.  Of forking them off
    from eachother and computing them independently, and collecting it all
    together.

    `l` and `r` are supposed to be functions that turn the tupled
    inputs/outputs of the parallel computations and makes them fit `ParArrow a
    b`.  `r` is kind of supposed to be `id`, and `l` is supposed to be `id`
    (to continue a parallel action) or `\x -> (x,x)` (to begin a fork).

    It's a little hacky, and there might be a better way with GADT's and all
    sorts of type/kind-level magic, but it was the way I found that I
    understood the most.

    The main purpose of `l` and `r` is to be able to meaningfully refer to the
    two parallel `ParArrow`s in terms of the `a` and `b` of the "combined"
    `ParArrow`.  Otherwise, the two inputs of the two parallel `ParArrow`s
    don't have anything to do with the input type `a` of the combined
    `ParArrow`, and same for output.

Okay, let's define a Category instance, that lets us compose `ParArrow`s:

~~~haskell
instance Category ParArrow where
    id    = Pure id
    f . g = Seq g f
~~~

No surprises there, hopefully!  Now an Arrow instance:

~~~haskell
instance Arrow ParArrow where
    arr      = Pure
    first f  = f  *** id
    second g = id *** g
    f &&& g  = Par (id &&& id) f g id
    f *** g  = Par id          f g id
~~~

Also simple enough.  Note that `first` and `second` are defined in terms of
`(***)`, instead of the typical way of defining `second`, `(&&&)`, and `(***)`
in terms of `arr` and `first`.

### The Magic

Now, for the magic --- consolidating a big composition of fragmented
`ParArrow`s into a streamlined simple-as-possible graph:

~~~haskell
collapse :: ParArrow a b -> ParArrow a b
collapse (Seq f g)       =
    case (collapse f, collapse g) of
      (Pure p1, Pure p2)      -> Pure (p1 >>> p2)
      (Seq s1 s2, _)          -> Seq (collapse s1)
                                     (collapse (Seq s2 g))
      (_, Seq s1 s2)          -> Seq (collapse (Seq f s1))
                                     (collapse s2)
      (Pure p, Par l p1 p2 r) -> Par (p >>> l)
                                     (collapse p1) (collapse p2)
                                     r
      (Par l p1 p2 r, Pure p) -> Par l
                                     (collapse p1) (collapse p2)
                                     (r >>> p)
      (Par l p1 p2 r,
       Par l' p1' p2' r')     -> let p1f x = fst . l' . r $ (x, undefined)
                                     p2f x = snd . l' . r $ (undefined, x)
                                     pp1 = collapse (p1 >>> arr p1f >>> p1')
                                     pp2 = collapse (p2 >>> arr p2f >>> p2')
                                 in  Par l pp1 pp2 r'
      (f', g') -> Seq f' g'
collapse p = p
~~~



