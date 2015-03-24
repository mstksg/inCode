Effectful, Recursive, Real-World Autos: Intro to Machine/Auto Part 3
====================================================================

Categories
:   Haskell
:   Ramblings
Tags
:   arrows
:   auto-machines
:   haskell
:   machines
CreateTime
:   2014/07/21 21:28:28
PostDate
:   2015/02/09 10:19:03
Series
:   Intro to Machines and Arrows
Identifier
:   machines-3

Hi! I have to apologize a bit for the long delay; starting grad school and
things like that have made me have to scramble to adjust to the new life.  But
a couple of people have asked me to finish up and wrap up this series, and I
think I owe it to them then :)  Welcome to the final chapter.

In the [last post][part2], we looked deeper into the Auto type, played
around with instancing it as familiar typeclasses, saw it as a member of the
powerful *Category* and *Arrow* typeclasses, and took advantage of this by
composing Autos both manually and using proc/do notation, and were freed from
the murk and mire of explicit recursion.  We observed the special nature of
this composition, and saw some neat properties, like local statefulness.

[part2]: http://blog.jle.im/entry/auto-as-category-applicative-arrow-intro-to-machines

At this point I consider most of the important concepts about working with
`Auto` covered, but now, we are going to push this abstraction further, to the
limits of real-world industrial usage.  We're going to be exploring mechanisms
for adding effects and, making the plain ol' `Auto` into something more rich
and featureful.  We'll see how to express denotative and declarative
compositions using recursively binded `Auto`s, and what that even means.
It'll be a trip down several avenues to motivate and see practical
Auto usage.[^nofrp]  Basically, it'll be a "final hurrah".

[^nofrp]: Some of you might recall an earlier plan for this post that would
include FRP.  Unfortunately, I've refactored FRP into a completely new topic,
because I've realized that the two aren't exactly as related as I had led you
all to believe.  Still, most if not all of these techniques here are used in
actual arrowized FRP libraries today.  So, look out for that one soon!

A fair bit of warning --- if the last post is not fresh in your mind, or you
still have some holes, I recommend going back and reading through them again.
This one is going to hit hard and fast :)  (Also, it's admittedly kind of long
for a single post, but I didn't want to break things up into two really short
parts.)

As always, feel free to leave a comment if you have any questions, drop by
freenode's *#haskell*, or find me on [twitter][] :)

[twitter]: https://twitter.com/mstk "Twitter"

All of the code in this post is [available for download][codesamps] and to
load up into ghci for playing along!

[codesamps]: http://blog.jle.im/source/code-samples/machines

Effectful Stepping
------------------

Recall our original definition of `Auto a b` as a newtype wrapper over a
function:

~~~haskell
a -> (b, Auto a b)
~~~

This can be read as saying, "feed the `Auto` an `a`, and (purely) get a
resulting `b`, and a 'next stepper'" --- the `b` is the result, and the `Auto
a b` contains the information on how to proceed from then on.

If you've been doing Haskell for any decent amount of time, you can probably
guess what's going to happen next!

Instead of "purely" creating a naked result and a "next step"...we're going to
return it in a context.

~~~haskell
a -> f (b, Auto a b)
~~~

What, you say?  What good does that do?

Well, what does returning things in a context *ever* let you do?

In Haskell, contexts like these are usually meant to be able to defer the
process of "getting the value" until the end, after you've built up your
contextual computation.  This process can be complicated, or simple, or
trivial.

For example, a function like:

~~~haskell
a -> b
~~~

means that it simply creates a `b` from an `a`.  But a function like:

~~~haskell
a -> State s b
~~~

Means that, given an `a`, you get a state machine that can *create a `b`*
using a stateful process, once given an initial state.  The `b` doesn't
"exist" yet; all you've given is instructions for creating that `b`...and the
`b` that is eventually created will in general depend on whatever initial `s`
you give the state machine.

A function like:

~~~haskell
a -> IO b
~~~

Means that, given an `a`, you're given *a computer program* that, when
executed by a computer, will generate a `b`.  The `b` doesn't "exist" yet;
depending on how the world is and how IO processes interact, how you are
feeling that day...the `b` generated will be different.  The process of IO
execution has the ability to *choose* the `b`.

So how about something like:

~~~haskell
a -> State s (b, Auto a b)
~~~

This means that, given `a`, "running" the `Auto` with an `a` will give you *a
state machine* that gives you, using a stateful process, both the *result* and
the *next step*.  The crazy thing is that now you are given the state machine
*the ability to decide the next `Auto`*, the next "step".


Something like:

~~~haskell
a -> IO (b, Auto a b)
~~~

means that your new `Auto`-running function will give you a result and a "next
step" that is going to be dependent on IO actions.

Let's jump straight to abstracting over this and explore a new type, shall we?

### Monadic Auto

~~~haskell
!!!machines/Auto3.hs "newtype AutoM"
~~~

We already explained earlier the new power of this type.  Let's see if we can
write our favorite instances with it.  First of all, what would a `Category`
instance even do?

Recall that the previous `Category` instance "ticked" each `Auto` one after
the other and gave the final results, and then the "next Auto" was the
compositions of the ticked autos.

In our new type, the "ticking" happens *in a context*.  And we need to tick
twice; and the second one is dependent on the result of the first.  This means
that your context has to be *monadic* in order to allow you to do this.

So we sequence two "ticks" inside the monadic context, and then return the
result afterwards, with the new composed autos.

The neat thing is that Haskell's built-in syntax for handling monadic
sequencing is nice, so you might be surprised when you write the `Category`
instance:

~~~haskell
!!!machines/Auto3.hs "instance Monad m => Category (AutoM m)"
~~~

Does it look familiar?

It should!  Remember the logic from the `Auto` Category instance?

~~~haskell
!!!machines/Auto2.hs "instance Category Auto"
~~~

It's basically *identical* and exactly the same :O  The only difference is
that instead of `let`, we have `do`...instead of `=` we have `<-`, and instead
of `in` we have `return`.  :O

The takeaway here is that when you have monadic functions, their sequencing
and application and composition can really be abstracted away to look pretty
much like application and composition of normal values.  And Haskell is one of
the few languages that gives you language features and a culture to be able to
fully realize the symmetry and similarities.

Check out the `Functor` and `Arrow` instances, too --- they're exactly the
same!


~~~haskell
!!!machines/Auto2.hs "instance Functor (Auto r)" "instance Arrow Auto"
~~~

~~~haskell
!!!machines/Auto3.hs "instance Monad m => Functor (AutoM m r)" "instance Monad m => Arrow (AutoM m)"
~~~


(I've left the rest of the instances from the previous part as an
exercise; the solutions are available in the downloadable.)

Neat, huh?  Instead of having to learn over again the logic of `Functor`,
`Applicative`, `Arrow`, `ArrowPlus`, etc., you can directly use the intuition
that you gained from the past part and apply it to here, if you abstract away
function application and composition to application and composition in a
context.

Our previous instances were then just a "specialized" version of `AutoM`, one
where we used naked application and composition,[^compapl]

[^compapl]: I'm going to go out on a limb here and say that, where Haskell
lets you abstract over functions and function composition with `Category`,
Haskell lets you abstract over values and function application with `Monad`,
`Applicative`, and `Functor`.

<div class="note">
**Aside**

If you look at the instances we wrote out, you might see that for some of
them, `Monad` is a bit overkill.  For example, for the `Functor` instance,

~~~haskell
instance Functor m => Functor (AutoM m r) where
    fmap f a = AConsM $ (f *** fmap f) . runAutoM a
~~~

is just fine.  We only need `Functor` to make `AutoM m r` a `Functor`.  Cool,
right?

If you try, how much can we "generalize" our other instances to?  Which ones
can be generalized to `Functor`, which ones `Applicative`...and which ones
can't?
</div>

By the way, it might be worth noting that our original `Auto` type is
identical to `AutoM Identity` --- all of the instances do the exact same
thing.

### Putting it to use

Now let's try using these!

First some utility functions just for playing around: `autoM`, which upgrades
an `Auto a b` to an `AutoM m a b` for any `Monad` `m`[^autom], and `arrM`,
which is like `arr`, but instead of turning an `a -> b` into an `Auto a b`, it
turns an `a -> m b` into an `AutoM m a b`:

[^autom]: This function really could be avoided if we had written all of our
`Auto`s is `AutoM`'s parameterized over all `m` in the first place --- that
is, written our `Auto a b`'s as the equally powerful `Monad m => AutoM m a b`.
But we're just going to run with `Auto` for the rest of this series to make
things a bit less confusing.

~~~haskell
!!!machines/Auto3.hs "autoM ::" "arrM ::"
~~~

We will need to of course re-write our trusty [`testAuto`][testauto] functions from the
first entry, which is again a direct translation of the original ones:

!!![testauto]:machines/Auto.hs "testAuto ::" "testAuto_ ::"

~~~haskell
!!!machines/Auto3.hs "testAutoM ::" "testAutoM_ ::"
~~~

First, let's test `arrM` ---

~~~haskell
ghci> :t arrM putStrLn
arrM putStrLn :: AutoM IO String ()
ghci> res <- testAutoM_ (arrM putStrLn) ["hello", "world"]
"hello"
"world"
ghci> res
[(), ()]
~~~

`arrM putStrLn` is, like `arr show`, just an `Auto` with no internal state. It
outputs `()` for every single input string, except, in the process of getting
the "next Auto" (and producing the `()`), it emits a side-effect --- in our
case, printing the string.

#### in IO

We can sort of abuse this to get an `Auto` with "two input streams": one from
the normal input, and the other from `IO`:

~~~haskell
!!!machines/Auto3.hs "replicateGets ::"
~~~

So, `replicateGets` uses [`monoidAccum`][macum] (or, an `AutoM` version) to
accumulate a string.  At every step, it adds `inpStr` to the running
accumulated string.  `inpStr` is the result of repeating the the string that
`getLine` returns replicated `n` times --- `n` being the official "input" to
the `AutoM` when we eventually run it.

!!![macum]:machines/Auto.hs "monoidAccum ::"

~~~haskell
ghci> testAutoM_ replicateGets [3,1,5]
> hello
> world
> bye
[ "hellohellohello"         -- added "hello" three times
, "hellohellohelloworld"    -- added "world" once
, "hellohellohelloworldbyebyebyebyebye"     -- added "bye" five times
]
~~~

Here, we used `IO` to get a "side channel input".  The main input is the
number of times we repeat the string, and the side input is what we get from
sequencing the `getLine` effect.

You can also use this to "tack on" effects into your pipeline.

~~~haskell
!!!machines/Auto3.hs "logging ::"
~~~

Here, `logging a` will "run" `a` with the input like normal (no side-channel
inputs), but then also log the results line-by-line to *log.txt*.

~~~haskell
ghci> testAutoM_ (logging summer) [6,2,3,4,1]
[6,8,11,15,16]
ghci> putStrLn =<< readFile "log.txt"
6
8
11
15
16
~~~

(By the way, as a side note, `logging :: Auto a b -> AutoM IO a b` here can be
looked at as an "`Auto` transformer".  It takes a normal `Auto` and transforms
it into an otherwise identical `Auto`, yet which logs its results as it ticks
on.)

#### Motivations

At this point, hopefully you are either excited about the possibilities that
monadic `Auto` composition/ticking offers, or are horribly revolted at how we
mixed IO and unconstrained effects and "implicit side channel" inputs.  Or
both!

After all, if all we were doing in `replicateGets` was having two inputs, we
could have just used:

~~~haskell
replicateGets' :: Auto (String, Int) String
~~~

And have the user "get" the string before they run the `Auto`.

And hey, if all we were doing in `logging` was having an extra logging
channel, we could have just manually logged all of the outputs as they popped
out.

All valid suggestions.  Separate the pure from the impure.  We went out of our
way to avoid *global* states and side-effects, so why bother to bring it all
back?

Superficially, it might seem like just moving the burden from one place to the
other.  Instead of having the user having to worry about getting the string,
or writing the log, the `Auto` can just handle it itself internally without
the "running" code having to worry.

The real, deep advantage in `AutoM`, however, is --- like in `Auto` --- its
(literal) *composability*.

Imagine `replicateGets'` was not our "final `Auto`" that we run...imagine it
was in fact an `Auto` used in a composition inside the definition of an `Auto`
used several times inside a composition inside the definition of another
`Auto`.  All of a sudden, having to "manually thread" the extra channel of
input in is a real nightmare.  In addition, you can't even statically
guarantee that the `String` `replicateGets` eventually was the same `String`
that the user originally passed in.  When composing/calling it, who knows if
the Auto that composes `replicateGets'` passes in the same initially gotten
String?[^readernotio]

[^readernotio]: By the way, you might notice this pattern as something that
seems more fit for `Reader` than `IO`.  We'll look at that later!

Imagine that the `Auto` whose results we wanted to log actually was not the
final output of the entire `Auto` we run (maybe we want to log a small
internal portion of a big `Auto`).  Again, now you have to manually thread the
*output*.  And if you're logging several things through several layers --- it
gets ugly very fast.

And now, all of your other `Auto`s in the composition get to (and *have* to)
see the values of the log!  So much for "locally stateful"!

As you can see, there is a trade-off in either decision we make.  But these
monadic compositions really just give us another tool in our toolset that we
can (judiciously) use.

#### Other contexts

It's fun to imagine what sort of implications the different popular monads in
Haskell can provide.  `Writer` gives you a running log that all `Auto`s can
append to, for example.  `Reader` gives you every composed `Auto` the ability
to access a shared global environment...and has an advantage over manual
"passing in" of parameters because every composed `Auto` is guaranteed to
"see" the same global environment per tick.

`State` gives every composed `Auto` the ability to access and modify a
globally shared state.  We talk a lot about every `Auto` having their own
local, internal state; usually, it is impossible for two composed `Auto`s to
directly access each other's state (except by communicating through output and
input).  With `State`, we now give the opportunity for every `Auto` to share
and modify a collective and global state, which they can use to determine how
to proceed, etc.

Good?  Bad?  Uncontrollable, unpredictable?  Perhaps.  You now bring in all of
the problems of shared state and reasoning with shared mutable state...and
avoiding these problems was one of the things that originally motivated the
usage of `Auto` in the first place!  But, we can make sound and judicious
decisions without resorting to "never do this" dogma.[^dogma]  Remember, these
are just tools we can possibly explore.  Whether or not they work in the real
world --- or whether or not they are self-defeating --- is a complex story!

[^dogma]: Which really isn't the point of these posts, anyway!

#### in State

Here is a toy state example to demonstrate different autos talking to
each other; here, the state is a measure of "fuel"; we can take any `Auto a b`
and give it a "cost" using the `limit` function defined here.  Here, every
`Auto` consumes fuel from the same pool, given at the initial `runState`
running.

~~~haskell
!!!machines/Auto3.hs "limit ::" "sumSqDiff ::" "stuff ::"
~~~

~~~haskell
-- a State machine returning the result and the next Auto
ghci> let stuffState  = runAutoM stuff 4
-- a State machine returning the result
ghci> let stuffState_ = fst <$> stuffState
ghci> :t stuffState_
stuffState_ :: State Int (Maybe Int, Maybe Int, Int)
-- start with 10 fuel
ghci> runState stuffState_ 10
((Just 8, Just 12, 12),   3)        -- end up with 3 fuel left
-- start with 2 fuel
ghci> runState stuffState_ 2
((Just 8, Nothing, 16),   0)        -- poop out halfway
~~~

You can see that an initial round with an even number should cost you seven
fuel...if you can get to the end.  In the case where we only started with two
fuel, we only were able to get to the "doubled" part before running out of
fuel.

Let's see what happens if we run it several times:

~~~hasell
ghci> let stuffStateMany = testAutoM_ stuff [3..6]
ghci> :t stuffStateMany
stuffStateMany :: State Int [(Maybe Int, Maybe Int, Int)]
ghci> runState stuffStateMany 9
( [ (Just 6 , Just 9 , 6 )
  , (Just 8 , Just 12, 25)
  , (Nothing, Just 15, 0 )
  , (Nothing, Nothing, 0 ) ]
, 0 )
~~~

So starting with nine fuel, we seem to run out halfway through the second
step.  The third field should be the sum of the squares so far, minus the sum
so far...at `25`, it's probably just the sum of the squares so far.  So it
couldn't even subtract out the sum so far.  Note that the `Just 15` on the
third step goes through because for *odd* inputs (5, in this case), the second
field doesn't require any fuel.

Anyways, imagine having to thread this global state through by hand.  Try it.
It'd be a disaster!  Everything would have to take an extra parameter and get
and extra parameter...it really is quite a headache.  Imagine the source for
`stuff` being written out in `Auto` with manual state threading.

But hey, if your program needs global state, then it's probably a good sign
that you might have had a design flaw somewhere along the way, right?

#### in Reader

Here we use `Reader` to basically give a "second argument" to an `Auto` when
we eventually run it, but we use the fact that every composed `Auto` gets the
*exact same* input to great effect:

~~~haskell
!!!machines/Auto3.hs "integral ::" "derivative ::" "fancyCalculus ::"
~~~

Now, we are treating our input stream as time-varying values, and the "Reader
environment" contains the "time passed since the last tick" --- The time step
or sampling rate, so to speak, of the input stream.  We have two stateful
`Auto`s ("locally stateful", internal state) that compute the time integral
and time derivative of the input stream of numbers...but in order to do so, it
needs the time step.  We get it using `ask`.  (Note that the time step doesn't
have to be the same between every different tick ... `integral` and
`derivative` should work just fine with a new timestep every tick.) (Also note
that `derivative` is `Nothing` on its first step, because there is not yet any
meaningful derivative on the first input)


In `fancyCalculus`, we calculate the integral, the derivative, the second
derivative, and the integral of the derivative, and return the second
derivative and the integral of the derivative.

In order for us to even *meaningfully say* "the second derivative" or "the
integral of the derivative", the double derivative has to be calculated with
the same time step, and the integral and the derivative have to be calculated
with the same time step.  If they are fed different time steps, then we aren't
really calculating a real second derivative or a real integral of a derivative
anymore.  We're just calculating arbitrary numbers.

Anyways, if you have taken any introduction to calculus course, you'll know
that the integral of a derivative is the original function --- so the integral
of the derivative, if we pick the right `x0`, should just be an "id"
function:

~~~haskell
integral x0 . derivative == id      -- or off by a constant difference
~~~

Let's try this out with some input streams where we know what the second
derivative should be, too.

We'll try it first with `x^2`, where we know the second derivative will just
be 2, the entire time:

~~~haskell
ghci> let x2s = map (^2) [0,0.05..1]
ghci> let x2Reader = testAutoM_ fancyCalculus x2s
ghci> :t x2Reader
x2Reader :: Reader Double [(Double, Double)]
ghci> map fst (runReader x2Reader 0.05)
[ ... 2.0, 2.0 ... ]    -- with a couple of "stabilizing" first terms
ghci> map snd (runReader x2Reader 0.05)
[ 0.0, 0.0025, 0.01, 0.0225, 0.04 ...]
ghci> x2s
[ 0.0, 0.0025, 0.01, 0.0225, 0.04 ...]
~~~

Perfect!  The second derivative we expected (all 2's) showed up, and the
integral of the derivative is pretty much exactly the original function.

For fun, try running it with a `sin` function.  The second derivative of `sin`
is `netage . sin`.  Does it end up as expected?

The alternative to using `AutoM` and `Reader` here would be to have each
composed Auto be manually "passed" the `dt` timestep.  But then we really
don't have any "guarantees", besides checking ourselves, that every `Auto`
down the road, down every composition, will have the same `dt`.  We can't say
that we really are calculating integrals or derivatives.  And plus, it's
pretty messy when literally every one of your composed `Auto` needs `dt`.

#### Mixing Worlds

We talked about a huge drawback of `State s` --- global mutable state is really
something that we originally looked to `Auto` to avoid in the first place.
But some portions of logic are much more convenient to write with autos that
all have access to a global state.

What if we wanted the best of both worlds?  What would that look like?

In Haskell, one common technique we like to use, eloquently stated by Gabriel
Gonzalez in his post [the Functor design pattern][tekmo], is to pick a "common
denominator" type, and push all of our other types into it.

[tekmo]: http://www.haskellforall.com/2012/09/the-functor-design-pattern.html

We have two fundamentally different options here.  We can pick our "main type"
to be `AutoM (State s)` and have global state, and "push" all of our
non-global-state Autos into it, or we can pick our "main type" to be `Auto`,
and "seal" our global-state-Autos into non-global-state ones.

For the former, we'd use `autoM`'s whenever we want to bring our `Auto`s into
`AutoM (State s)`...or we can always write `AutoM`'s parameterized over `m`:

~~~haskell
summer :: (Monad m, Num a) => AutoM m a a
~~~

It is statically guaranteed that `summer` *cannot touch any global state*.

For the latter option, we take `AutoM (State s)`'s that operate on global
state and then basically "seal off" their access to be just within their local
worlds, as we turn them into `Auto`'s.

~~~haskell
!!!machines/Auto3.hs "sealStateAuto ::" "runStateAuto ::"
~~~

`sealStateAuto` does exactly this.  Give it an initial state, and the `Auto`
will just continuously feed in its output state at every tick back in as the
input state.  Every `Auto` inside now has access to a local state, untouchable
from the outside.

`runStateAuto` is a way to do this were you can pass in a new initial state
every time you "step" the `Auto`, and observe how it changes --- also another
useful use case.

<div class="note">
**Aside**

We can even pull this trick to turn any `AutoM (StateT s m)` into an `AutoM
m`.  See if you can write it :)

~~~haskell
sealStateAutoM :: AutoM (StateT s m) a b -> s -> AutoM m a b
sealStateAutoM = ...

runStateAutoM :: AutoM (StateT s m) a b -> AutoM m (a, s) (b, s)
runStateAutoM = ...
~~~
</div>

In both of these methods, what is the real win?  The big deal is that you can
now chose to "work only in the world of non-global-state", combining
non-global `Auto`s like we did in part 1 and part 2 to create non-global
algorithms.  And then you can also chose to "work in the world of global
state", combining global `Auto`s like we did in the previous section, where
having `State s` made everything more clear.

We're allowed to live and compose (using `Category`, proc notation, etc.) in
whatever world we like --- create as complex compositions as we could even
imagine --- and at the end of it all, we take the final complex product and
"glue it on" to our big overall type so everything can work together.

This discussion is about `State`, but the ramifications work with almost any
`Auto` or type of `Auto` or underlying monad we talk about.

We can simulate an "immutable local environment", for example:


~~~haskell
!!!machines/Auto3.hs "runReaderAuto ::"
~~~

Now you can use a `Reader` --- composed with "global environment" semantics
--- inside a normal `Auto`!  Just give it the new environment very step!  (Can
you write a `sealReaderAuto` that just takes an initial `r` and feeds it back
in forever?)


Recursive Auto
--------------

Let's move back to our normal `Auto` for now, and imagine a very common use
case that might come up.

What if you wanted two chained `Auto`s to "talk to each other" --- for their
inputs to depend on the other's outputs?

Here's a common example --- in control theory, you often have to have adjust
an input to a system to get it to "respond" to a certain desired output (a
control).

One way is to start with a test input, at every step, observe the resulting
response and adjust it up or down until we get the response we want.  We call
the difference between the response and the control the "error".

How do you think you would calculate the adjustment?  Well...if the error is
big, we probably want a big adjustment.  And, the longer we are away from the
error, we also might want to make a bigger adjustment accordingly, too.

In other words, we might want our adjustment to have a term *proportional* to
the error, and a term that is *the sum of all* errors so far.

This system is known as [PI][pid], and is actually used in many industrial
control systems today, for controlling things like lasers and other super
important stuff.  Congrats, you are now a control theorist!

[pid]: http://en.wikipedia.org/wiki/PID_controller

Let's see how we might write this using our `Auto`s:

~~~haskell
piTargeter :: Auto Double Double
piTargeter = proc control -> do
    let err = control - response
    errSums  <- summer         -< err

    input    <- summer         -< 0.2 * err + 0.01 * errSums
    response <- blackBoxSystem -< input

    id -< response
  where
    blackBoxSystem = id     -- to simplify things :)
~~~

So this is an `Auto` that takes in a `Double` --- the control --- and outputs
a `Double` --- the response.  The goal is to get the response to "match"
control, by running a value, `input`, through a "black box system" (To
simplify here, we're only running `input` through `id`).

Here is the "logic", or the relationships between the values:

1.  The error value `err` is the difference between the control and the
    response.
2.  The sum of errors `errSums` is the cumulative sum of all of the error
    values so far.
3.  The input `input` is the cumulative sum of all of the correction terms: a
    multiple of `err` and a multiple of `errSums`.
4.  The response `response` is the result of running the input through the
    black box system (here, just `id`).
5.  The output is the response!

Look at what we wrote.  Isn't it just...beautifully declarative?  Elegant?
All we stated were *relationships between terms*...we didn't worry about
state, loops, variables, iterations...there is no concept of "how to update",
everything is just "how things are".  It basically popped up exactly as how we
"said" it.  I don't know about you, but this demonstration always leaves me
amazed, and was one of the things that sold me on this abstraction in the
first place.

But, do you see the problem?  To calculate `err`, we used `resp`.  But to
get `resp`, we need `err`!

We need to be able to define "recursive bindings".  Have Autos recursively
depend on each other.

In another language, this would be hopeless.  We'd have to have to resort to
keeping explicit state and using a loop.  However, with Haskell...and the
world of laziness, recursive bindings, and tying knots...I think that we're
going to have a *real win* if we can make something like what we wrote work.

### ArrowLoop

There is actually a construct in *proc* notation that lets you do just that.
I'm going to cut to the chase and show you how it looks, and how you use it.
I'll explain the drawbacks and caveats.  And then I'll explain how it works in
an aside --- it's slightly heavy, but some people like to understand.

Without further ado ---

~~~haskell
!!!machines/Auto3.hs "piTargeter ::"
~~~

The key here is the *rec* keyword.  Basically, we require that we write an
instance of `ArrowLoop` for our `Auto`...and now things can refer to each
other, and it all works out like magic!  Now our solution works...the feedback
loop is closed with the usage of `rec`.  Now, our algorithm looks *exactly*
like how we would "declare" the relationship of all the variables. We
"declare" that `err` is the difference between the control and the response.
We "declare" that `errSums` is the cumulative sum of the error values.  We
"declare" that our `input` is the cumulative sum of all of the adjustment
terms.  And we "declare" that our response is just the result of feeding our
input through our black box.

No loops.  No iteration.  No mutable variables.  Just...a declaration of
relationships.

~~~haskell
ghci> testAuto_ piTargeter [5,5.01..6]      -- vary our desired target slowly
[ 0, 1.05, 1.93, 2.67, 3.28 ...         -- "seeking"/tracking to 5
, 5.96, 5.97, 5.98, 5.99, 6.00          -- properly tracking
]
~~~

Perfect!

Wait wait wait hold on...but how does this even work?  Is this magic?  Can we
just throw *anything* into a recursive binding, and expect it to magically
figure out what we mean?

Kinda, yes, no.  This works based on Haskell's laziness.  It's the reason
something like `fix` works:

~~~haskell
fix :: (a -> a) -> a
fix f = f (fix f)
~~~

Infinite loop, right?

~~~haskell
ghci> head (fix (1:))
1
~~~

What?

`fix (1:)` is basically an infinite lists of ones.  But remember that `head`
only requires the first element to be evaluated:

~~~haskell
head (fix (1:))
head (1 : fix (1:))     -- head (x:_) = x
1
~~~

So that's the key.  If what we *want* doesn't require the entire result of the
infinite loop...then we can safely reason about infinite recursion in haskell.

The MVP here really is this function that I sneakily introduced,
`laggingSummer`:

~~~haskell
!!!machines/Auto3.hs "laggingSummer ::"
~~~

`laggingSummer` is like `summer`, except all of the sums are delayed.  Every
step, it adds the input to the accumulator...but returns the accumulator
*before* the addition.  Sort of like `x++` instead of `++x` in C. If the
accumulator is at 10, and it receives a 2, it *outputs 10*, and *updates the
accumulator to 12*.  The key is that it *doesn't need the input* to
*immediately return that step's output*.

~~~haskell
ghci> testAuto_ laggingSummer [5..10]
[0, 5, 11, 18, 26, 35]
~~~

The accumulator starts off at 0, and receives a 5...it then outputs 0 and
updates the accumulator to 5.  The accumulator then has 5 and receives a
6...it outputs 5 and then updates the accumulator to 11.  Etc.  The next step
it would output 45 *no matter what input it gets*.

Look at the definition of `piTargeter` again.  How would it get its "first
value"?

1.  The first output is just `response`.
2.  The first response is just the first `input`
3.  The first `input` is just the result of `laggingSummer`.
4.  The first result of `laggingSummer` is 0.

And that's it!  Loop closed!  The first result is zero...no infinite recursion
here.

Now that we know that the first result of `response` is 0, we can also find
the first values of `err` and `errSums`:  The first `err` is the first control
(input to the `Auto`) minus 0 (the first response), and the first `errSums` is
a cumulative sum of `errs`, so it too starts off as the first control minus
zero.

So now, we have all of the first values of *all* of our Autos.  Check!  Now
the next step is the same thing!

Recursive bindings have a lot of power in that they allow us to directly
translate natural language and (cyclic) graph-like "relationships" (here,
between the different values of a control system) and model them *as
relationships*.  Not as loops and updates and state modifications.  But *as
relationships*.  Something we can *declare*, at a high level.

And that's definitely something I would write home about.

The only caveat is, of course, that we have to make sure our loop can produce
a "first value" without worrying about its input.  Autos like `laggingSummer`
give this to us.

In the following aside, I detail the exact mechanics of how this works :)

<div class="note">
**Aside**

Ah, so you're curious?  Or maybe you are just one of those people who really
wants to know how things work?

The `rec` keyword in proc/do blocks desugars to applications of a function
called `loop`:

~~~haskell
class Arrow r => ArrowLoop r where
    loop :: r (a, c) (b, c) -> r a b
~~~

The type signature seems a bit funny.  Loop takes a morphism from `(a, c)` to
`(b, c)` and turns it into a morphism from `a` to `b`.  But...how does it do
that?

I'll point you to [a whole article about the `(->)` instance of
`ArrowLoop`][circprog] and how it is useful, if you're interested.
But we're looking at `Auto` for now.

[circprog]: https://wiki.haskell.org/Circular_programming

We can write an `ArrowLoop` instance for `Auto`:

~~~haskell
!!!machines/Auto2.hs "instance ArrowLoop Auto"
~~~

So what does this mean?  When will we be able to "get a `y`"?

We will be able to get a `y` in the case that the `Auto` can just "pop out"
your `y` without ever evaluating its arguments...or only using `x`.

The evaluation of `a'` is then deferred until later...and through this,
everything kinda makes sense.  The loop is closed.  See the article linked
above for more information on how `loop` really works.

The actual desugaring of a `rec` block is a little tricky, but we can
trust that if we have a properly defined `loop` (that typechecks and has the
circular dependencies that loop demands), then `ArrowLoop` will do what it is
supposed to do.

In any case, we can actually understand *how to work with rec blocks* pretty
well --- as long as we can have an `Auto` in the pipeline that can pop
something out immediately ignoring its input, then we can rest assured that
our knot will be closed.

By the way, this trick works with `ArrowM` too --- provided that the `Monad`
is an instance of `MonadFix`, which is basically a generalization of the
recursive `let` bindings we used above:

~~~haskell
!!!machines/Auto3.hs "instance MonadFix m => ArrowLoop (AutoM m)"
~~~
</div>


Going Kleisli
-------------

This is going to be our last "modification" to the `Auto` type --- one more
common `Auto` variation/trick that is used in real life usages of `Auto`.

### Inhibition

It might some times be convenient to imagine the *results* of the `Auto`s
coming in contexts --- for example, `Maybe`:

~~~haskell
Auto a (Maybe b)
~~~

How can we interpret/use this?  In many domains, this is used to model
"on/off" behavior of `Auto`s.  The `Auto` is "on" if the output is `Just`, and
"off" if the output is `Nothing`.

We can imagine "baking this in" to our Auto type:

~~~haskell
!!!machines/AutoOn.hs "newtype AutoOn"
~~~

Where the semantics of composition are: if you get a `Nothing` as an input,
just don't tick anything and pop out a `Nothing`; if you get a `Just x` as an
input run the auto on the `x`:

~~~haskell
!!!machines/AutoOn.hs "instance Category AutoOn"
~~~

The other instances are on the file linked above, but I won't post them here,
so you can write them as an exercise.  Have fun on the `ArrowLoop`
instance![^autoonnt]

[^autoonnt]: Another exercise you can do if you wanted is to write the exact
same instances, but for `newtype AutoOn a b = AutoOn (Auto a (Maybe b))` :)

<div class="note">
**Aside**

This aside contains category-theoretic justification for what we just did. You
can feel free to skip it if you aren't really too familiar with the bare
basics of Category Theory (What an endofunctor is, for example)... but, if you
are, this might be a fun perspective :)

What we've really done here is taken a category with objects as Haskell types
and morphisms are `Auto a b`, and turned it into a category with objects as
Haskell types and whose morphisms are `Auto a (m b)`, where `m` is a Monad.

The act of forming this second category from the first is called forming the
*Kleisli category* on a category.  We took `Auto` and are now looking at the
Kleisli category on `Auto` formed by `Maybe`.

By the way, a "Monad" here is actually different from the normal `Monad`
typeclass found in standard Haskell.  A Monad is an endofunctor on a category
with two associated natural transformations --- unit and join.

Because we're not dealing with the typical Haskell category anymore (on
`(->)`), we have to rethink what we actually "have".

For any Haskell Monad, we get for free our natural transformations:

~~~haskell
unitA :: Monad m => Auto a (m a)
unitA = arr return

joinA :: Monad m => Auto (m (m a)) (m a)
joinA = arr join
~~~

But what we *don't get*, necessarily, the *endofunctor*.  An endofunctor must
map both objects and morphisms.  A type constructor like `Maybe` can map
objects fine --- we have the same objects in `Auto` as we do in `(->)`
(haskell types).  But we also need the ability to map *morphisms*:

~~~haskell
class FunctorA f where
    fmapA :: Auto a b -> Auto (f a) (f b)
    -- fmapA id = id
    -- fmapA g . fmapA f = fmapA (g . f)
~~~

So, if this function exists for a type constructor, following the usual `fmap`
laws, then that type is an endofunctor in our `Auto` category.  And if it's
also a Monad in `(->)`, then it's also then a Monad in `Auto`.

We can write such an `fmapA` for `Maybe`:

~~~haskell
instance FunctorA Maybe where
    fmapA a = ACons $ \x ->
                case x of
                  Just _x -> let (y, a') = runAuto a x
                             in  (Just y, fmapA a')
                  Nothing -> (Nothing, fmapA a)
~~~

And, it is a fact that if we have a Monad, we can write the composition of its
Kleisli category for free:

~~~haskell
(<~=<) :: (FunctorA f, Monad f) => Auto a (f c) -> Auto a (f b) -> Auto a (f c)
g <~=< f = joinA . fmapA g . f
~~~

In fact, for `f ~ Maybe`, this definition is identical to the one for the
`Category` instance we wrote above for `AutoOn`.

And, if the `FunctorA` is a real functor and the `Monad` is a real monad,
then we have for free the associativity of this super-fish operator:

~~~haskell
(h <~=< g) <~=< f == h <~=< (g <~=< f)
f <~=< unitA      == unitA <~=< f      == f
~~~

Category theory is neat!

By the way, definitely not all endofunctors on `(->)` are endofunctors on
`Auto`.  We see that `Maybe` is one.  Can you think of any others?  Any others
where we could write an instance of `FunctorA` that follows the laws?  Think
about it, and post some in the comments!

One immediate example is `Either e`, which is used for great effect in many
FRP libraries!  It's "inhibit, with a *value*".  As an exercise, see if you
can write its `FunctorA` instance, or re-write the `AutoOn` in this section to
work with `Either e` (you might need to impose a typeclass constraint on the
`e`) instaed of `Maybe`!
</div>


I'm not going to spend too much time on this, other than saying that it is
useful to imagine how it might be useful to have an "off" Auto "shut down"
every next Auto in the chain.

One neat thing is that `AutoOn` admits a handy `Alternative` instance; `a1 <|>
a2` will create a new `AutoOn` that feeds in its input to *both* `a1` and
`a2`, and the result is the first `Just`.


~~~haskell
!!!machines/AutoOn.hs "instance Alternative (AutoOn a)"
~~~

Unexpectedly, we also get the handy `empty`, which is a "always off" `AutoOn`.
Feed anything through `empty` and it'll produce a `Nothing` no matter what.
You can use this to provide an "always fail", "short-circuit here" kind of
composition, like `Nothing` in the `Maybe` monad.

You also get this an interesting and useful concept called "switching" that
comes from this; the ability to switch from running one Auto or the other by
looking if the result is on or off --- here is a common switch that behaves
like the first `AutoOn` until it is off, and then behaves like the second
forever after:

~~~haskell
!!!machines/AutoOn.hs "(-->) ::"
~~~

### Usages

Let's test this out; first, some helper functions (the same ones we wrote for
`AutoM`)

~~~haskell
!!!machines/AutoOn.hs "autoOn ::" "arrOn ::" "fromAutoOn ::"
~~~

`autoOn` turns an `Auto a b` into an `AutoOn a b`, where the result is always
`Just`.  `arrOn` is like `arr` and `arrM`...it takes an `a -> Maybe b` and
turns it into an `AutoOn a b`.  `fromAutoOn` turns an `AutoOn a b` into a
normal `Auto a (Maybe b)`, just so that we can leverage our existing test
functions on normal `Auto`s.

Let's play around with some test `AutoOn`s!

~~~haskell
!!!machines/AutoOn.hs "onFor ::" "filterA ::" "untilA ::"
-- alternatively, using explicit recursion:
-- untilA p = AConsOn $ \x ->
--              if p x
--                then (Just x , untilA p)
--                else (Nothing, empty   )
~~~

One immediate usage is that we can use these to "short circuit" our proc
blocks, just like with monadic `Maybe` and do blocks:

~~~haskell
!!!machines/AutoOn.hs "shortCircuit1 ::" "shortCircuit2 ::"
~~~

If either the `filterA` or the `onFor` are off, then the whole thing is off.
How do you think the two differ?

~~~haskell
ghci> testAuto (fromAutoOn shortCircuit1) [1..12]
[ Nothing, Just 20, Nothing, Just 40, Nothing, Just 60
, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
ghci> testAuto (fromAutoOn shortCircuit2) [1..12]
[ Nothing, Just 20, Nothing, Nothing, Nothing, Nothing
, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
~~~

Ah.  For `shortCircuit1`, as soon as the `filterA` fails, it jumps *straight
to the end*, short-circuiting; it doesn't bother "ticking along" the `onFor`
and updating its state!

The arguably more interesting usage, and the one that is more used in real
life[^schead], is the powerful usage of the switching combinator `(-->)`
in order to be able to combine multiple `Auto`'s that simulate "stages"...an
`Auto` can "do what it wants", and then choose to "hand it off" when it is
ready.

[^schead]: Admittedly, implicit short-circuiting with `Auto`s is actually
often times a lot more of a headache than it's worth; note that "switching"
still works if you have a normal `Auto a (Maybe b)`; this is the approach that
many libraries like [*auto*][auto] take --- write switching combinators on
normal `Auto a (Maybe b)`'s instead.

~~~haskell
!!!machines/AutoOn.hs "stages ::"
~~~

~~~haskell
ghci> testAuto_ (fromAutoOn stages) [1..15]
[ Just (-1), Just (-2)              -- stage 1
, Just 3, Just 7, Just 12           -- stage 2
, Just 100, Just 200, Just 100      -- stage 3
, Just (-9), Just (-10)             -- stage 1
, Just 11                           -- stage 2
, Just 100, Just 200, Just 100      -- stage 3
, Just (-15), Just (-16)            -- stage 1
]
~~~

Note that the stages continually "loop around", as our recursive definition
seems to imply.  Neat!

<div class="note">
**Aside**

You might note that sometimes, to model on/off behavior, it might be nice to
really be able to "keep on counting" even when receiving a `Nothing` in a
composition.  For example, you might want both versions of `shortCircuit` to
be the same --- let `onFor` still "keep on counting" even when it has been
inhibited upstream.

If this is the behavior you want to model (and this is actually the behavior
modeled in some FRP libraries), then the type above isn't powerful enough;
you'll have to go deeper:


~~~haskell
!!!machines/AutoOn2.hs "newtype AutoOn2"
~~~

So now, you can write something like `onFor`, which keeps on "ticking on" even
if it receives a `Nothing` from upstream:

~~~haskell
!!!machines/AutoOn2.hs "onFor ::"
~~~

You can of course translate all of your `AutoOn`s into this new type:

~~~haskell
!!!machines/AutoOn2.hs "autoOn ::"
~~~

Or you can use the smart constructor method detailed immediately following.
</div>


Working all together
--------------------

Of course, we can always literally throw everything we can add together into
our `Auto` type:

~~~haskell
!!!machines/AutoX.hs "newtype AutoX"
~~~

(Again, instances are in the source file, but not here in the post directly)

Here is a same of a big conglomerate type where we throw in a bunch of things.

The benefit?  Well, we could work and compose "normal" `Auto`s, selecting for
features that we only need to work with.  And then, when we need to, we can
just "convert it up" to our "lowest common denominator" type.

This is the common theme, the "functor design pattern".  Pick your common
unifying type, and just pop everything into it.  You can compose, etc. with
the semantics of the other type when convenient, and then have all the parts
work together in the end.

This pattern is awesome, if only we didn't have so many types to convert in
between manually.

Well, we're in luck.  There's actually a great trick, that makes *all of this*
even more streamlined: we can replace the "normal constructors" like `ACons`,
`AConsM`, and `AConsOn`, with *smart constructors* `aCons`, `aConsM`,
`aConsOn`, that work *exactly the same way*:

~~~haskell
!!!machines/AutoX.hs "aCons ::" "aConsM ::" "aConsOn ::"
~~~

Compare these definitions of `summer`, `arrM`, and `untilA` from their
"specific type" "real constructor" versions to their `AutoX`-generic "smart
constructor" versions:

~~~haskell
!!!machines/Auto.hs "summer ::"
!!!machines/Auto3.hs "arrM ::"
!!!machines/AutoOn.hs "untilA' ::"
~~~

~~~haskell
!!!machines/AutoX.hs "summer ::" "arrM ::" "untilA ::"
~~~

They are literally exactly the same...we just change the constructor to the
smart constructor!

You might also note that we can express a "pure, non-Monadic" `Auto` in
`AutoM` and `AutoX` by making the type polymorphic over all monads:

~~~haskell
!!!machines/AutoX.hs "summer ::"1
~~~

An `Auto` with a type like this says, "I cannot perform any effects during
stepping" --- and we know that `summer` definitely does not.  `summer` is
statically guaranteed not to affect any state or IO, and it's reflected in its
type.

The takeaway?  You don't even have to mungle around multiple types to make
this strategy work --- just make all your `Auto`s from the start using these
smart constructors, and they all compose together!  One type from the start
--- we just expose different constructors to expose the different "subtypes of
power" we want to offer.

Now it's all just to chose your "greatest common denominator".  If you don't
want inhibition-based semantics, just only use `AutoM`, for example!

By the way, here's a "smart constructor" for `AutoM`.

~~~haskell
!!!machines/Auto3.hs "aCons ::"
~~~


Closing Remarks
---------------

That was a doozy, wasn't it?  For those of you who have been waiting, thank
you for being patient.  I hope most if not all of you are still with me.

Hopefully after going through all of these examples, you can take away some
things:

*   From the previous parts, you've recognized the power of local statefulness
    and the declarative style offered by proc notation.

*   From here, you've seen that the Auto type can be equipped in many ways to
    give it many features which have practical applications in the real world.

*   You've learned how to handle those features and use and manage them
    together in sane ways, and their limitations.

*   You also know that you can really "program", "compose", or "think" in any
    sort of Auto or composition semantics that you want, for any small part of
    the problem.  And then at the end, just push them all into your greatest
    common denominator type.  So, you aren't afraid to play with different
    effect types even in the same program!

*   You've seen the power of recursive bindings to make complete the promise
    of declarative programming --- being able to extend the realm of what we
    can express "declaratively", and what we can *denote*.

*   You are ready to really understand anything you encounter involving `Auto`
    and `Auto`-like entities.

So, what's next?

*   [Download the files of this post][codesamps], play along with the examples
    in this post, CTRL+F this page for "exercise" to find exercises, and try
    writing your own examples!

*   Feel ready to be able to have a grasp of the situation you see `Auto` in
    the real world, such as in the popular FRP library [netwire][]!

*   Apply it to the real world and your real world problems!

*   Well, a bit of self-promotion, my upcoming library [auto][] is basically
    supposed to be almost all of these concepts (except for implicit on/off
    behavior) implemented as a finely tuned and optimized performant library,
    attached with semantic tools for working with real-world problems with
    these concepts of local statefulness, composition, and declarative style.
    You can really apply what you learned here to start building projects
    right away!

    Well, sorta.  Unfortunately, as of Feburary 2015, it is not yet ready for
    real usage, and the API is still being finalized.  But now that this post
    is finished, I will be posting more examples and hype posts in the
    upcoming weeks and months leading up to its official release.

    I am open to pull requests and help on the final stages of documentation
    :)

    If you're interested, or are curious, stop by *#haskell-auto* on freenode
    or send me a message!

*   Look forward to an actual series on Arrowized FRP, coming up soon!  We'll
    be using the concepts in this series to *implement FRP*.

[netwire]: http://hackage.haskell.org/package/netwire
[auto]: https://github.com/mstksg/auto.

Happy Haskelling!
