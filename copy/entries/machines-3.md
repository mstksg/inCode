Effectful, Recursive, and Real-World Autos: Intro to Machine/Auto Part 3
========================================================================

Categories
:   Haskell
:   Ramblings
Tags
:   haskell
:   functional reactive programming
:   arrows
:   netwire
CreateTime
:   2014/07/21 21:28:28
PostDate
:   Never
Series
:   Intro to Machines and Arrows
Identifier
:   machines-3

Hi!  I have to apologize a bit for the long delay; [starting grad
school][chapman] and things like that have made me have to scramble to adjust
to the new life.  But a couple of people have asked me to finish up and wrap
up this series, and I think I owe it to them :)

[chapman]:http://blog.jle.im/entry/looking-forward-a-doctorate-program

In the [last post][part2], we looked deeper into the Auto type, played
around with instancing it as familiar typeclasses, saw it as a member of the
powerful *Category* and *Arrow* typeclasses, and took advantage of this by
composing Autos both manually and using proc/do notation, and were freed from
the murk and mire of explicit recursion.  We observed the special nature of
this composition, and saw some neat properties, like local statefulness.

[part2]: http://blog.jle.im/entry/auto-as-category-applicative-arrow-intro-to-machines

Here we are going to push the abstraction further to see where it will go by
introducing mechanisms for adding effects, making the plain ol' Auto type into
something rich and featureful. And finally, at the very end, we'll do a short
case study on one motivating example: arrowized FRP libraries!

As always, feel free to leave a comment if you have any questions, drop by
freenode's *#haskell*, or find me on [twitter][] :)

[twitter]: https://twitter.com/mstk "Twitter"

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

Well, if you've been doing Haskell for any decent amount of time, you can
probably guess what's going to happen next.  It's, incidentally, one of the
things that Haskellers love doing the most, and it's been engrained deeply
into the Haskell culture.

Instead of "purely" creating a naked result and a "next step"...we're going to
return it in a context.

~~~haskell
a -> f (b, Auto a b)
~~~

Whaat, you say?  What good does that do?

Well, what does returning things in a context ever lets you do?

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
a -> Maybe b
~~~

Means...it *might* give you a `b` from an `a`?  Or it might not?  You won't
really know until you inspect the result later.  If you eventually use
`fromMaybe`, then the resulting `Maybe b` can *control*, using its Nothingness
or its Justness, the final `b` that you get.

A function like:

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
!!!machines/Auto3.hs "newtype AutoM" machines
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
!!!machines/Auto3.hs "instance Monad m => Category (AutoM m)" machines
~~~

Does it look familiar?

It should!  Remember the logic from the `Auto` Category instance?

~~~haskell
!!!machines/Auto2.hs "instance Category Auto" machines
~~~

It's basically *identical* and exactly the same :O  The only difference is
that instead of `let`, we have `do`...instead of `=` we have `<-`, and instead
of `in` we have `return`.  :O

The takeaway here is that when you have monadic functions, their sequencing
and application and composition can really be abstracted away to look pretty
much like application and composition of normal values.  And Haskell is one of
the few languages that gives you language features and a culture to be able to
fully realize the symmetry and similarities.

Following this exact same pattern, let's see the other various instances we
wrote in the last article, along with the new monadic instances:


~~~haskell
!!!machines/Auto2.hs "instance Functor (Auto r)" "instance Applicative (Auto r)" "instance Arrow Auto" "instance ArrowChoice Auto" machines
~~~

~~~haskell
!!!machines/Auto3.hs "instance Monad m => Functor (AutoM m r)" "instance Monad m => Applicative (AutoM m r)" "instance Monad m => Arrow (AutoM m)" "instance Monad m => ArrowChoice (AutoM m)" machines
~~~


Neat, huh?

Instead of having to learn over again the logic of `Functor`, `Applicative`,
`Arrow`, `ArrowPlus`, etc., you can directly use the intuition that you gained
from the past part and apply it to here, if you abstract away function
application and composition to application and composition in a context.

Our previous instances then were just a "specialized" version of `AutoM`, one
where we used naked application and composition :)[^compapl]

[^compapl]: I'm going to go out on a limb here and say that, where Haskell
lets you abstract over functions and function composition with `Category`,
Haskell lets you abstract over values and function application with `Monad`,
`Applicative`, and `Functor`.

<div class="note">
**Aside**

If we look closely at the instances we wrote, we might notice that for some of
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

First some utility functions: `autoM`, which upgrades an `Auto a b` to an
`AutoM m a b` for any `Monad` `m`[^autom], and `arrM`, which is like `arr`, but
instead of turning an `a -> b` into an `Auto a b`, it turns an `a -> m b` into
an `AutoM m a b`:

[^autom]: This function really could be avoided if we had written all of our
`Auto`s is `AutoM`'s parameterized over all `m` in the first place --- that
is, written our `Auto a b`'s as the equally powerful `Monad m => AutoM m a b`.
But we're just going to run with `Auto` for the rest of this series to make
things a bit less confusing.

~~~haskell
!!!machines/Auto3.hs "autoM ::" "arrM ::" machines
~~~

We will need to of course re-write our trusty [`testAuto`][testauto] functions from the
first entry, which is again a direct translation of the original ones:

!!![testauto]:machines/Auto.hs "testAuto ::" "testAuto_ ::" machines

~~~haskell
!!!machines/Auto3.hs "testAutoM ::" "testAutoM_ ::" machines
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

`arrM putStrLn`, like `arr show`, is just an `Auto` with no internal state
that returns `()` for every single input string, except, in the process of
getting the "next Auto", it emits a side-effect --- in our case, printing the
string.

#### in IO

We can sort of abuse this to get an `Auto` with "two input streams": one from
the normal input, and the other from `IO`:

~~~haskell
!!!machines/Auto3.hs "replicateGets ::" machines
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
!!!machines/Auto3.hs "logging ::" machines
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

By the way, as a side note, `logging :: Auto a b -> AutoM IO a b` here can be
looked at as an "`Auto` transformer".  It takes a normal `Auto` and transforms
it into an otherwise identical `Auto`, yet which logs its results as it ticks
on.

#### Motivations

At this point, hopefully you are either excited about the possibilities that
monadic `Auto` composition/ticking offers, or are horribly revolted at how we
mixed IO and unconstrained effects and "implicit side channel" inputs.  Or
both!

You might have noted that we could have actually "factored out" the direct IO
from our functions.  For example, we could have made `replicateGets` into
something that actually takes an explicit string:

~~~haskell
replicateGets  :: AutoM IO Int String

-- the only point of IO is to get a String, so why not:
replicateGets' :: Auto (String, Int) String
~~~

And then defer the "getting of the `String`" to the actual process of
"testing" the `Auto`:

~~~haskell
testReplicateGets' :: Auto (String, Int) String -> IO String
~~~

That is, have the "testing function" do the IO for you, and take it out of the
`Auto`.

And for `logging`, maybe we could have just had the "testing function"
manually do the logging:

~~~haskell
testAutoLogging_ :: Show b => Auto a b -> [a] -> IO [b]
~~~

That is, while `testAutoLogging_` is stepping the `Auto`s, it is also logging
along the way, factoring out the IO.

These are all valid alternatives, depending on your domain.  However, there
are some advantages.

One is that you simplify what your *user* might do.  Instead of building a
"pure"[^pure] Auto (non-IO) and making your user get the IO themselves, you
can have it implicitly handle it for you.  And instead of forcing your user to
log the results at the end, you can just handle it in-house.

[^pure]: "pure": This word is so loaded in so many ways in this
context...there are so many meanings it could take here, especially when we
have multiple levels of states and effects like we do now.  So I'm trusting
you to know what I'm talking about :)  In this case, it is an `Auto` instead
of an `AutoM`.

But probably the *biggest* advantage of implicit logging or inputs is that it
just "composes" nicer.  Suppose that `replicateGets` *isn't* the *final*
`Auto` that you want to run, or give your user.  Suppose that `replicateGets`
is just a small cog in a huge nested composition of `Auto`s.  Instead of
having to thread a `String` input throughout your *entire* composition chain
*just* for `replicateGets` to use it...you can just pull it out when you need
it.

In addition, one thing danger from manually treading a `String` input
throughout your entire composition is that somewhere along the way, any `Auto`
can chose to "change" the input before it's passed, meaning that you can't
guarantee that the `String` that `replicateGets` eventually gets is the same
one that was originally "gotten" before the whole thing was run anyway.

(This is indeed a problem solved by having `AutoM`, but in real life, perhaps
`Reader String` would be a better way to solve this specific situation than
`IO`)

For `logging`, if you only want to log a small portion of your entire `Auto`
--- that is, only log one small composed `Auto` out of an entire big
composition --- if you take the "thread the output" approach, you're going to
have to manually thread the output throughout the entire composition to the
end.

And, back to our "locally stateful" emphasis from before, every other `Auto`
in your composition has accessed to the logged items...even if you wanted to
keep it hidden.  And what if you wanted to log five, six `Auto`s to five or
six different files?  You're going to be passing five or six strings from
different places!  In this case, you gain a lot from implicit logging.

The point here is that there is a trade-off in either case.  But these monadic
compositions really just give us another tool in our toolset that we can
(judiciously) use.

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
!!!machines/AutoState.hs "limit ::" "sumSqDiff ::" "stuff ::" machines
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
!!!machines/AutoReader.hs "delay ::" "integral ::" "derivative ::" "fancyCalculus ::" machines
~~~

(Note the delay helper auto, `delay x0`, which outputs the "last received"
value...starting with `x0` as the first output.  This really could have been
written using `foldAuto` and a tuple, but the explicit recursion version is
arguably nicer)

Now, we are treating our input stream as time-varying values, and the "Reader
environment" contains the "time passed since the last tick" --- The time step
or sampling rate, so to speak, of the input stream.  We have two stateful
`Auto`s ("locally stateful", internal state) that compute the time integral
and time derivative of the input stream of numbers...but in order to do so, it
needs the time step.  We get it using `ask` and `arrM`.  Note that the time
step doesn't have to be the same for every different tick ... `integral` and
`derivative` should work just fine with a new timestep every tick.

In `fancyCalculus`, we calculate the integral, the derivative, the second
derivative, and the integral of the derivative, and return the second
derivative and the integral of the derivative.

In order for us to even *meaningfully say* "the second derivative" or "the
integral of the derivative", the double derivative has to be calculated with
the same time step, and the integral and the derivative have to be calculated
with the same time step.  If they are fed different time steps, then we aren't
really calculating a real second derivative or a real integral of a derivative
anymore.  We're just calculating random numbers.

Anyways, if you have taken any introduction to calculus course, you'll know
that the integral of a derivative is the original function --- so the integral
of the derivative, if we pick the right initial values, should just be an "id"
function:

~~~haskell
integral x0 . derivative d0 == id
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

For fun, try running it with a `sin` function.  The second derivative should
be original `sin` stream flipped, this time.  Does it end up as expected?

The alternative to using `AutoM` and `Reader` here would be to have each
composed Auto be manually "passed" the `dt` timestep.  But then we really
don't have any "guarantees", besides checking ourselves, that every `Auto`
down the road, down every composition, will have the same `dt`.


#### Unrolling AutoM's

We talked about a huge drawback of `State s` --- global mutable state is really
something that we originally looked to `Auto` to avoid in the first place.
But some portions of logic are much more convenient to write with autos that
all have access to a global state.

What if we could have the best of both worlds?  What if we knew that an
isolated part of our program.logic needs access to some global state, and
would be better written as an `AutoM (State s)` ... but we would rather not
have the rest of the program have access to it, or even know about it?  Sort
of like a "local scope", in other languages?

We can do this!  We can write an Auto using global state --- most importantly,
*taking advantage of Category, Arrow instances* to make things work like you
want them to --- and then, at the end, "use them in a normal Auto":

~~~haskell
!!!machines/AutoState.hs "runStateAuto ::" machines
~~~

This lets you turn any `AutoM (State s) a b` that you built up into an `Auto
(a, s) (b, s)`...and throw it into any normal `Auto`.  At every step, give it
the input state and get out the output state as another channel if
input/output.  You'll have to throw in the state and the output manually,
though...but you only do it *once*, instead of through every single
composition.

If we ever don't want the outside to ever have access to the state at all, we
can recover our "local statefulness" principles that we loved so much by
throwing in an initial state and letting it just tick itself away:

~~~haskell
!!!machines/AutoState.hs "sealStateAuto ::" machines
~~~

This "seals away" the state in a `AutoM (State s)` in an `Auto`; give it an
initial `State` and it keeps on ticking away with that initial state, feeding
it back into itself.

The trick here is we "reduce" or "unroll" an `AutoM` into a normal `Auto`, so
that we can compose it in with normal `Auto`s.  This way, we are allowed to
"think" in whatever manner we want (global state, non-global state...),
compose them in the way that suits us the best, and in the end, use them all
together in the "greatest common denominator", `Auto`.

<div class="note">
**Aside**

We can even pull this trick to turn any `AutoM (StateT s m)` into an `AutoM
m`.  See if you can write it :)

~~~haskell
runStateAutoM :: AutoM (StateT s m) a b -> AutoM m a b
runStateAutoM = ...
~~~
</div>

We can do the same thing with `Reader`, btw --- compose a part of our program
that would like a common global environment, and then use it in a bigger
program that does not:

~~~haskell
!!!machines/AutoReader.hs "runReaderAuto ::" "sealReaderAuto ::" machines
~~~

`sealReaderAuto` here takes a `AutoM (Reader r)` and a permanent unchanging
`r`, and returns just an normal `Auto` that repeatedly runs it with the given
`r`.

Recursive Auto
--------------

Let's move back to our normal `Auto` for now, and imagine a very common use
case that might come up.

What if you wanted two chained `Auto`s to "talk to eachother" --- for their
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

Let's see how we would write this using our `Auto`s:

~~~haskell
piTargeter :: Auto Double Double
piTargeter = proc control -> do
    let err = control - currResp

    errSums <- summer -< err

    currInp  <- summer -< 0.2 * err + 0.01 * errSums
    currResp <- blackBoxSystem  -< currInp

    id -< currVal
  where
    blackBoxSystem = id         -- just to simplify things
~~~

So this is an `Auto` that takes in a `Double` --- the control --- and outputs
a `Double` --- the response.  The goal is to get the response to "match"
control, by running a value, `currInp`, through a "black box system" (To
simplify here, we're only running `currInp` through `id`).

The logic is this:

1.  The "error" value `err` is the difference of the control and the current
    response.
2.  The "sum of errors" `errSums` value is the cumulative sum of all of the
    error values so far.
3.  The current input `currInp` is adjusted at every step with `err` and
    `errSums`.
4.  `currInp` is fed through our black bock system to get the current
    response.  In this case, it's just `id`, so it returns it as-is.


Look at what we wrote; isn't it just...beautifully declarative?  Elegant?  We
basically just took the description word-for-word and translated it.  We
didn't need to worry about keeping track of state...swapping variables...there
are no loops.  It all just basically popped out exactly like we would have
"said" it.

But, do you see the problem?  To calculate `err`, we used `currResp`.  But to
get `currResp`, we need `err`!

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
!!!machines/Auto3.hs "piController ::" machines
~~~
