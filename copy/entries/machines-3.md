Effectful Arrows: Intro to Machines and Arrow Part 3
====================================================

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
that your context has to be *monadic*, in order to allow you to do this.

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
`Arrow`, `ArrowPlus`, etc., you can directly use the intution that you gained
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

### Putting it to use

Let's look at some esoteric and contrived applications and some actual
useful ones.

First some utility functions: `autoM`, which upgrades an `Auto a b` to an
`Auto m a b` for any `Monad` m, and `arrM`, which is like `arr`, but instead
of turning an `a -> b` into an `Auto a b`, it turns an `a -> m b` into an
`AutoM m a b`:

~~~haskell
!!!machines/Auto3.hs "autoM ::" "arrM ::" machines
~~~

We will need to of course re-write our trusty `testAuto` functions from the
first entry, which is again a direct translation of the original ones:

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

#### Motivation for IO

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
it.  In addition, one thing danger from manually treading a `String` input
throughout your entire composition is that somewhere along the way, you can
chose to "change" the input before it's passed, meaning that you can't
gauruntee that the `String` that `replicateGets` eventually gets is the same
one that was originally "gotten" before the whole thing was run anyway.

(This is indeed a problem solved by having `AutoM`, but in real life, perhaps
`Reader String` would be a better way to solve this specific situation than
`IO`)

For `logging`, if you only want to log a small portion of your entire `Auto`
--- that is, only log one small composed `Auto` out of an entire big
composition --- if you take the "thread the output" approach, you're going to
have to manually thread the output throughout the entire composition to the
end.  And, back to our "locally stateful" emphasis from before, every other
`Auto` in your composition has accessed to the logged items...even if you
wanted to keep it hidden.  And what if you wanted to log five, six `Auto`s to
five or six different files?  You're going to be passing five or six strings
from different places!  In this case, you gain a lot from implicit
logging.

The point here is that there is a trade-off in either case.  But these monadic
compositions really just give us another tool in our toolset that we can
(judiciously) use.

#### Other contexts

It's fun to imagine what sort of implications the different popular monads in
Haskell can provide.  `Writer` gives you a running log that all `Auto`s can
append to, for example.

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
usage of `Auto` in the first place.  But, we can make sound and judicious
decisions without resorting to "never do this" dogma.[^dogma]  Remember, these
are just tools we can possibly explore.  Whether or not they work in the real
world --- or whether or not they are self-defeating --- is a complex story!

[^dogma]: Which really isn't the point of these posts anyway!

I will however offer one tried-and-true monadic context that is used *to great
extent* in the real world and in real life applications of `Auto`.  In fact,
it is one of *the critical abstractions* that even *allows* `Auto` to be used
in Functional Reactive Programming: `Reader`.

With `AutoM (Reader r) a b`:

~~~haskell
stepAutoM :: AutoM (Reader r) a b -> a -> (... next ...)

-- really becomes a

stepAutoM :: AutoM (Reader r) a b -> a -> r -> (... next ...)
~~~

Meaning, instead of just passing an `a` to get the next step, you have to pass
both an `a` *and* an `r` for every step.

Having a `Reader r` environment gives every `Auto` access to shared, read-only
global data.  But `Reader r` is *much more useful* than just "passing the
parameter".  It's much more useful than manually explicitly requiring an `a`
and an `r`.  `AutoM (Reader r) a b` is *more useful* than `Auto (a, r) b`.
Why?

Because we can *guarantee* that *every composed Auto* will, for every step,
*receive the same `r`*.  With the manual parameter passing method, any `Auto`
along the way can modify what they pass down along.  Using `Reader r` will
guarantee that, across every "tick", every `Auto` gets the same `r`.

We will see later that our ability to do this makes it possible to implement
semantics-following FRP using `Auto`s.



