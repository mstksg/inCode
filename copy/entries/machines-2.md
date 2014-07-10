Intro to Machines & Arrows: the Category Typeclass (Part 2)
===================================================================

Categories
:   Haskell
:   Ramblings
Tags
:   haskell
:   functional reactive programming
:   machines
:   arrows
:   category
CreateTime
:   2014/02/04 21:18:53
PostDate
:   Never
Series
:   Intro to Machines and Arrows
Identifier
:   machines-2

In this post we're going to be continuing on with building up from simple
machines like Stream and Auto to a semantic picture of Arrowized Functional
Reactive Programming.

In [the last post][part1], we introduced two simple machines, the Stream and
the Auto.  In this post we're going to be focusing on Auto and recognizing it
as a member of a very powerful typeclass (or two!).  In the next, we bridge
the gap and introduce the final machine we'll be working with, the Wire, and
transition into the popular AFRP library [netwire][].  Join along if you wish!

[part1]: http://blog.jle.im/entry/intro-to-machines-arrows-part-1-stream-and
[netwire]: http://hackage.haskell.org/package/netwire

Recap
-----

In the last post, we introduced first a simple stream ---

~~~haskell
!!!machines/Stream.hs "newtype Stream"
~~~

Which we saw as a an infinitely long linked list, or an infinitely long stream
of values that had:

1.  An internal state that progresses deterministically as a function of the
    last.
2.  An output (a "head") of type `b`, a function of the internal state.

We saw this with a [simple stream][simplestream] that counts from 1 to
infinity, whose state was a number that counted up and whose output at every
step was just that same number.

!!![simplestream]:machines/Stream.hs "myStream:"

We saw that having a time-varying behavior that could not be affected by the
outside world was kinda limiting, so we then looked at Auto:

~~~haskell
!!!machines/Auto.hs "newtype Auto"
~~~

Which is a stream, but every time you "ask" for the next value, you give an
"influencing input".  Autos have:

1.  An influencing input of type `a` that is taken at every step
2.  An internal state that is a function of the previous state and the
    influencing input
3.  An output "head" of type `b`, a function of the internal state.

And we looked at [a simple auto][simpleauto] we which was like our stream, but
at every step could be "reset" with a value.

!!![simpleauto]:machines/Auto.hs "settableAuto:"

Then we took another approach to looking at this --- we thought about Autos as
functions "with state".  As in, `Auto a b` was like a function `a -> b`, but
which had an internal state that updated every time it was called.  In this
sense, `Stream b` was like a function `() -> b`, or a "constant"...yet the
constant possibly changed every time you asked for it.

We saw this in an auto that [returns the sum][summer] of everyhting you have
given it.

!!![summer]:machines/Auto.hs "summer:"

Autos are "function-like things"...they map or "morph" things of type `a` to
things of type `b` in some form, just like functions.  It looks like we
stumbled onto some sort of design pattern.  Wouldn't it be cool if we could
treat Autos the same way we treat functions?  And reason about them the same
way, think about them using the same logic?

What is the *essense* of function-like-ness?

The Essense of Function-like-ness
---------------------------------

Okay, so using "function-like-ness" more than a few times is slightly silly so
I'm going to introduce a word or two.  These things I will call *morphisms*.
Sometimes you'll see them called "arrows", but this is a slightly loaded word
as there is an Arrow typeclass in Haskell that doesn't exactly refer to the
same thing.

Here is my claim: the "essense" of this functionlikeness is their ability to
*compose* and "chain", and the *nature* of that composition process.

That is, if you have a morphism `f` from `a` to `b`, and a morphism `g` from
`b` to `c`, then you can "compose" or "chain" them to get a morphism from `a`
to `c`.

In Haskell we use the `(.)` operator for this --- to say more formally:

~~~haskell
f     :: Morphism a b
g     :: Morphism b c
g . f :: Morphism a c
~~~

Some important aspects of the nature of this composition is that it must
"associate".  That means:

~~~haskell
(h . g) . f == h . (g . f)
~~~

Composing the composition of `h` and `g` to `h` should be the same as
composing `h` with the composition of `g` and `f`.

The final feature is that there must exist some "identity" morphism that
leaves other morphisms unchanged under composition:

~~~haskell
id :: Morphism b b

id . f  == f
g  . id == g
~~~

It doesn't really matter what `id` literally "does" --- it only matters that
it leaves morphisms unchanged.

And...that's it!

### Functions are morphisms

We're just going to take a quick detour verify that normal functions satisfy
this new notion of "function-likeness"...so that we aren't crazy.

In Haskell, our functions are things of type `a -> b` --- a morphism from
`a` to `b`.

Our composition operator is the familiar `(.)` from Prelude.  You can prove
all of the above laws yourself using the definition of `(.)`:

~~~haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
g . f = \x -> g (f x)
~~~

In practice, you can see associativity

~~~haskell
ghci> ((< 20) . (+4)) . (^2) $ 4
True
ghci> (< 20) . ((+4) . (^2)) $ 4
True
~~~

The identity is just Prelude's `id`:

~~~haskell
id :: a -> a
id x = x
~~~

~~~haskell
ghci>      (*3) $ 7
21
ghci> id . (*3) $ 7
21
ghci> (*3) . id $ 7
21
~~~

<div class="note">
**Aside**

I mean it, you can prove it yourself if you are bored some time :)  I'll start
you off with one of the identity laws:

~~~haskell
g . id = \x -> g (id x)
       = \x -> g x
       = g
~~~
</div>

So cool...this intuition applies to our actual idea of functions, so we are on
a sane track!

### Autos are morphism!

So we see that functions fit this idea.  Let's jump back to what we were
trying to show in the first place --- that Autos fit this "idea" of
function-like things, or morphisms.

Let's say I had an `f :: Auto a b` and a `g :: Auto b c`.  I want to "compose"
them --- `g . f`.  To get an `Auto a c`, somehow.  What would that even mean?

Well...if we think of `f` like a stateful function that takes in an `a` and
pops out a `b`...and `g` like a stateful function that takes in a `b` and pops
out a `c`...We can think of `g . f` as a stateful function that takes in an
`a`, feeds it to `f`, gets the `b` that `f` pops out, feeds that to `g`, and
gets the final `c` out at the end.

Also, Autos spit out both the result (the `c`) and the "updated Auto"...so the
updated Auto of the composition can just be the composition of the updated
Autos!

Enough talk, let's code!  We'll call our composition operator `(~.~)`.

~~~haskell
(~.~) :: Auto b c -> Auto a b -> Auto a c
g ~.~ f = ACons $ \x -> let (y, f') = runAuto f x
                            (z, g') = runAuto g y
                        in  (z, g' ~.~ f')
~~~

And...that should be it!  We run the input through first `f` then `g`,
collecting the "modified `f` and `g`", and returning both of those at the end,
composed.

Let's write a useful helper function so that we have more things to test this
out on:

~~~haskell
toAuto :: (a -> b) -> Auto a b
toAuto f = ACons \x -> (f x, toAuto f)
~~~

`toAuto` basically turns a function `a -> b` into a stateless `Auto a b`.

Time to test these out!

~~~haskell
ghci> let doubleA  = toAuto (*2)      :: Auto Int Int
ghci> let succA    = toAuto (+1)      :: Auto Int Int
ghci> let constA x = toAuto (const x) :: a -> Auto b a

ghci> testAuto_ doubleA [1..10]
[2,4,6,8,10,12,14,16,18,20]

ghci> testAuto_ (succA ~.~ doubleA) [1..10]
[3,5,7,9,11,13,15,17,19,21]

ghci> testAuto_ (succA ~.~ constA 20) [1,2,undefined]
[21,21,21]

ghci> testAuto_ summer [5,1,9,2,-3,4]
[5,6,15,17,14,18]

ghci> testAuto_ (double ~.~ summer) [5,1,9,2,-3,4]
[10,12,30,34,28,39]

ghci> testAuto_ settableAuto [Nothing,Nothing,Just (-3),Nothing,Nothing]
[1,2,-3,-2,-1]

ghci> testAuto_ summer [1,2,-3,-2,-1]
[1,3,0,-2,-3]

ghci> testAuto_ (summer ~.~ settableAuto)
    |     [Nothing,Nothing,Just (-3),Nothing,Nothing]
[1,3,0,-2,-3]

ghci> take 10 $ testAuto_ (settableAuto ~.~ constA Nothing) [(),()..]
[1,2,3,4,5,6,7,8,9,10]
~~~

And it looks like our Autos really can meaningfully compose!

Well, wait.  We need one last thing: the identity Auto:

~~~haskell
idA :: Auto a a
idA = ACons $ \x -> (x, idA)
~~~

~~~haskell
ghci> testAuto_ summer [5,1,9,2,-3,4]
[10,12,30,34,28,39]

ghci> testAuto_ (idA ~.~ summer) [5,1,9,2,-3,4]
[10,12,30,34,28,39]

ghci> testAuto_ (summer ~.~ idA) [5,1,9,2,-3,4]
[10,12,30,34,28,39]
~~~

### Category

These concepts are actually formalized in the mathematical concept of a
"category" --- things with objects and morphisms between them, following
certain properties (like the ones I mentioned earlier).

In Haskell, we often consider our objects as Haskell types; our usual
morphisms is the function arrow, `(->)`[^func] --- but in this case, it might be
interesting to consider a different category --- the category of Haskell types
and morphisms `Auto`.

[^func]: Remember, we can write `a -> b` as `(->) a b`; like other operators,
`(->)` can be used both infix and prefix.

In Haskell, we have a typeclass that allows us to do generic operations on all
Categories --- so now we can basically treat `Auto`s "as if" they were `(->)`.
We can literally "abstract over" the idea of a function.  Neat, huh?

~~~haskell
class Category r where
    id  :: r a a
    (.) :: r b c -> r a b -> r a c
~~~

Basically, with Category, we can "abstract over" function composition and
`id`.  That is, insteaed of `(.)` being only for composing normal
functions...we can use to compose morphisms in any Category, like Auto!  We
can also write "generic code" that works on *all* morphisms --- not just
`(->)`!  This is like having functions `mapM` and `sequence` --- which work
for *all* Monads, not just IO or Maybe or something.  We can reason about
Monads as things on their own, instead of just as isolated instances.

First, let's write the `(->)` Category instance:

~~~haskell
instance Category (->) where
    id    = \x -> x
    g . f = \x -> g (f x)
~~~

And then our `Auto` Category instance:

~~~haskell
instance Category Auto where
    id    = ACons $ \x -> (x, id)
    g . f = ACons $ \x -> let (y, f') = runAuto f x
                              (z, g') = runAuto g y
                          in  (z, g' . f')
~~~

And now... we can work with both `(->)` and `Auto` as if they were the "same
thing" :)

~~~haskell
doTwice :: Category r => r a a -> r a a
doTwice f = f . f
~~~

~~~haskell
ghci> doTwice (*2) 5
20
ghci> testAuto_ (doTwice (toAuto (*2))) [5]
[20]
ghci> testAuto_ (doTwice summer) [5,1,9,2,-3,4]
[5,11,26,43,57,61]
ghci> take 6 $ testAuto_ (doTwice summer) (repeat 1)
[1,3,6,10,15,21]
~~~

The main cool thing here is that we can now abstract over the "idea" of `id`
and `(.)`, and now our Autos have basically not only captured the idea of
function-ness, but can now literally act like normal functions in our code.  I
mentioned something similar in an earlier post [in MonadPlus][monadplus] ---
the ability to have a "common language" to talk and abstract over many things
is powerful not only for expressiveness but for reasoning and maintainability.

[monadplus]: http://blog.jle.im/entry/the-list-monadplus-practical-fun-with-monads-part

More Typeclasses!
-----------------

Anyways, we love typeclasses so much.  Let's get more familiar with our Auto
type and see what other useful typeclases it can be :)  Not only are these
good excercises for understanding our type, we'll also be using these
instances later!

### Functor

Functor is always a fun typeclass!  One way to think of a Functor is that `f
a` encapsulates the idea of some sort of "producer of `a`".  `Maybe a`
produces an `a` when used with `fromMaybe d`; `IO a` is a computation that
computes an `a`, `Reader r a` produces an `a` when given an `r`.

So if you have `f a`, we have a handy function `fmap`:

~~~haskell
fmap :: Functor f => (a -> b) -> f a -> f b
~~~

Which says, "If you have an `a -> b`, I can turn a producer of `a`'s into a
producer of `b`'s."

There are some laws associated with fmap --- most importantly that `fmap id =
id`.

Can we turn `Auto` into a Functor?

...well, no, we can't.  Because `fmap :: (a -> b) -> Auto a -> Auto b` makes
no sense...Auto takes two type parameters, not one.

But we *can* think of `Auto r a` as a "producer of `a`"s, when used with
`runAuto`.  Our Functor is `Auto r`:

~~~haskell
fmap :: (a -> b) -> Auto r a -> Auto r b
~~~

Which says, "Give me any `a -> b`, and I'll take an Auto that takes an `r` and
returns an `a`...and give you an Auto tht takes an `r` and returns a `b`".

How can I do that?

Well...for one...I can just "run" the Auto you give me to get the `a`...and
then apply the function to that `a` to get the `b`!

For example, if I fmapped `show` onto `summer` --- if `summer` was going to
output a 1, it will now output a `"1"`.  It turns an `Auto Int Int` into an
`Auto Int String`!

~~~haskell
instance Functor (Auto r) where
    fmap f a = ACons $ \x -> let (y  , a') = runAuto f x
                             in  (f y, fmap f a')
~~~

~~~haskell
ghci> testAuto_ (fmap show summer) [5,1,9,2,-3,4]
["5","6","15","17","14","18"]
~~~

Functor, check!

What's next?

### Applicative

Everyone knows that the "cool", *hip* typeclasses are the classic trio,
[Functor, Applicative, Monad][fam].  Let's just move on right along and go for
Applicative.

[fam]: http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html

If we continue the same sort of pattern that we did with Functor (Functors as
producers-kinda), Applicative gives you two things: the ability to "create a
new 'producer'", and the ability to take something that produces a function
and something that produces a value and squash it into something that produces
the application of the two.

This stuff...is really better said in types.

~~~haskell
class Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
~~~

In `pure`, give me an `a` and I'll give you a "producer" of `a`.  In `(<*>)`,
give me a producer of `a -> b` and a produer of `a` and I'll give you a
producer of `b`.

We can pretty much use this to write our Applicative instance for `Auto r`.

~~~haskell
instance Applicative (Auto r) where
    pure y    = ACons $ \_ -> (y, pure y)
    af <*> ay = ACons $ \x -> let (f, af') = runAuto af x
                                  (y, ay') = runAuto ay x
                              in  (f y, af' <*> ay')
~~~

Note that `pure` gives us a "constant Auto" --- `pure` is basically the
`constA` that we wrote before.

The useful thing about Applicative is that it gives us `liftA2`, which allows
us to apply a function "over Applicative"s.

~~~haskell
liftA2 :: (a -> b -> c) -> Auto r a -> Auto r b -> Auto r c
~~~

That is, it "feeds in" the input to *both* the `Auto r a` and the `Auto r b`,
applies the function to them, and the returns the result.

~~~haskell
ghci> testAuto_ summer [5,1,9,2,-3,4]
[5,6,15,17,14,18]
ghci> let addSumDoub = liftA2 (+) doubleA summer
ghci> testAuto_ addSumDoub [5,1,9,2,-3,4]
[15,8,33,21,8,26]
-- [5 + 10, 6 + 2, 15 + 18, 17 + 4, 14 - 6, 18 + 8]
~~~

Hopefully by now you've seen enough usage of the `Auto` type and writing
`Auto` combinators that do useful things that you are now Auto experts :) Feel
free to press pause here, because we're going to ramp up to some more
unfamiliar abstractions.  If you don't understand some of the examples above,
feel free to tinker with them on your own until you are comfortable.  And as
always, if you have any questions, feel free to leave a comment or drop by the
freenode #haskell channel.

Okay, now on to...

### Monad

Sykes!! We're not going to make a Monad instance :)  Even though it is
possible, a Monad instance for `Auto` is remarkably useless.  We won't be
using a monadic interface when working with Auto, so forget about it!

Arrow
-----

Okay.  As it turns out, `Category` by itself is nice, but for many of the
things we will be playing with function composition, it's just not going to
cut it.

We'd like to be able to "side chain" compositions.  That is, split off values,
perform different compositions on both forks, and recombine them.  We require
sort of basic set of combinators on top of our Category instance.

The Arrow typeclass was invented for just this --- a grab-bag of combinators
that allow such side-chaining, forking, merging behavior.


~~~haskell
class Category r => Arrow r where
    arr    :: (a -> b) -> r a b
    first  :: r a b -> r (a, c) (b, c)
    second :: r a b -> r (c, a) (c, b)
    (***)  :: r a b -> r c d -> r (a, c) (b, d)
    (&&&)  :: r a b -> r a c -> r a (b, c)
~~~

`arr` turns any `a -> b` function into an `Auto a b`.  `first` turns an `Auto
a b` into an `Auto (a, c) (b, c)` --- an Auto that operates on single values
to an Auto that operates only on the first part of a tuple.

`(***)` chains Autos side-by-side: `Auto a b -> Auto c d -> Auto (a, c) (b,
d)`.  It basically has each Auto operate on the tuple "in parallel".

`(&&&)` "forks".  Give an `Auto a b` and an `Auto a c`, and it'll create a
"forking" `Auto a (b, c)`.

Writing the instance is straightforward enough:

~~~haskell
instance Arrow Auto where
    arr f     = ACons $ \x -> (f x, arr f)
    first a   = ACons $ \(x,y) ->
                  let (x', a') = runAuto a x
                  in  ((x', y), first a')
    second a  = ACons $ \(x,y) ->
                  let (y', a') = runAuto a y
                  in  ((x, y'), second a')
    a1 *** a2 = ACons $ \(x,y) ->
                  let (x', a1') = runAuto a1 x
                      (y', a2') = runAuto a2 y
                  in  ((x',y'), a1' *** a2')
    a1 &&& a2 = ACons $ \x ->
                  let (y1, a1') = runAuto a1 x
                      (y2, a2') = runAuto a2 x
                  in  ((y1,y2), a1' &&& a2')
~~~

<div class="note">
**Aside**

We can also just take a shortcut and implement these in terms of combinators
we have already written from different typeclasses:

~~~haskell
instance Arrow Auto where
    arr f     = fmap f id
    first a   = liftA2 (,) (a . arr fst) (arr snd)
    second a  = liftA2 (,) (arr fst) (a . arr snd)
    a1 *** a2 = second a2 . first a1
    a1 &&& a2 = (a1 *** a2) . arr (\x -> (x, x))
~~~

Remember, `id` is the identity Auto... and `fmap f` applies `f` "after" the
identity.  So this makes sense.

`first` is a little tricker; we are using `liftA2 (,)` on two Autos, kind of
like we used before.  `liftA2` says "run these two Autos in parallel, and then
at the end, `(,)`-up their results."

The first of those two autos is `a . arr fst` --- get the first thing in the
tuple, and then chain the `a` auto onto it.  The second of those two autos
just simply extracts out the second part of the tuple.

~~~haskell
a           :: Auto a b
arr fst     :: Auto (a, c) a
a . arr fst :: Auto (a, c) b
arr snd     :: Auto (a, c) c
liftA2 (,) (a . arr fst) (arr snd) :: Auto (a, c) (b, c)
~~~

The rest I think should be a straightforward, as they all refer to other parts
of `Arrow`.

What does this show?  Well, that `Arrow` really isn't anything too
special...it's really just what we already had --- a `Category` with
`Applicative`.  But we are able to define more efficient instances, and also
sort of look at the problem in a "different way".
</div>

What we have here isn't really anything too mystical.  It's just some basic
combinators.  And like the aside says, we didn't introduce any "new power" ---
we already had a free Arrow instance with our Category and Applicative
instances.

The main point is just that we have these neat combinators to chain things in
more useful and expressive ways --- something very important when we
eventually go into AFRP.

#### ArrowChoice

Another useful set of combinators is `ArrowChoice`, which provides `left`:

~~~haskell
left :: Auto a b -> Auto (Either a c) (Either b c)
~~~

It applies the Auto to the `Left` case, but leaves the `Right` case unchanged.

~~~haskell
instance ArrowChoice Auto where
    left a = ACons $ \x ->
                 case x of
                   Left l  -> let (l', a') <- runAuto a l
                              in  (Left l', left a')
                   Right r -> (Right r, left a')
~~~


### Proc Notation

<!-- The *main* purpose for Arrow (in our situation) is that now that we have -->
<!-- instanced Arrow for our Autos, we can now use it in proc notation. -->

<!-- This is similar to how once we instance something as a Monad, we can use it in -->
<!-- "do" notation. -->

<!-- Proc notation is basically "do notation for Arrows", and is just syntactical -->
<!-- sugar for the various Arrow combinators we described before. -->

<!-- Proc notation consists of lines of cute little ASCII "arrows": -->

<!-- ~~~haskell -->
<!-- output <- arrow -< input -->
<!-- ~~~ -->

<!-- where `arrow` is the Arrow, `input` is the "input" fed into the Arrow, and -->
<!-- "output" binds the result to the name "output".  We can omit "output" and -->
<!-- we will "forget" the output. -->

<!-- Cute, right? -->

<!-- For example, to write our `succ . double` composition we wrote earlier: -->

<!-- ~~~haskell -->
<!-- doubleSucc1 :: Auto Int Int -->
<!-- doubleSucc1 = succ . double -->

<!-- doubleSucc2 :: Auto Int Int -->
<!-- doubleSucc2 = proc n -> do -->
<!--   doubled <- double -< n -->
<!--   succ -< doubled -->
<!-- ~~~ -->

<!-- What if we wanted the arrow to return a tuple with the result of doubling, and -->
<!-- also the result of doubling then succing?? -->

<!-- ~~~haskell -->
<!-- doubleSucc'1 :: Auto Int (Int,Int) -->
<!-- doubleSucc'1 = (id *** succ) . (double &&& id) -->

<!-- doubleSucc'2 :: Auto Int (Int,Int) -->
<!-- doubleSucc'2 = proc n -> do -->
<!--   doubled      <- double -< n -->
<!--   doubleSucced <- succ   -< doubled -->
<!--   returnA -< (doubled, doubleSucced) -->
<!-- ~~~ -->

<!-- `returnA` is just `id` (for Category Auto), the identity Arrow.  But we call -->
<!-- it `returnA` to draw an analogy between `return` for Monads.  They both serve -->
<!-- the same purpose --- they take normal values and turn them into something you -->
<!-- can use in a do/proc block. -->

<!-- So basically, every line in a proc block must look like: -->

<!-- ~~~haskell -->
<!-- arrow -< input -->
<!-- ~~~ -->

<!-- Or, if you want to name the result for later use, -->

<!-- ~~~haskell -->
<!-- output <- arrow -< input -->
<!-- ~~~ -->

<!-- Just like how "do" blocks compose several monad values into one giant monad -->
<!-- value, "proc" blocks compose several morphisms/arrows into one giant arrow. -->

<!-- Remember that proc blocks don't actually "do" anything.  You aren't sequencing -->
<!-- actions.  You basically are creating a *dependency* graph --- saying which -->
<!-- arrows depend on the output of which arrows, and how they all twist and -->
<!-- combine together. -->

<!-- In `doubleSucc'2`, we are saying this: -->

<!-- "If you want to run this arrow, and you give us a value `n`, then the result -->
<!-- is a tuple `(doubled, doubleSucced)`, where `doubled` is the result of running -->
<!-- that `n` through `double`, and `doubleSucced` is the result of running -->
<!-- `doubled` through `succ`." -->

<!-- So when we eventually use proc notation with our Auto, it describes one giant -->
<!-- "tick" of the big function. -->

<!-- #### proc rec -->

<!-- Finally, we often will need to have Autos that "depend" on eachother in a -->
<!-- cyclic way.  For example, in a harmonic oscillator system, the position -->
<!-- depends on the force applied to the object, but the force applied depends on -->
<!-- the position. -->

<!-- These "recursive" bindings come from combinators provided by the ArrowLoop -->
<!-- typeclass: -->

<!-- ~~~haskell -->
<!-- object :: Auto () Int -->
<!-- object = proc _ -> do -->
<!--   rec -->
<!--     let acc = -1 * pos -->
<!--     vel <- summer -< acc -->
<!--     pos <- summer -< vel -->
<!--   returnA -< pos -->
<!-- ~~~ -->

<!-- So we can have `pos` depend on `acc`, and `acc` depend on `pos`. -->

<!-- Hopefully our Autos im -->
