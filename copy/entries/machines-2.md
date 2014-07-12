Auto as Category, Applicative, & Arrow (Intro to Machines/Arrows Part 2)
========================================================================

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

Welcome back!  It's been a while since the last post, admittedly; sorry!  In
this post we'll be continuing on from [the previous post][part1].  In
particular, we're going to be looking at the `Auto` type as something that is
a part of a pretty powerful pattern of abstraction, and try to exploit it to
write concise, expressive code using Auto composition and proc notation.

[part1]: http://blog.jle.im/entry/intro-to-machines-arrows-part-1-stream-and

And eventually, we're going to tie it all together into how it fits into the
semantics and implementation of Functional Reactive Programming!  Yay!

As always, feel free to leave a comment if you have any questions, or try to
find me on [twitter][], or drop by the #haskell Freenode IRC channel!  (I go
by *jle`*)

[twitter]: https://twitter.com/mstk "Twitter"

Note that all of the code in this post can be downloaded (from
[Auto.hs][autodl] for the last post, and [Auto2.hs][auto2dl] for this post's
new material) so you can play along on GHCi, or write your own code using it
the concepts and types here :)  You can also run it [online
interactively][fpcomplint].

!!![autodl]:machines/Auto.hs
!!![auto2dl]:machines/Auto2.hs
[fpcomplint]: https://www.fpcomplete.com/user/jle/machines

A fair warning: at times this post might feel a bit fragmented; but remember
that we really are just going to be exploring and getting very familiar with
the Auto type and building an intuition.  Everything comes to a mini-climax at
the end, and a final satisfying one at the next post --- kind of like every
Part 2 in every trilogy ever, you know? :)

Recap
-----

We left off in our last post having looked at `Auto`:

~~~haskell
!!!machines/Auto.hs "newtype Auto" machines
~~~

which we saw as a stream that had an influencing input of type `a`, an
internal, opaque state (a function of the input and of the previous state),
and an output "head" of type `b` (also a function of the input and of the
previous state).

And we looked at [a simple auto][simpleauto] which acted like a constantly
incrementing stream, but where you could reset the counter by passing in a
`Just`.

!!![simpleauto]:machines/Auto.hs "settableAuto:"

Then we took another approach to looking at this --- we thought about Autos as
functions "with state".  As in, `Auto a b` was like a function `a -> b`, but
which had an internal state that updated every time it was called.

We saw this in an auto that [returns the sum][summer] of everything you have
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

I'm going to introduce some formality and call things "function-like-things"
*morphisms*. Sometimes you'll see them called "arrows", but this is a slightly
loaded word as there is an Arrow typeclass in Haskell that doesn't exactly
refer to the same thing.

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
g . id = \x -> g (id x)     -- definition of (.)
       = \x -> g x          -- definition of id
       = g                  -- eta reduction
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
!!!machines/Auto2.hs "toAuto ::" machines
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
!!!machines/Auto2.hs "idA ::" machines
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

Just be sure to use the write imports so you don't have name clashes with the
Prelude operators:

~~~haskell
import Control.Category
import Prelude hiding (id, (.))
~~~

First, let's write the `(->)` Category instance:

~~~haskell
instance Category (->) where
    id    = \x -> x
    g . f = \x -> g (f x)
~~~

And then our `Auto` Category instance:

~~~haskell
!!!machines/Auto2.hs "instance Category Auto" machines
~~~

And now... we can work with both `(->)` and `Auto` as if they were the "same
thing" :)

~~~haskell
!!!machines/Auto2.hs "doTwice ::" machines
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
type and see what other useful typeclasses it can be :)  Not only are these
good exercises for understanding our type, we'll also be using these
instances later!

### Functor

Functor is always a fun typeclass!  One use case of Functor is as a "producer
transformer" --- the `f a` is some "producer of `a`".  `IO a` is a computation
that produces an `a`; `Reader r a` produces an `a` when given an `r`.

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
returns an `a`...and give you an Auto that takes an `r` and returns a `b`".

How can I do that?

Well...for one...I can just "run" the Auto you give me to get the `a`...and
then apply the function to that `a` to get the `b`!

For example, if I fmapped `show` onto `summer` --- if `summer` was going to
output a 1, it will now output a `"1"`.  It turns an `Auto Int Int` into an
`Auto Int String`!

~~~haskell
!!!machines/Auto2.hs "instance Functor (Auto r)" machines
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
new 'producer'" producing a given value, and the ability to take something
that produces a function and something that produces a value and squash it
into something that produces the application of the two.

This stuff...is really better said in types.

~~~haskell
class Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
~~~

In `pure`, give me an `a` and I'll give you a "producer" of `a`.  In `(<*>)`,
give me a producer of `a -> b` and a producer of `a` and I'll give you a
producer of `b`.

We can pretty much use this to write our Applicative instance for `Auto r`.

~~~haskell
!!!machines/Auto2.hs "instance Applicative (Auto r)" machines
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

Take *that*, [tomtomtom7][]! :D

[tomtomtom7]: http://www.reddit.com/r/programming/comments/25yent/i_like_haskell_because_it_lets_me_live_inside_my/chmj8al

Arrow
-----

Okay.  As it turns out, `Category` by itself is nice, but for many of the
things we will be playing with function composition, it's just not going to
cut it.

As it turns out, we can actually sort of take advantage of a "do
notation-like" syntactical sugar construct to perform complex compositions.
But in order to do that, we first need to be able to "side chain"
compositions.  That is, split off values, perform different compositions on
both forks, and recombine them.  We require sort of basic set of combinators
on top of our Category instance.

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

In our case, `arr` turns any `a -> b` function into an `Auto a b`.  `first`
turns an `Auto a b` into an `Auto (a, c) (b, c)` --- an Auto that operates on
single values to an Auto that operates only on the first part of a tuple.

`(***)` chains Autos side-by-side: `Auto a b -> Auto c d -> Auto (a, c) (b,
d)`.  It basically has each Auto operate on the tuple "in parallel".

`(&&&)` "forks".  Give an `Auto a b` and an `Auto a c`, and it'll create a
"forking" `Auto a (b, c)`.

Writing the instance is straightforward enough:

~~~haskell
!!!machines/Auto2.hs "instance Arrow Auto" machines
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
anything we could "do" with Auto using Arrow, we could already do with
Category and Applicative.

The main point is just that we have these neat combinators to chain things in
more useful and expressive ways --- something very important when we
eventually go into AFRP.

~~~haskell
ghci> let sumDoub = summer &&& doubleA
ghci> testAuto_ sumDoub [5,1,9,2,-3,4]
[(5, 10), (6, 2), (15, 18), (17, 4), (14, -6), (18, 8)]
~~~

As we'll see, the *real* benefit of Arrow will be in the syntactical sugar it
provides, analogous to Monad's do-blocks.

#### ArrowChoice

Another useful set of combinators is the `ArrowChoice` typeclass, which
provides:

~~~haskell
left  :: Auto a b -> Auto (Either a c) (Either b c)
right :: Auto a b -> Auto (Either c a) (Either c b)
(+++) :: Auto a b -> Auto c d -> Auto (Either a c) (Either b d)
(|||) :: Auto a c -> Auto b c -> Auto (Either a b) c
~~~

If you look really closely...`left` is kinda like `first`; `right` is kinda
like `second`...`(+++)` is kinda like `(***)`, and `(|||)` is like a backwards
`(&&&)`.

If `Arrow` allows computations to be "side-chained", `ArrowChoice` allows
computations to be "skipped/ignored".

We'll instance `left`, which applies the given `Auto` on every `Left` input,
and passes any `Right` input along unchanged; the `Auto` isn't stepped or
anything.  The rest of the methods can be implemented in terms of `left` and
`arr`.

~~~haskell
!!!machines/Auto2.hs "instance ArrowChoice Auto" machines
~~~

We'll see `ArrowChoice` used in the upcoming syntactic sugar construct,
enabling for if/then/else's and case statements.  Don't worry about it for now
if you don't understand it.

### Proc Notation

Actually, here is the *real* reason Arrow is useful.  It's actually a pretty
well-kept secret, but...just like Monad enables *do notation* syntactical
sugar, Arrow enables *proc notation* syntactical sugar.  Which is probably
cooler.

Not gonna lie.

A lot of AFRP and a lot of what we're going to be doing will pretty much rely
on proc notation to be able to express complex compositions...rather
elegantly.

Proc notation consists of lines of "arrows":

~~~haskell
arrow -< x
~~~

which says "feed `x` through the Arrow `arrow`".

Like in monadic do-blocks, you can also "bind" the result, to be used later in
the block:

~~~haskell
y <- arrow -< x
~~~

Which says "feed `x` through the Arrow `arrow`, and name the result `y`".

Hey!  It looks like a little ASCII arrow!  Cute, huh?

The last line of a proc do block is the "return"/result of the block, like in
monadic do-blocks.

Let's write our first proc block; one that emulates our `liftA2 (+) doubleA
summer`:

~~~haskell
doubSummer :: Auto Int Int
doubSummer = proc x -> do
    summed  <- summer  -< x
    doubled <- doubleA -< x
    id -< summed + doubled
~~~

In the last line, we want to "return" `summed + double`; we have to put an
Arrow command there, so we can just feed `summed + double` through `id`, to
have it pop out at the end.

You can think of `id` like `return` in normal do notation.

#### Simple useful example

How about an `Auto (Either Int Int) (Int, Int)`, which maintains *two*
internal counters.  You increment the first one with an input of `Left x`, and
you increment the second one with an input of `Right x`.  The output is the
state of both counters.

We could write this "from scratch":

~~~haskell
!!!machines/Auto2.hs "dualCounterR ::" machines
~~~

But we know in Haskell that explicit recursion is usually a sign of bad
design.  So many potential places for bugs!

Let's try writing the same thing using Auto composition:

~~~haskell
!!!machines/Auto2.hs "dualCounterC ::" machines
~~~

That's a bit more succinct, but I think the proc notation is much nicer!

~~~haskell
!!!machines/Auto2.hs "dualCounterP ::" machines
~~~

It's a bit more verbose...but I think it's much clearer what's going on,
right?

~~~haskell
ghci> testAuto_ dualCounterP [Right 1, Left 2, Right (-4), Left 10, Right 3]
[(0, 1), (2, 1), (2, -3), (12, -3), (12, 0)]
~~~

#### Proc shines

And let's say we wanted another constraint.  Let's say that...for the `Left`
case, every *other* time it's a Left, *ignore the value* and don't add
anything.  That is, every second, fourth, sixth `Left i` input should ignore
the `i` and not add anything.

How would we do this in the explicit recursive case?  Why --- well, adding
another component to the "explicit state" tuple, and dealing with that when
necessary.

I don't even know how to begin writing it in a readable way using arrow
composition.

But the proc notation?  Piece of cake!

~~~haskell
!!!machines/Auto2.hs "dualCounterSkipP ::" machines
~~~

~~~haskell
ghci> testAuto_ dualCounterP [Right 1, Left 2, Right (-4), Left 10, Right 3]
[(0, 1), (2, 1), (2, -3), (2, -3), (2, 0)]
~~~

And that's something to write home about :)

### Locally Stateful Composition

The last example highlights something very significant about Autos and their
Arrow-based composition: Autos with composition allow you to make *locally
stateful compositions*.

What if we had done the above using some sort of state monad, or doing the
explicit recursion?

We'd have carried the "entire" state in the parameter:

~~~haskell
!!!machines/Auto2.hs "dualCounterSkipR ::" machines
~~~

Not only is it a real mess and pain --- and somewhere where bugs are rife to
pop up --- note the entire state is contained in one thing.  That means
everything has access to it; all access is sort of haphazard and ad-hoc, as
well.  Note that the `Right` case can do whatever it wants with the `s`.  It
has access to it, can read it, act on it, modify it...anything it wants!

In the proc example...the `s` is a `summer` that is "locked inside" the `Left`
branch.  `Right` branch stuff can't touch it.

In fact, all of the `summers` keep their own state, independently from
each other.  Nothing can really modify their state except for themselves, if
they chose to.  Less room for bugs, too, in adding, because you already know
that `summer` works.

This property --- that every single component maintains its own internal state
--- is, in my opinion, one of the most significant aspects of this whole
game with Auto and Category and Arrow etc.  Every component minds its own
state.

And also --- if we wanted to "add a new component" to the state, like we did,
we don't have to really change anything besides just plopping it on.  In the
explicit recursion example, we needed to go in and *change the state type* to
"make room" for the new state.  We needed to pretty much refactor the entire
thing!

This really demonstrates the core principles of what *composability* and
*modularity* even really *mean*.

Moving on
---------

Welp, hopefully by now you are experts at working with the Auto machine, and
understanding it as "function-like things".  You've gotten deep and intimate
by instancing some common typeclasses.

Then you saw Arrow, and understood how Auto fits into the Arrow abstraction.
And then you learned about proc notation, and how...everything just...fits
together.  And you can declare some nice computations/compositions in a way
that looks a lot like monadic do blocks.

We saw how complex compositions --- and complex recursions --- now look really
pretty and nice in proc-do notation.

And then we saw an extension of the "opaque state" concept we learned last
time --- *locally stateful compositions*.  Using Auto composition and the
Arrow instance, we can now combine Autos with local state together...and they
all maintain and keep track of their own state.  No more "global state", like
before --- every individual component only minds what it knows, and nothing
else.  And that this is really what "composability" really is all about.

Up next, we will transition from Auto to the Wire abstraction, which is sort
of like an Auto with more features.

Then we will finally bridge the gap between the Wire abstraction
implementation and the *semantic model* of FRP.

And then we will be on our way! :D


