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

### The Essense of Essenses

Pause for a bit.  Do you remember learning about Monoids?  If you haven't
actaully learned about them, monoids are things where it makes sense to define
an operation between two of them (for example, adding two numbers together)
and where there is a "thing" that doesn't change the thing it's being operated
with (for example, the number zero for addition of two numbers).  Also, the
order in which you apply the operation doesn't matter --- (1+2)+3 = 1+(2+3)

At first it's a little crazy to think that these two simple properties give
you anything of use to study.  They're way too general.  How can you learn
anything meaningful from only studying these extremely general properties?

As it turns out...there is actually a lot more than you'd initially expect.
[Brent Yorgey][Brent], maintainer and author of the popular Haskell library
[diagrams][] explains this perhaps [better than anyone][monoids].

[Brent]: http://www.cis.upenn.edu/~byorgey/pub/monoid-pearl.pdf
[diagrams]: http://hackage.haskell.org/package/diagrams
[monoids]: http://www.cis.upenn.edu/~byorgey/pub/monoid-pearl.pdf

Monoids can be said to encapsulate the essense of a sort of 'combining-ness'.
Combine two integers.  Combine two pictures.  Combine two lists.  Combine two
booleans.  And now, all of a sudden, we can study all of these extremely
different things under the same framework.  Conclusions we make when we talk
about adding integers now apply when we combine lists, figures...

The pattern is clear --- isolate the essense, and now we can study all
instances all the same way, and apply the conclusions we learn to all of them.

### The Essense of Morphisms

Okay, enough rambling.  I'm going to make a bold claim.  The *essense* of what
makes something function-like is three things:

1.  You must be able to "chain" (or more formally, "compose") them.  You have
    to be able to take a morphism `a -> b` and a morphism `b -> c` and make a
    new morphism `a -> c`, which applies the first and then the second.

    We denote this as an operator `.`, where `f . g` composes `g` and `f` ---
    it's a morphism that first applies `g`, then applies `f`.

2.  For every morphism there must be a morphism you can chain "before" it and
    the resulting chained morphism is identical to the original one.  There
    must also be one you can chain "after" it that will do the same thing.

    We denote this as `id`.  `f . id` must be identical to `f`, and `id . g`
    must be identical to `g`.

3.  As long as you compose a bunch of morphisms in the same order, it doesn't
    matter which ones you compose first.

    This means that `(f . g) . h` must be the same as `f . (g . h)`.

And...that's it!

#### The "Normal" Function

Just to prove that I'm not crazy, let's see that normal functions fulfill
this.

Anyways, we have the composition operator `(.)` that does exactly what the
definition above says --- `f . g` is a function that applies first `g`, then
`f`.

~~~haskell
λ: (*2) 5
10
λ: (+1) 10
11
λ: ( (+1) . (*2) ) 5
11
~~~

There is also `id`, which is a function that returns its input.  This fulfills
the second property.

~~~haskell
λ: (*2) 5
10
λ: ( (*2) . id ) 5
10
λ: ( id . (*2) ) 5
10
~~~

As you can see, `id . (*2)` and `(*2) . id` are both identical to just `(*2)`.

Here is a quick proof about the `id . f` law --- you can prove the
`f . id` law yourself as an exercise.

~~~haskell
-- definition of (.)
f . g = \x -> f (g x)

-- definition of id
id x = x

-- f . id == f
   id . f
== \x -> id (f x)       -- expand out the definition of (.)
== \x -> f x            -- expand out the definition of id
== f                    -- eta reduce, (\x -> f x) == f
~~~

You can also trust me when I say that function composition is associative
(functions can be combined in any order).  The proof is not too difficult,
either.

### Autos are morphisms!

Alright, let's see if we can get Autos to fulfill these properties.  And if
they do, then if I'm right, we can now reason about them the same way we
reason about normal functions.

(This kinda puts Object-Oriented abstractions to shame, huh?  In OOP we
can abstract Cats and Dogs to be Animals...in Haksell we abstract
functions themselves!)

#### Auto composition

Autos need to be able to be chained.

Let's think about two Autos `g :: Auto a b` and `f :: Auto b c`, and their
composition `f . g`.  What would it mean?

Well, it would yield a new big fat "mashed" Auto, `f . g :: Auto a c`, which
first runs the input through `g`, then runs that result though `f`, and pops
out the result of that.

`f` and `g` are functions with state.  So when we run `f . g`, we can say that
we run the input through `g`, then `f` --- and both times, "ticking" the state
as we go along.  So after we run `f . g`, we now have `f' . g'`, where `f'`
and `g'` are the updated-state functions.  And every time we run our `f . g`,
we update both states.

Let's write this out!

Let's use (`.~`) as our composition operator for now, to avoid confusion.
It's `(.)`, but for Autos!

~~~haskell
(.~) :: Auto b c -> Auto a b -> Auto a c
f .~ g = ACons $ \x ->
  let (y, g') = runAuto g x     -- run `g`
      (z, f') = runAuto f y     -- fun `f`
  in  (z, f' .~ g')
~~~

To test this, let's make a helpful function `autoize`, which takes a normal
function `a -> b` and turns it into an `Auto a b`, with no state.

~~~haskell
autoize :: (a -> b) -> Auto a b
autoize f = ACons $ \x -> (f x, autoize f)
~~~

Now to test with both these "pure" Autos and Autos we made last post --

~~~haskell
λ: let double = autoize (*2)
λ: let succ = autoize (+1)
λ: let constant x = autoize (const x)

λ: testAuto_ (succ .~ double) [1..10]
[3,5,7,9,11,13,15,17,19,21]

λ: testAuto_ (succ .~ constant 20) [1,2,undefined]
[21,21,21]

λ: testAuto_ summer [5,1,9,2,-3,4]
[5,6,15,17,14,18]

λ: testAuto_ double [5,6,15,17,14,18]
[10,12,30,34,28,36]

λ: testAuto_ (double .~ summer) [5,1,9,2,-3,4]
[10,12,30,34,28,36]

λ: testAuto_ settableAuto [Nothing,Nothing,Just (-3),Nothing,Nothing]
[1,2,-3,-2,-1]

λ: testAuto_ summer [1,2,-3,-2,-1]
[1,3,0,-2,-3]

λ: testAuto_ (summer .~ settableAuto)
  |    [Nothing,Nothing,Just (-3),Nothing,Nothing]
[1,3,0,-2,-3]

λ: take 10 $ testAuto_ (settableAuto .~ constant Nothing) [(),()..]
[1,2,3,4,5,6,7,8,9,10]

λ: take 10 $ testAuto_ (summer .~ double .~ constant 2) [(),()..]
[4,8,12,16,20,24,28,32,36,40]

λ: take 10 $ testAuto_ (summer .~ summer .~ constant 1) [(),()..]
[1,3,6,10,15,21,28,36,45,55]
~~~

Sweet!  It looks like it actually does make sense to compose Autos.  Maybe
Autos really are morphisms?  We need one more thing though.

#### The Identity Auto

This is the last thing we need to make to have all of the things we need to
consider Autos as full-fledged morphisms.  We need an identity Auto --- an
Auto that always returns exactly what it is given.

This is sort of trivial using even the combinators we have already defined ---
`idAuto = autoize id`.  But we can write it out by hand too.

~~~haskell
idAuto :: Auto a a
idAuto = ACons $ \x -> (x, idAuto)
~~~

~~~haskell
λ: testAuto_ double [1..10]
[2,4,6,8,10,12,14,16,18,20]

λ: testAuto_ (double .~ idAuto) [1..10]
[2,4,6,8,10,12,14,16,18,20]
~~~

### Category

As it turns out, I wasn't pulling all of this out of thin air --- all of these
properties are actually formalized in a formal mathematical concept.  The name
of this concept has developed quite a scary reputation within the Haskell
community --- especially for newcomers.  In fact the mere mention of the C
word has been known to strike fear in even the bravest of souls.

But now that we already understand its concepts, let's just get it over and
say it, Bella.

These ideas are formalized in the mathematical concept of a *category*.

Categories describe objects (in our case, Haskell values), *morphisms* between
these objects (in our case, `Auto a b` and `idAuto`) and the *composition* of
those morphisms, following the laws I mentioned before.

There's actually a typeclass in Haskell base that captures this.  It's called,
unsurprisingly, Category.

~~~haskell
class Category cat where
    (.) :: cat b c -> cat a b -> cat a c
    id  :: cat a a
~~~

A Category (capital C) has two things:

1.  An associative composition operator between two concrete members.
2.  A special identity member.

Which must follow three laws:

1.  `f . id` = `f`
2.  `id . g` = `g`
3.  `(f . g) . h` = `f . (g . h)`

Note that the full concrete *members* of Category, say, `cat a b`, are
morphisms/arrows --- those are the `f` and `g` mentioned here..

We know that `(->) a b` describes a morphism following the category laws, so
`(->)` is a Category:

~~~haskell
instance Category (->) where
    f . g = \x -> f (g x)
    id x  = x
~~~

And we know that `Auto a b` also describes a morphism following the category
laws, so `Auto` is a Category (from the functions we defined before):

~~~haskell
instance Category Auto where
    id    = ACons $ \x -> (x, id)
    f . g = ACons $ \x ->
              let (y, f') = runAuto f x
                  (z, g') = runAuto g y
              in  (z, f' . g')
~~~

And there you go.

(Remember then to hide `(.)` and `id` from Prelude, which by default only work
on the `(->)` Category:

~~~haskell
import Prelude hiding ((.),id)
~~~
)

What hath man wrought
---------------------

You might not have caught it, but we actually just did something amazing.

This is really a demonstration of the true power of Haskell typeclasses, and a
poignant example of the motivation behind typeclasses in the first place.

We just took two types with completely different implementations and
representations...and now are able to talk about them using the same language.

I talked about this in an earlier blog post [on MonadPlus][monadplus], where
we were able to use both Maybe and List in the exact same way by providing a
common language: `return` to represent success, and `mzero` to represent
failure.  We can use Maybe and List in the exact same way and reason about
them as if they were the "same thing", if we just used `return` and `mzero`.

[monadplus]: http://blog.jle.im/entry/the-list-monadplus-practical-fun-with-monads-part

For Functors, `fmap` for Maybe and `fmap` for List --- not to mention `fmap`
for IO and `fmap` for State --- have completely different implementations and
at the low level have nothing to do with eachother at all.  In many languages,
we would write a separate `fmap` function for every data structure --- a
`maybeMap`, an `ioMap`, a `stateMap`...

Yet, with typeclasses, we are now able to talk about all of these things in
the *exact same way* --- we capture the essential design pattern in `fmap (+1)
(Just 1)` and in `fmap (+1) [1,2,3]` and in `fmap (+1) readLn`.  We can use
operators like `(<$>)` and expect them to work the same high-level way for IO,
State, Maybe, List, etc.

Why not just have `maybeMap`, `ioMap`, etc.?  Why not just have `.` and `.~`,
`id` and `idAuto`?

Because now we can truly write *generic code* -- code that works for *all*
Functors, all Categories, *not even caring* what they actually are.

We can write `fmap (*2) x`, without even caring about what type `x` is, and
say "This code works for all Functors!  I don't even care!".  You can write `f
. g` and say "This works for all Categories!  This code is powerful and
generic!"

More Typeclasses!
-----------------

So because we now love typeclasses so much, let's see what other useful
abstractions we can apply to Auto.

This section is just going to be a whirlwind tour of instancing Auto as
various useful typeclasses --- mostly typeclassess that we'll be using later.

### Functor

Of course we already spent a lot of time talking about Functor, so we might as
well start here.

What is a functor?  It represents something that can be mapped over.  More
pedantically, it is something that implements a lawful `fmap`.

~~~haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
~~~

Can we make `Auto` a Functor?

Actually...well...not really.  It really doesn't make sense, if you think
about it.  If you look at the type signature for `fmap` and substitute `f ~
Auto`, you'll immediately see why:

~~~haskell
fmap :: (a -> b) -> Auto a -> Auto b
~~~

You can't have a function `Auto a -> Auto b`...it doesn't really make sense
because `Auto a` isn't even the type of a value, a concrete type.

Conceptually, you can think of this as asking "What am I even mapping over?",
and see that you can't really "map over" `a b` with one function.

*BUT*, it *does* make sense for `Auto i` to be a Functor:

~~~haskell
fmap :: (a -> b) -> (Auto i) a -> (Auto i) b
~~~

Okay, what would this even mean?

`Auto i` is something that takes an `i` as an input.  `Auto i a` is something
that outputs an `a`.  So if `Auto i` is a functor...it means that I can turn
`Auto i a` into `Auto i b` with a function `a -> b`.  I can turn an Auto
taking an `i` and outputting an `b` into an Auto taking an `i` and outputting
a `b`.  I "map over" the *output*.

So `Auto i` is a functor where you can `map` the output.  If I was going to
output a `5`, if I `fmap (+1)`, I'd actually output a `6`.

~~~haskell
instance Functor (Auto i) where
    fmap f a =  ACons $ \x ->
                  let (y  , a') = runAuto a x
                  in  (f y, fmap f a')
~~~

~~~haskell
λ: testAuto_ settableAuto
  |  [Nothing,Nothing,Just (-3),Nothing,Nothing]
[1,2,-3,-2,-1]

λ: testAuto_ (fmap (*2) settableAuto)
  |  [Nothing,Nothing,Just (-3),Nothing,Nothing]
[2,4,-6,-4,-2]
~~~

Functor...check!  What next?

### Applicative

Everyone knows that the "cool", *hip* typeclasses are the classic trio,
[Functor, Applicative, Monad][fam].  Let's just move on right along and go for
Applicative.

[fam]: http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html

What is an Applicative functor, anyway?  It really has two things: the ability
to apply functions "inside" containers to values "inside" containers, and the
ability to wrap a value in a default context/container (all following the laws
of course).  The second part is going to be more immediately useful to
us.[^pointed]

[^pointed]: Actually this "wrapping" property really was kind of jammed into
Applicative, it technically belongs [Pointed][] typeclass, and Applicative is
technically only supposed to mean the function wrapping ability, under some
interpretations.  But splitting up typeclasses to such a fine degree isn't
quite practical.

[Pointed]: http://hackage.haskell.org/package/pointed-4.0/docs/Data-Pointed.html

~~~haskell
class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
~~~

Again, we see that it doesn't make too much sense for `Auto` to be an
Applicative...but `Auto i` definitely can be.

~~~haskell
pure :: a -> (Auto i) a
~~~

Can you think of a function that has this type signature?

As it turns out, due to [parametricity][], there is actually only exactly one
meaningful function that has this type signature.

[parametricity]: http://en.wikipedia.org/wiki/Parametricity

What must that `Auto i a` be?  Well, it clearly must output an `a`.  Can it
possibly incorporate `i` in any way?  It can't!  Because it can't really
"make" any `a`s besides the one given to it in `pure k`.  So that `pure k ::
Auto i a` must be an Auto that ignores its input `i` and always returns `k`
every time.

It must be the "constant" arrow.

~~~haskell
instance Applicative (Auto i) where
    pure k = ACons $ \_ -> (k, pure k)
    (<*>)  = undefined      -- for now
~~~

~~~haskell
λ: testAuto_ (pure 5) [1..10]
[5,5,5,5,5,5,5,5,5,5]

λ: testAuto_ (summer . pure 5) [1..10]
[5,10,15,20,25,30,35,40,45,50]

λ: testAuto_ (pure 5 . summer) [1..10]
[5,5,5,5,5,5,5,5,5,5]
~~~

As it turns out, `pure k` is the same as the `constant k` that we defined
earlier.  Now we just have a more semantically meaningful way of constructing
it instead of using `autoize`

I'll leave the implementation of `(<*>)` as an exercise, partially because
it's not too surprising and would be a fun thing to work out on your own, and
partially because `pure` is more useful for the time being.

### Monad

So a Monad, conceptually, is just a functor we can "squish".  Basically, we
need the function:

~~~haskell
join :: (Auto i) ((Auto i) a) -> (Auto i) a
~~~~

Basically, we must turn an Auto returning an Auto into just an Auto returning
a value.

We must turn a morphism returning a morphism and turn it into a morphism
returning a value.

Typically, this is done by feeding your `i` into the outside Auto to get a
returned Auto, and feeding that *same* `i` into the returned Auto, and getting
the result of that.

This actually gives you a *Reader-like* monad behavior, for Auto --- the
ability to chain together multiple Auto's all together with a "common"
input/environment.

However, even though this is interesting, we actually won't be using the Monad
instance of `Auto` all too much for now either.  I'll again leave this as an
exercise.  The solutions for all of these exercises are available in the
sample code for the article.

Arrow
-----

Now, one final typeclass: Arrow.

As it turns out, `Category` by itself is nice, but for the games we will
eventually be playing with function composition, it doesn't offer too much in
terms of combinators.

There is a neat Haskell extension that provides syntactic sugar for complex,
multi-way, side-chained compositions, called "proc notation". Proc notation
will prove invaluable to us eventually, but it requires some more Category
combinators to work.

As it turns out, the `Arrow` typeclass exists as a general grab-bag of
combinators to make life a lot easier for us.

~~~haskell
class Category r => Arrow r where
    arr    :: (a -> b) -> r a b
    first  :: r a b -> r (a,c) (b,c)
    second :: r a b -> r (c,a) (c,b)
    (***)  :: r a b -> r c d -> r (a,c) (b,d)
    (&&&)  :: r a b -> r a c -> r a (b,c)
~~~

### instance Arrow

Let's write an Arrow instance for Auto.

`arr` takes a normal function and turns it into a pure Auto --- we wrote this
before, it's just `functionToAuto`

`first` takes an Auto and turns it into an Auto that only operates on the
first part of a tuple.  `second` is the same, but for the second part.

`(***)` takes two Autos and makes an Auto that applies them "in parallel" to
two parts of a tuple.  `(&&&)` takes two Autos that both take the same type,
and makes an Auto that applies both Autos to the same value "in parallel" and
returns the results as a tuple.

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

Don't be too mystified by the Arrow typeclass.  Really, Arrows are just
Categories for which we have defined ways to chain compositions side-by-side.
Things like `(***)` and `(&&&)` are pretty useful if we want to be able to
compose multiple functions in fork-like ways, or to compose two functions
side-by-side with another function.  We will end up doing this a lot when we
work with AFRP, so this is pretty handy to have.

As it turns out, there are actually a lot of specialized Arrow typeclasses,
too, which are the same sort of "grab bag" of combinators, except for
different purposes.

The more useful ones for our purpose will be ArrowChoice and ArrowLoop, and
you can read into these.  It actually isn't all too important right now to
understand how these are implemented --- just know that ArrowChoice gives you
combinators for forking compositions and ArrowLoop gives you combinators for
*recursive* compositions.

### Proc Notation

The *main* purpose for Arrow (in our situation) is that now that we have
instanced Arrow for our Autos, we can now use it in proc notation.

This is similar to how once we instance something as a Monad, we can use it in
"do" notation.

Proc notation is basically "do notation for Arrows", and is just syntactical
sugar for the various Arrow combinators we described before.

Proc notation consists of lines of cute little ASCII "arrows":

~~~haskell
output <- arrow -< input
~~~

where `arrow` is the Arrow, `input` is the "input" fed into the Arrow, and
"output" binds the result to the name "output".  We can omit "output" and
we will "forget" the output.

Cute, right?

For example, to write our `succ . double` composition we wrote earlier:

~~~haskell
doubleSucc1 :: Auto Int Int
doubleSucc1 = succ . double

doubleSucc2 :: Auto Int Int
doubleSucc2 = proc n -> do
  doubled <- double -< n
  succ -< doubled
~~~

What if we wanted the arrow to return a tuple with the result of doubling, and
also the result of doubling then succing??

~~~haskell
doubleSucc'1 :: Auto Int (Int,Int)
doubleSucc'1 = (id *** succ) . (double &&& id)

doubleSucc'2 :: Auto Int (Int,Int)
doubleSucc'2 = proc n -> do
  doubled      <- double -< n
  doubleSucced <- succ   -< doubled
  returnA -< (doubled, doubleSucced)
~~~

`returnA` is just `id` (for Category Auto), the identity Arrow.  But we call
it `returnA` to draw an analogy between `return` for Monads.  They both serve
the same purpose --- they take normal values and turn them into something you
can use in a do/proc block.

So basically, every line in a proc block must look like:

~~~haskell
arrow -< input
~~~

Or, if you want to name the result for later use,

~~~haskell
output <- arrow -< input
~~~

Just like how "do" blocks compose several monad values into one giant monad
value, "proc" blocks compose several morphisms/arrows into one giant arrow.

Remember that proc blocks don't actually "do" anything.  You aren't sequencing
actions.  You basically are creating a *dependency* graph --- saying which
arrows depend on the output of which arrows, and how they all twist and
combine together.

In `doubleSucc'2`, we are saying this:

"If you want to run this arrow, and you give us a value `n`, then the result
is a tuple `(doubled, doubleSucced)`, where `doubled` is the result of running
that `n` through `double`, and `doubleSucced` is the result of running
`doubled` through `succ`."

So when we eventually use proc notation with our Auto, it describes one giant
"tick" of the big function.

#### proc rec

Finally, we often will need to have Autos that "depend" on eachother in a
cyclic way.  For example, in a harmonic oscillator system, the position
depends on the force applied to the object, but the force applied depends on
the position.

These "recursive" bindings come from combinators provided by the ArrowLoop
typeclass:

~~~haskell
object :: Auto () Int
object = proc _ -> do
  rec
    let acc = -1 * pos
    vel <- summer -< acc
    pos <- summer -< vel
  returnA -< pos
~~~

So we can have `pos` depend on `acc`, and `acc` depend on `pos`.

Hopefully our Autos im
