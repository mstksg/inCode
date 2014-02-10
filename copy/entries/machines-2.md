Intro to Machines & Arrows: Category and Arrow Typeclasses (Part 2)
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

!!![simpleauto]:machines/Auto.hs "summer:"

Autos are "function-like things"...they map or "morph" things of type `a` to
things of type `b` in some form, just like functions.  As it turns out, this
design pattern has a name.  They're instances of the type class "Category".

Categories
----------

You might have heard of the word "category" applied to Haskell in a few
different contexts.  Don't worry, the Category typeclass isn't scary!  It's
rather simple, actually!  Much simpler than a certain other infamous typeclass
(that rhymes with Shmonad).

The Functor typeclass represents the idea of mapability.  `Maybe`, for
example, is a functor.  `Maybe a`, a concrete type for all `a`, is a concrete
object that "contains" an `a`.

As we will see, `Auto` is a Category.  `Auto a b` is called, for all `a` and
`b`, a *morphism* (or an "arrow", but that is a loaded term in Haskell) in
that category from things of type `a` to things of type `b`.

The Category typeclass represents the very essense of "function-like"-ness.
As it turns out, it includes one special member and one function over
members.

~~~haskell
class Category cat where
    id  :: cat a a
    (.) :: cat b c -> cat a b -> cat a c
~~~

Basically, something is a Category if it has:

1.  A special member (remember, members of Category are called "morphisms" or
    "arrows") that is an "identity function-like"
2.  An operator that takes two arrows and "composes"/sequences them into one
    big one.

These come with three laws:

1.  "Left identity": `id . f == f`
2.  "Right identity": `f . id == f`
3.  "Associativity": `f . (g . h) == (f . g) . h`

<aside>
    ###### Aside

Remember that `id` represents a *concrete member* of the Category, and that
`(.)` represents a *function between members*.

This is similar in pattern to the classic Monoid typeclass:

~~~haskell
class Monoid a where
    mempty :: a
    (<>) :: a -> a -> a
~~~

Where `mempty` describes one specific member, and `(<>)` represents a
binary function between two members.

It's just that for Monoid, our members are concrete "things", but for
Category, our members are morphisms/functions!

Also, remember the Monoid laws?

1. "Left identity": `mempty <> x == x`
2. "Right identity": `x <> mempty == x`
3. "Associativity": `x <> (y <> z) == (x <> y) <> z`

Looks awfully similar!  But can you see why a given Category `cat a b` and
Monoid `a` are not technically the same?
</aside>

But enough with silly maths, let's look at a concrete example.

Basically, this typeclass says that the essense of function-like-ness is the
existence of an identity function-like, and the ability to compose and
sequence functions.

### The `(->)` Category

If categories embody function-like things, then obviously we would expect a
normal function `(->)` to be a Category.

The first item in the Category typeclass is `id`, a member that's an "arrow"
represents an identity function: a function that returns, unchanged, its
input.

~~~haskell
funcId :: (->) a a
funcId = \x -> x
~~~

~~~haskell
λ: funcId 1
1
λ: funcId "hello"
"hello"
~~~

The second item in the Category typeclass is `(.)`, a function between arrows
that composes and sequences.  It takes `g :: a -> b` and `f :: b -> c` (in
reverse order) and creates a new function `a -> c` that first applies `f` and
then applies `g`:

~~~haskell
funcComp :: (->) b c -> (->) a b -> (->) a c
funcComp f g = \x -> f (g x)
~~~

~~~haskell
λ: ((+1) `funcComp` (*2)) 5
11
λ: (show `funcComp` not) True
"False"
~~~

Checking that these satisfy the category laws is a nice exercise, and involves
just substituting the definitions for eachother.

With this in mind, let's write a Category instance for `(->)`:

~~~haskell
instance Category (->) where
    id = \x -> x
    f . g = \x -> f (g x)
~~~

Et voilà!

### The `Auto` Category

Now let's create a Category instance for our Autos.

First, the identity arrow.  It is supposed to take an `a` and return an
identical `a` unchanged.

~~~haskell
autoId :: Auto a a
autoId = ACons $ \x -> (x, autoId)
~~~

~~~haskell
λ: testAuto_ autoId [1..10]
[1,2,3,4,5,6,7,8,9,10]
λ: testAuto_ autoId "hello world"
"hello world"
λ: testAuto_ autoId [True]
[True]
~~~

Perfect!  We created an Auto that does nothing and has no internal state ---
it only "dumb"ly returns the input as-is; the "head" is the same as the input
and the "tail" is just the same identity Auto.

Composition is a bit tricker.  Let's think a bit about what it would mean.

If we sequence `g :: Auto a b` and `f :: Auto b c`...we can think of it as
creating a big Auto with two states that keep on ticking.  `g` takes an `a`,
advances its state appropriately, and gives the resulting `b` to `f`.  `f`
takes that `b`, advances its own state, and pops out a `c` overall.  The
result is an `a` turning into a `c`, and both `g` and `f` updating their
state.

Let's take a deep breath and jump right into it.

~~~haskell
autoComp :: Auto b c -> Auto a b -> Auto a c
f `autoComp` g = ACons $ \x ->
  let (y, f') = runAuto f x
      (z, g') = runAuto g y
  in  (z, f' `autoComp` g')
~~~

To test this, let's make a simple Auto combinator `functionToAuto`, that takes
a pure function and turns it into a "pure Auto" --- that is, an Auto that
behaves exactly like that function and does not have any internal state.

~~~haskell
functionToAuto :: (a -> b) -> Auto a b
functionToAuto f = ACons $ \x -> (f x, functionToAuto f)
~~~

And let's test some compositions of our pure Autos:

~~~haskell
λ: let doubleAuto = functionToAuto (*2)
λ: let succAuto = functionToAuto (+1)
λ: let constAuto x = functionToAuto (const x)

λ: testAuto_ (succAuto `autoComp` doubleAuto) [1..10]
[3,5,7,9,11,13,15,17,19,21]

λ: testAuto_ (doubleAuto `autoComp` autoId) [1..10]
[2,4,6,8,10,12,14,16,18,20]

λ: testAuto_ doubleAuto [1..10]         -- f . id == f
[2,4,6,8,10,12,14,16,18,20]

λ: testAuto_ (succAuto `autoComp` (constAuto 20)) [1,2,undefined]
[21,21,21]
~~~

And how about we compose with some of our "impure" Autos from before?

~~~haskell
λ: testAuto_ summer [5,1,9,2,-3,4]
[5,6,15,17,14,18]

λ: testAuto_ doubleAuto [5,6,15,17,14,18]
[10,12,30,34,28,36]

λ: testAuto_ (doubleAuto `autoComp` summer) [5,1,9,2,-3,4]
[10,12,30,34,28,36]

λ: testAuto_ settableAuto [Nothing,Nothing,Just (-3),Nothing,Nothing]
[1,2,-3,-2,-1]

λ: testAuto_ summer [1,2,-3,-2,-1]
[1,3,0,-2,-3]

λ: testAuto (summer `autoComp` settableAuto)
  |    [Nothing,Nothing,Just (-3),Nothing,Nothing]
[1,3,0,-2,-3]
~~~

And now we can write a Category instance for Auto:

~~~haskell
instance Category Auto where
    id    = ACons $ \x -> (x, id)
    f . g = ACons $ \x ->
              let (y, f') = runAuto f x
                  (z, g') = runAuto g y
              in  (z, f' . g')
~~~

Now we can use Autos just like we use functions!

~~~haskell
λ: ((+1) . (*2)) 2
5

λ: testAuto_ (succAuto . doubleAuto) [2]
[5]

λ: ((+1) . id) 2
3

λ: testAuto_ (succAuto . id) [2]
[3]

λ: testAuto_ (summer . settableAuto . id)
  |  [Nothing,Nothing,Just (-3),Nothing,Nothing]
[1,3,0,-2,-3]
~~~

It's...a bit trippy at first, getting used to the fact that `id` and `(.)` are
now "overloaded" in the same sense that `(>>=)` and `return` or `mempty` and
`(<>)` are, but the more we work with multiple Category instances, the more we
get used to it :)

Of course remember that you have to explicily hide the `(.)` and `id` that
come in Prelude:

~~~haskell
import Prelude hiding ((.), id)
~~~

The Power of Typeclasses
------------------------

Let's step back for a second and see what we just did.  This is the true power
of Haskell typeclasses in action.  We just took two types that had completely
different implementations and representations...and now are able to talk about
both of them **using the same language**.

This is actaully a pretty amazing thing and is a real triumph of the typeclass
approach to abstraction.

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

You might have first learned that you can map over lists.  Then one day, you
realize that you can map over a *lot* of things...and now lists are longer any
"special" mappable object.  They are just one mappable thing out of many.

This is what we just did here.

You might have one day learned that you could compose functions `(.)` and have
identity functions `id`.  Then, one day (maybe that day is today!) you realize
that you can also compose and have identity arrows over...lots of things!  So
many things!  We haven't even scratched the surface of the wide variety of
useful `Category` instances!

Now, plain ol' `(->)` functions are no longer any "special" function things.
They are just one function-like thing out of many, many.

And now we can reason with *all* of them, as a whole, using `(.)` and `id`.
We can `(.)` and have `id` for many things.

If you're interested in this, you might want to look into the Kleisli
category, whose arrows are of type `Kleisli a b`, and which represent
functions `a -> m b` for monad `m`.  Using `Kleisli`, we can also use `(.)`
and `id` to reason with monads, as well as functions and Autos.

More Typeclasses!
-----------------

Now that we know how cool typeclasses are, let's take some time to try to
instance our Autos into the "cool" typeclasses to be in: [Functor,
Applicative, and Monad][fam].

[fam]: http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html

### Functor

What is a functor?  It represents something that can be mapped over.  More
pedantically, it is something that implements `fmap` and follows certain laws.

~~~haskell
fmap :: Functor f => (a -> b) -> f a -> f b
~~~

It doesn't really make sense for `Auto` to be a functor, because you are
mapping over both the input and the output?  What?

You can see this absurdify by trying to substitute `f ~ Auto` in the type
signature for `fmap`:

~~~haskell
fmap :: (a -> b) -> Auto a -> Auto b
~~~

`Auto a` isn't even a concrete type...so this doesn't really make too much
sense!

But it does make sense for `Auto i` to be a functor!

~~~haskell
fmap :: (a -> b) -> (Auto i) a -> (Auto i) b
~~~

Okay, what would this even mean?

`Auto i` is something that takes an `i` as an input.  `Auto i a` is something
that outputs an `a`.  So if `Auto i` is a functor...it means that I can turn
`Auto i a` into `Auto i b` with a function `a -> b`.  I "map over" the
"output".

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

### Applicative

What is an Applicative functor?  It really is two things: the ability to apply
functions "inside" containers to values "inside" containers, and the ability
to wrap a value in a default context/container (all following the laws of
course).  The second part is going to be more useful to us.[^pointed]

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
    (<*>)  = undefined
~~~

~~~haskell
λ: testAuto_ (pure 5) [1..10]
[5,5,5,5,5,5,5,5,5,5]

λ: testAuto_ (summer . pure 5) [1..10]
[5,10,15,20,25,30,35,40,45,50]

λ: testAuto_ (pure 5 . summer) [1..10]
[5,5,5,5,5,5,5,5,5,5]
~~~

As it turns out, `pure k` is the same as the `constAuto k` that we defined
earlier.  Now we just have a more semantically meaningful way of constructing
it instead of using `functonToAuto`

I'll leave the implementation of `(<*>)` as an exercise, partially because
it's not too surprising, and mostly because `pure` is more interesting for the
time being.

Arrow
-----

As it turns out, `Category` by itself is nice, but for the games we will
eventually be playing with function composition, it doesn't offer too much in
terms of combinators.

There is a well-established Haskell extension that provides syntactic sugar
for complex, multi-way, side-chained compositions, called "proc notation".
Proc notation will prove invaluable to us eventually, but it requires some
Category combinators to work.

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

The relevant ones we will be using later are:

