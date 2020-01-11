---
title: "Adjunctions in the wild: foldl"
categories: Haskell, Math
tags: haskell, category theory
create-time: 2020/01/08 04:10:21
date: never
series: Advent of Code
identifier: advent-shuffle
slug: foldl-adjunction
---

I recently made a few connections that linked some different concepts in
Haskell that I hadn't realized before.  They deal with one of my favorite
"practical" libraries in Haskell, and also one of the more "profound" category
theory-inspired abstractions in Haskell.

This post mainly goes through my thought process in finding this out --- it's
very much a "how I think through this" sort of thing --- in the end, the goal
is to show how much this example made me further appreciate the conceptual idea
of adjunctions and how they can pop up in interesting places in practical
libraries.  Unlike most of my other posts, it's not about necessarily about how
practically useful an abstraction is, but rather what insight it gives us to
understanding its instances.

The audience of this post is Haskellers with an understanding/appreciation of
abstractions like `Applicative`, but be aware that the final section is
separately considered as a fun aside for those familiar with some of Haskell's
more esoteric types.  The source code is [available on github][source].

!!![source]:adjunctions/foldl.hs

foldl
-----

The first concept is the great *[foldl][]* library, which provides a nice
"stream processor" type called `Fold`, where `Fold r a` is a stream processor
that takes a stream of `r`s and produces an `a`:

[foldl]: http://hackage.haskell.org/package/foldl

```haskell
import           Control.Foldl   (Fold(..))
import qualified Control.Foldl   as F

F.sum  :: Num a        => Fold a a
F.mean :: Fractional a => Fold a a
F.elem :: Eq a         => a -> Fold a Bool

fold sum  [1,2,3,4]
#   => 10
fold mean [1,2,3,4]
#   => 2.5
fold (elem 3) [1,2,3,4]
#   => True
fold (elem 5) [1,2,3,4]
#   => False
```

The most useful thing about the library is that it treats the folds as
first-class objects, so you can create more complex folds by combining simpler
folds (for example, with `-XApplicativeDo`)

```haskell
!!!adjunctions/foldl.hs "variance ::" "varianceTooBig ::"
```

Most importantly, `Fold r` is an instance of both `Functor` and `Applicative`,
so you can map over and combine the results of different folds.

To me, *foldl* is one of the shining examples of how well Haskell works for
data and stream processing, and a library I often show to people when they ask
what the big deal is about Haskell abstractions like `Applicative`, purity, and
lists.

Adjunctions
-----------

The second concept is the idea of *[adjoint functors][]* (see also [Bartosz
Milewski's introduction][bartosz] and [nlab][]'s description), represented in
Haskell by the *[adjunctions][]* library and typeclass ([Chris Penner][] has a
nice article with an example of using the typeclass).

[adjoint functors]: https://en.wikipedia.org/wiki/Adjoint_functors
[Chris Penner]: https://chrispenner.ca/posts/adjunction-battleship
[bartosz]: https://bartoszmilewski.com/2016/04/18/adjunctions/
[nlab]: https://ncatlab.org/nlab/show/adjoint+functor
[adjunctions]: https://hackage.haskell.org/package/adjunctions/docs/Data-Functor-Adjunction.html

The idea is that, for some functors, we can think of a "conceptual inverse".
We can ask "I have a nice functor `F`.   Conceptually, what functor represents
the opposite idea/spirit of `F`?"  The concept of an adjunction is one way to
formalize what this means.  The high-level idea is that if `F -| G` (`F` is
left adjoint to `G`, and `G` is right adjoint to `F`), then all the ways of
going "out of" `F a` to `b` are the same as all the ways of going "into" `G b`
from `a`.  Ways of going out can be encoded as ways of going in, and vice
versa.  They represent opposite ideas.

### Examples

For example, one of the more famous adjunctions in Haskell is the adjunction
between `(,) r` and `(->) r`.  "Tupling" represents some sort of "opposite"
idea to "parameterizing".

The ways to get "out" of a tuple is `(r, a) -> b`.  The ways to go "into" a
function is `a -> (r -> b)`.  Haskellers will recognize that these two types
are the "same" (isomorphic) --- any `(a, b) -> c` can be re-written as `a -> (b -> c)`
(currying), and vice versa (uncurrying).

Another common pair is with same-typed either and tuple:

```haskell
newtype SameEither a = SE (Either a a)
newtype SameTuple  a = ST (a, a)
```

People familiar with `Either` (sums) and `(,)` (products) in Haskell will
recognize them as "opposite" ideas --- one is "or", and the other is "and"
(depending on if you are talking about using them or making them).

We can formalize this idea of opposites using adjunctions: Going "out of"
`Either a a` into `b` can be encoded as going "into" `(b, b)` from `a`,m and
vice versa: `Either a a -> b` can be encoded as `a -> (b, b)` (and vice versa)
--- the two types are isomorphic.  This is because to go out of `Either a a`,
you have to handle the situation of getting a `Left` and the situation of
getting a `Right`.  To go into `(b, b)`, you have to able to ask what goes in
the first field, and what goes in the right field.  Both `Either a a -> b` and
`a -> (b, b)` have to answer the same questions. (A fun exercise would be to
write the functions to convert between the two)

### Big Picture

Aside from being an interesting curiosity (formalizing the idea of "opposite
idea" is pretty neat), hunting for adjunctions can be useful in figuring out
"why" a functor is useful, what you can do with it, and also what functors are
intimately connected with it.  There's also the helper functions in the
[Data.Functor.Adjunction][adjunctions] module that implement some nice
helper functions on your types if an adjoint happens to exist --- you can do
some neat things by going "back and forth" between adjoint functors.

Hunting for Adjunctions
-----------------------

So, from the build-up, you've probably guessed what we're going to do next:
find a functor that is adjoint to `Fold r`.  If you guessed that ... you're
right!  What's the "conceptual opposite" of `Fold r`?  Let's go adjunction
hunting!

Important note --- the rest of this section is not a set of hard rules, but
rather an intuitive process of heuristics to search for candidates that would
be adjoint to a given functor of interest.  There are no hard and fast rules,
and the adjoint might not always exist --- it usually doesn't.  But when it
does, it can be a pleasant surprise.

### Patterns to look for

Now, on to the hunting.  Let's say we have functor `Q` and we want to identify
any adjoints.  We want to spot functions that use both `Q a` and `a` with some
other value, in [opposite positions][polarity].

[polarity]: https://www.foldl.io/posts/pos-neg-functions/

(Of course, this is only the case if we are using a functor that comes from a
library.  If we are writing our own functor from scratch, and want to hunt for
adjunctions there, we have to instead *think* of ways to use `Q a` and `a`)

One common pattern is functions for "converting between" the going-in and
going-out functions. In [Data.Functor.Adjunctions][adjunctions], these are
called `leftAdjunct` and `rightAdjunct`:

```haskell
leftAdjunct  :: Adjunction f u => (f a -> b) -> (a -> u b)
rightAdjunct :: Adjunction f u => (a -> u b) -> (f a -> b)
```

These will often come in pairs, and they are significant because they are
essentially the adjunctions "in practice":  Sure, an `(r, a) -> b` is useful,
but "using" the adjunction means that you can convert between `(r, a) -> b`
(`uncurry`) and backwards.

Basically, any time `Q a` is spotted with `a` with opposite polarity, it's
something to investigate.

Another common pattern that you can spot are "indexing" and "tabulating"
functions, in the case that you have a right-adjoint:

```haskell
indexAdjunction    :: Adjunction f u => u b -> f () -> b
tabulateAdjunction :: Adjunction f u => (f () -> b) -> u b
```

These also come in pairs!  And they are equivalent in power --- you can
implement `rightAdjunct` in terms of `indexAdjunction` and `leftAdjunct` in
terms of `tabulateAdjunction` and vice versa.

`indexAdjunction` means: if it's possible to "extract" from `u b` to `b` using
only an `f ()` as extra information, then `u` might be right-adjoint to `f`.

`tabulateAdjunction` means: if it's possible to "generate" a `u b` based on a
function that "builds" a `b` from `f ()`, then `u` might right-adjoint to `f`.

### Adjoints to `Fold`

Now, let's look out for examples of these functions for `Fold`!  In the case of
`Fold`, there is actually only one function I can find that directly takes a
`Fold r a` and returns an `a`:

```haskell
fold :: Fold r b -> [r] -> b
````

(the type has been simplified and re-labeled, for illustration's sake)

You "give" a `Fold r b` and "get" an `b` (and so they have opposite
polarities/positions).  This sort of function would make `Fold r` a *right
adjoint*, since the naked type `b` (the final parameter of `Fold r b`) is the
final result, not the input.

Of our common patterns, this one looks a looooot like
`indexAdjunction`.[^index]

[^index]: As it so happens, `fold` is actually exactly `index` for
`Representable (Fold r)`, from *[Data.Functor.Rep][Rep]*.  Here we are
utilizing the fact that a representable functor gives rise to a left-adjoint
for free.

[Rep]: https://hackage.haskell.org/package/adjunctions/docs/Data-Functor-Rep.html

```haskell
fold            :: Fold r b -> [r]  -> b
indexAdjunction :: Fold r b -> f () -> b
```

This means that `Fold r b` is right-adjoint to some functor `f` where `f () =
[r]`.  A good first guess (just a hunch?) would be to just have `f a = ([r],
a)`:

```haskell
!!!adjunctions/foldl.hs "data EnvList"
```

`EnvList r` is essentially just a *list* of `r`s.  It is now also our suspect
for a potential left-adjoint to `Fold r`: a "conceptual opposite".

```haskell
!!!adjunctions/foldl.hs "indexFold ::"
```

To seal the deal, let's find its pair, `tabulateAdjunction`.  That means we are
looking for:

```haskell
tabulateFold :: (EnvList r () -> b) -> Fold r b
```

Or, to simplify the type by expanding the definition of `EnvList r ()`:

```haskell
tabulateFold :: ([r] -> b) -> Fold r b
```

This tells us that, given any list processor `[r] -> b`, we can write a fold
`Fold r b` representing that list processor.  Scanning things more, we can see
that this actually looks a lot like `foldMap` from the library:

```haskell
import qualified Control.Foldl as F

F.foldMap
    :: Monoid w
    => (r -> w)
    -> (w -> b)
    -> Fold r b

-- or

F.foldMap (\r -> [r])
    :: ([r] -> b)
    -> Fold r b
```

So:

```haskell
!!!adjunctions/foldl.hs "tabulateFold ::"
```

And...that gives us a pretty strong footing to claim that `EnvList r` is
the left-adjoint of `Fold r`.

Note that if we had missed `fold` during our adjunction hunt, we might have
also lucked out by noticing `F.foldMap (\r -> [r])` fitting the criteria for a
candidate for `tabulateAdjunction`, instead.

Opposite Concepts
-----------------

We've identified a likely candidate for a left-adjoint to `Fold r`!  But ...
does any of this make any sense?  Does this make sense as a left-adjoint,
conceptually ... and did we gain anything?

Let's think about this from the beginning: What is the conceptual opposite of
"something that folds a list"?

Well, what other thing is more naturally an opposite than "a list to be
folded"!

*   `EnvList r`: Tuples a list of `r`
*   `Fold r`: Parameterizes on a list of `r`

Or, in terms of the result of the functor application:

*   `EnvList r a`
    *   A list of `r`
    *   ... tuplied with an `a`

*   `Fold r a`
    *   Consumes a list of `r`
    *   ... to give a parameterized `a`

It seems to "flip" the idea of "list vs. list consumer", and *also* the idea of
"tupled vs. parameterizing" (which was our first example of an adjunction
earlier, as well).

In addition, lists seem to be at the heart of how to create and consume a `Fold
r`.

`fold` can be thought of as the fundamental way to *consuming* a `Fold r`. This
makes the adjunction against `EnvList r` make sense: what good is the *ability*
to fold ... if there is nothing *to fold*?  `EnvList r` (a list of `[r]`) is
intimately related to `Fold r`: they are the yin and yang, peanut butter and
jelly, night and day.  Their fates are intertwined from their very inception.
You cannot have one without the other.

In addition, `F.foldMap` is arguably a fundamental (although inefficient) way
to *specify* a `Fold r`.  A `Fold r` is, fundamentally, a list processor ---
which is what `EnvList r a -> b` literally is (an `[r] -> b`).  `Fold r` and
`EnvList r` --- [dyads in the force][dyad].  Or, well...I guess literally
monads, since [all adjunctions give rise to monads][monads]...and comonads too.

[dyad]: https://starwars.fandom.com/wiki/Dyad_in_the_Force
[monads]: http://www.stephendiehl.com/posts/adjunctions.html

The fact that `EnvList r` and `Fold r` form an adjunction together formalizes
the fact that they are conceptually "opposite" concepts, and also that they are
bound together by destiny in a close and fundamental way.

The Helper Functions
--------------------

Let's take a look at some of the useful helper functions that an instance of
`Adjunction` gives us for `Fold r`, to see how their existence can better help
us understand `Fold`.  For all of these, I'm going to write them first as
`EnvList r a`, and then also as `([r], a)`, to help make things clearer.

```haskell
unit :: a -> Fold r (EnvList r a)
unit :: a -> Fold r ([r], a)

counit :: EnvList r (Fold r a) -> a
counit :: [r] -> Fold r a -> a

leftAdjunct :: (EnvList r a -> b) -> (a -> Fold r b)
leftAdjunct :: ([r] -> a -> b   ) -> (a -> Fold r b)

rightAdjunct :: (a -> Fold r b) -> (EnvList r a -> b)
rightAdjunct :: (a -> Fold r b) -> ([r] -> a -> b   )

tabulateAdjunction :: (EnvList r () -> b) -> Fold r b
tabulateAdjunction :: ([r] -> b)          -> Fold r b

indexAdjunction :: Fold r b -> EnvList r a -> b
indexAdjunction :: Fold r b -> [r]         -> b

zipR :: Fold r a -> Fold r b -> Fold r (a, b)
```


1.  `unit :: a -> Fold r ([r], a)`, when we specialize `a ~ ()`, becomes:

    ```haskell
    unit :: Fold r [r]
    ```

    This means that `unit` for `Fold r` folds a list `[r]` into "itself":

    ```haskell
    fold unit [1,2,3]
    #   => [1,2,3]
    ```

2.  `counit :: [r] -> Fold r a -> a` is essentially just `fold`.  Neat!

3.  `leftAdjunct :: ([r] -> a -> b) -> (a -> Fold r b)` ... if we write it as
    `leftAdjunct :: a -> (a -> [r] -> b) -> Fold r b`, and feed the `a` into
    the first function, we get:

    ```haskell
    leftAdjunct' :: ([r] -> b) -> Fold r b
    ```

    which is just `tabulateAdjunction`, or `F.foldMap (\r -> [r])`!  It encodes our
    list processor `[r] -> b` into a `Fold r b.`

4.  `rightAdjunct :: (a -> Fold r b) -> ([r] -> a -> b)` -- if we again rewrite
    as `rightAdjunct :: a -> (a -> Fold r b) -> [r] -> b`, and again feed the
    `a` into the first function, becomes:

    ```haskell
    rightAdjunct' :: Fold r b -> [r] -> b
    ```

    Which is just `fold`, or `counit`!

    Note that `leftAdjunct` and `rightAdjunct` aren't always this cleanly
    rearranged into `tabulate` or `counit` etc. -- in this case it's just
    because of how `EnvList r a` is shaped.

5.  `tabulateAdjunction` and `indexAdjunction` we went over earlier, seeing
    them as `F.foldMap (\r -> [r])` and `fold`

6.  `zipR :: Fold r a -> Fold r b -> Fold r (a, b)` takes two `Fold r`s and
    combines them into a single fold.  This is exactly the "combining fold"
    behavior that makes `Fold`s so useful!  The implementation of
    `zipR` is less efficient than the implementation of `<*>`/`liftA2` for
    `Fold r`, but knowing that `zipR` exists means that we know `Fold r`s can
    be combined.

Seeing how these functions all fit together, we can write a full instance of
`Adjunction`.  We can choose to provide `unit` and `counit`, or `leftAdjunct`
and `rightAdjunct`[^default]; the `unit`/`counit` definitions are the easiest
to conceptualize, for me, but the other pair isn't much tricker to write.

[^default]: We should also be able to define it using `tabulateAdjunction` and
`indexAdjunction`, I believe ... but this isn't allowed for some reason?

```haskell
!!!adjunctions/foldl.hs "instance Adjunction (EnvList r)"
```

Conclusion
----------

Admittedly, the selection of helper functions that `Adjunction` gives us pales
in comparison to abstractions like `Monoid`, `Applicative`, `Traversable`,
`Monad`, etc., which makes `Adjunction` (in my opinion) nowhere as practical
when compared to them.

However, to me (and, according to how I've seen other people use it),
`Adjunction` is most useful as a conceptual tool in Haskell.  The idea of
"opposites" or "duals" show up a lot in Haskell, starting from
the most basic level -- sums and products, products and functions.  From day 1,
Haskellers are introduced to natural pairs and opposites in concepts.
The idea of opposites and how they interact with each other is always on the
mind of a Haskeller, and close to their heart.

So, what makes `Adjunction` so useful to me is that it actually is able to
formalize what we mean by "opposite concepts".  The process of identifying
a functor's "opposite concept" (if it exists) will only help is better
understand the functor we're thinking about, in terms of how it works and how
it is used.


The Algebraic Way
-----------------

This article is done!  Our first guess for an adjunction seems to be morally
correct.  But as an aside ... let's see if we can take this idea further.  The
rest of this post is not really important to the point I was trying to make!

You never *need* all this fancy math to be able to write Haskell ... but many
feel like it can make things a lot more fun! :)

In this section we're going to get a bit mathy and look at the definition of
`Fold`, to see if we can *algebraically* find an adjunction of `Fold`, instead
of just trying to hunt for API functions like before.  In practice you don't
often have to make algebraic deductions like this, but it's at least nice to
know that something like this possible from a purely algebraic and logical
sense.

Be warned that this method *does* require some familiarity (or at least
awareness) of certain types that appear often in the more ... esoteric corners
of Haskelldom :)

The game plan here is to start with the definition of `Fold`, and then
rearrange it using algebraic substitutions until it matches something that
already has an `Adjunction` instance in the *adjunctions* library.

First, the actual definition of `Fold` in the *foldl* library itself is:

```haskell
data Fold r a = forall x. Fold (x -> r -> x) x (x -> a)
```

Maybe not the friendliest definition at first!  But something in this looks a
little familiar, maybe.  Let's do some re-arranging:

```haskell
data Fold r a = forall x. Fold (x -> r -> x) x (x -> a)
              = forall x. Fold x (x -> r -> x) (x -> a)
              = forall x. Fold x (x -> (r -> x, a))
```

Ah, this looks a *lot* like the constructor `Nu` for some `f`:

```haskell
data Nu f = forall x. Nu (x -> f x) x
```

`Nu` is one of the three main famous [fixed-point type combinators][foldable]
in Haskell.  The other two are `Mu` and `Fix`:

[foldable]: https://hackage.haskell.org/package/recursion-schemes/docs/Data-Functor-Foldable.html

```haskell
data    Nu  f = forall x. Nu (x -> f x) x
newtype Fix f = Fix (f (Fix f))
newtype Mu  f = Mu (forall x. (f x -> x) -> x)
```

In Haskell these are all equivalent[^munu], but they have very different
performance profiles for certain operations.  `Nu` is easy to "build up", and
`Mu` is easy to "tear down" -- and they exist sort of opposite to each other.
`Fix` exists in opposite to ... itself.  Sorry, `Fix`.

[^munu]: They are only equivalent in Haskell because of laziness --- in strict
languages, they are different.

Anyway, looking at `Fold r a`:

```haskell
data Fold r a = forall x. Fold x (x -> (r -> x, a))
data Nu   f   = forall x. Nu   x (x -> f x)
```

it seems like we can pick an `F` such that `Nu (F r a) = Fold r a`.  Let's
try...

```haskell
newtype (f :.: g) x = Comp (f (g x))

type Fold r a = Nu (((,) a) :.: ((->) r))
```

From here, some might recognize the fixed point of `(,) a :.: f` as
`Cofree`, from [Control.Comonad.Cofree][cofree] --- one of the more commonly
used fixed points.

[cofree]: https://hackage.haskell.org/package/free/docs/Control-Comonad-Cofree.html

```haskell
type Cofree f a = Nu ((,) a :.: f     )

type Fold r a   = Nu ((,) a :.: (->) r)
type Fold r     = Cofree ((->) r)
```

It looks like `Fold r` is just `Cofree ((->) r)` [^moore] ... and now we've hit
the jackpot!  That's because `Cofree f` has an instance of `Adjunction`!

[^moore]: Some might recognize `Cofree ((->) r)` as a common way of
implementing a [Moore machine][] in Haskell.  In fact, our derivation here is
basically a backwards version of [the process described here][mooreless].

[Moore machine]: https://en.wikipedia.org/wiki/Moore_machine
[mooreless]: https://www.schoolofhaskell.com/user/edwardk/moore/for-less


```haskell
instance Adjunction f u => Adjunction (Free f) (Cofree u)
```

This means that `Cofree u` is right-adjoint to `Free f`, if `f` is
right-adjoint to `u`.  Well, our `u` here is `(->) r`, which was actually our
very first example of a right-adjoint functor --- it's right-adjoint to `(,)`.
So, `Fold r` is apparently a right-adjoint, like we guessed previously!  More
specifically, it looks like like `Fold r` is right-adjoint to `Free ((,) r)`.

At least, we've reached our goal!  We found an adjunction for `Fold r` in a
purely algebraic way, and deduced it to be right-adjunct to `Free ((,) r)`.

At this point we have our answer, so we can stop here.  But it's possible to go
a little further, to find a true "perfect companion" for `Fold r`.  Its perfect
match and conceptual opposite, as the adjunction mythos claims.

We know that `Free f` is, itself, a fixed-point -- it's the fixed point of
`Sum (Const a) f` (from *[Data.Functor.Sum][Sum]*).  So `Free ((,) r) a` is the
fixed-point of `Sum (Const a) ((,) r)`.  Since we are looking at conceptual
opposites, maybe let's try using the `Mu` fixed-point operator, to be opposite
of the `Nu` that `Fold r` is.  This also makes sense because this is something
we're going to "tear down" with a `Fold`, and `Mu` is good at being torn down.

[Sum]: https://hackage.haskell.org/package/base/docs/Data-Functor-Sum.html

```haskell
newtype Mu f = Mu (forall x. (f x -> x) -> x)

type EL r a = Mu (Sum (Const a) ((,) r))

newtype EL r a = EL {
    runEL :: forall x. (Either a (r, x) -> x) -> x
  }

-- or, with some shuffling around, recognizing that `Either a b -> c` is
-- equivalent to `(a -> c, b -> c)`
newtype EL r a = EL {
    runEL :: forall x. (a -> x) -> (x -> r -> x) -> x
  }
```

The new `EL` is actually isomorphic to the `EnvList` one we wrote earlier (as
long as the list is finite), meaning that one can encode the other, and they
have identical structure.  Writing functions to convert between the two can be
fun; [here is one solution][exercise], and there's a [bonus
solution][exercisebonus] if you can write it using only the [*new*
instance][elinstance] for `Adjunction (EL r) (Fold r)` and
`F.foldMap`, since it can be shown that all adjuncts are unique up to
isomorphism.

!!![exercise]:adjunctions/foldl-algebraic.hs "toOld ::" "fromOld ::"
!!![exercisebonus]:adjunctions/foldl-algebraic.hs "toOldAdj ::" "fromOldAdj ::"
!!![repinstance]:adjunctions/foldl-algebraic.hs "instance Representable"


And...this looks pretty neat, I think.  In the end we discover that these two
types are adjoints to each other:[^eladjoint]

[^eladjoint]: The instance is [written out here][elinstance].

!!![elinstance]:adjunctions/foldl-algebraic.hs "instance Adjunction"

```haskell
data Fold r a = forall x. Fold            (x -> a)    (x -> r -> x)    x
data EL   r a =           EL   (forall x. (a -> x) -> (x -> r -> x) -> x)
```

They look superficially syntactically similar and I don't really know what to
make of that ... but a lot of "opposites" seem to be paired here.  The
existential `x` in `Fold` becomes a Rank2 universal in `EList`, and the `x ->
a` in `Fold` becomes an `a -> x` in `EList`. Neat neat.

Adjunctions: take an idea and just make everything opposite.

One nice thing about this representation is that writing the fundamental
operation of `Fold` (that is, `fold`) becomes really clean:[^tabulate]

```haskell
!!!adjunctions/foldl-algebraic.hs "foldEL ::"
```

And this is, maybe, the real treasure all along.

[^tabulate]: Implementing a `tabulate` equivalent is left as an exercise --- [solution here][tabex]

!!![tabex]:adjunctions/foldl-algebraic.hs "tabulateEL ::"
