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
Haskell that I hadn't realized before.

foldl
-----

The first concept is the great *[foldl][]* library, which provides a nice
"stream processor" type called `Fold`, where `Fold r a` is a stream processor
that takes a stream of `r`s and produces an `a`:

[foldl]: http://hackage.haskell.org/package/foldl

```haskell
sum  :: Num a        => Fold a a
mean :: Fractional a => Fold a a
elem :: Eq a         => a -> Fold a Bool

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
folds

```haskell
{-# LANGUAGE ApplicativeDo #-}

variance :: Fractional a => Fold a a
variance = do
    m  <- mean
    m2 <- lmap (^2) mean     -- the mean of squared items
    pure (m2 - m*m)

varianceTooBig :: Fractional a => Fold a Bool
varianceTooBig = (> 3) <$> variance
```

Most importantly, `Fold r` is an instance of both `Functor` and `Applicative`,
so you can map over and combine the results of different folds.

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
right!  Let's go adjunction hunting!

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

These also come in pairs!  And it's possible to write the other pair
(`leftAdjunct` and `rightAdjunct`) in terms of this pair, actually --- so
finding one is finding the other.

`indexAdjunction` means: if it's possible to "extract" from `u b` to `b` using
only an `f ()` as extra information, then `u` might be right-adjoint to `f`.

`tabulateAdjunction` means: if it's possible to "generate" a `u b` based on a
function that "builds" a `b` from `f ()`, then `u` might right-adjoint to `f`.

### Adjoints to `Fold`

In the case of `Fold`, there is actually only one function that takes a `Fold r
a` and returns an `a`:

```haskell
fold :: Fold r b -> [r] -> b
````

(the type has been simplified and re-labeled, for illustration's sake)

You "give" a `Fold r b` and "get" an `b` (and so they have opposite
polarities/positions).  This sort of function would make `Fold r` a *right
adjoint*, since the naked type `b` is the final result, not the input.

Of our common patterns, this one looks a looooot like `indexAdjunction`.

```haskell
fold            :: Fold r b -> [r]  -> b
indexAdjunction :: Fold r b -> f () -> b
```

This means that `Fold r b` is right-adjoint to some functor `f` where `f () =
[r]`.  A good first guess (just a hunch?) would be to just have `f a = ([r],
a)`:

```haskell
data EnvList r a = EnvList [r] a
  deriving (Show, Eq, Ord, Functor)
```

`EnvList r` is essentially just a *list* of `r`s.  It is now also our suspect
for a potential left-adjoint to `Fold r`: a "conceptual opposite".

```haskell
indexFold :: Fold r b -> EnvList r () -> b
indexFold fld (EnvList rs _) = fold f rs
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
tabulateFold :: (EnvList r () -> b) -> Fold r b
tabulateFold f = F.foldMap (\r -> EnvList [r] ())
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

*   `EnvList r`: Is a list of `r`
*   `Fold r`: Consumes a list of `r`

Or, in terms of the result of the functor application:

*   `EnvList r a`
    *   A list of `r`
    *   Tuplied with an `a`

*   `Fold r a`
    *   Consumes a list of `r`
    *   Produces an `a` as a result

It seems to "flip" the idea of "list vs. list consumer", and *also* the idea of
"tupled vs. producing".

In addition, lists seem to be at the heart of how to create and consume a `Fold
r`.

`fold` can be thought of as the fundamental way to *consuming* a `Fold r`. This
makes the adjunction against `EnvList r` make sense: what good is the *ability*
to fold ... if there is nothing *to fold*?  `EnvList r` (a list of `[r]`) is
intimately related to `Fold r`: they are the yin and yang, peanut butter and
jelly, night and day.  Their fates are intertwined from their very inception.
You cannot have one without the other.

In addition, `F.foldMap` is arguably the fundamental way to *construct* a `Fold
r`.  A `Fold r` is, fundamentally, a list processor --- which is what `EnvList
r a -> b` literally is (an `[r] -> b`).  `Fold r` and `EnvList r` --- [dyads in
the force][dyad].  Or, well...I guess literally monads, since [all adjunctions give
rise to monads][monads]...and comonads too.

[dyad]: https://starwars.fandom.com/wiki/Dyad_in_the_Force
[monads]: http://www.stephendiehl.com/posts/adjunctions.html

The fact that `EnvList r` and `Fold r` form an adjunction together formalizes
the fact that they are conceptually "opposite" concepts, and also that they are
bound together by destiny in a close and fundamental way.

Investigations
--------------

Let's take a look at some of the useful helper functions that an instance of
`Adjunction` gives us for `Fold r`.  For all of these, I'm going to write them
first as `EnvList r a`, and then also as `([r], a)`, to help make things
clearer.

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






<!-- There are a few nice practical advantages that come from identifying -->
<!-- mathematical abstractions in Haskell. -->

<!-- *   Once you do, you often get a nice set of "helper functions" to go -->
<!--     with it, written over all instances of that abstraction. -->
<!-- *   You can leverage mathematical identities and equivalences to simplify, -->
<!--     refactor, or speed up your code -->
<!-- *   You gain some new insight to the fundamental structure of your data -->
<!--     structures that you might not have known before. -->

<!-- These three advantages aren't necessarily always found in equal amounts in -->
<!-- every mathematical abstraction --- some are weighted more heavily in one than -->
<!-- the other. -->

<!-- ### Trial Run -->

<!-- Let's try out our hunting chops on one we already know about.  Let's say we -->
<!-- want to find a functor that is adjoint to `SameTuple`.  What sort of functions -->
<!-- can we write on `SameTuple`? -->

<!-- Well, one thing we can do is extract `SameTuple a -> a` based on which field to -->
<!-- extract -->

<!-- ```haskell -->
<!-- extractTuple :: Bool -> SameTuple a -> a -->
<!-- extractTuple False (ST (x, _)) = x -->
<!-- extractTuple True  (ST (_, y)) = y -->
<!-- ``` -->

<!-- This seems to fit in with `indexAdjunction` ... it suggests that `SameTuple` is -->
<!-- right-adjoint to some `f` where `f ()` is `Bool`.  So... something like... -->

<!-- ```haskell -->
<!-- data EnvBool a = EnvBool Bool a -->
<!-- ``` -->




<!-- Now, on to the hunting.  Let's say we have functor `Q` and we want to identify -->
<!-- any adjoints.  We want to spot functions that use both `Q a` and `a`, in -->
<!-- [opposite positions][polarity].  Common patterns functions for "converting -->
<!-- between" the going-in and going-out functions. In -->
<!-- [Data.Functor.Adjunctions][adjunctions], these are called `leftAdjunct` and -->
<!-- `rightAdjunct`: -->

<!-- ```haskell -->
<!-- leftAdjunct  :: Adjunction f u => (f a -> b) -> (a -> u b) -->
<!-- rightAdjunct :: Adjunction f u => (a -> u b) -> (f a -> b) -->
<!-- ``` -->

<!-- These functions are significant because they are essentially the adjunctions -->
<!-- "in practice":  Sure, an `(r, a) -> b` is useful, but "using" the adjunction -->
<!-- means that you can convert between `(r, a) -> b` (`uncurry`) and backwards. -->

<!-- Another very common pattern is an "indexing" function and a tabulating -->
<!-- function: -->

<!-- ```haskell -->
<!-- indexAdjunction    :: Adjunction f u => u b -> f () -> b -->
<!-- tabulateAdjunction :: Adjunction f u => (EnvList r () -> b) -> Fold r b -->
<!-- ``` -->

<!-- If `f ()` can be used as an "index" to get a `b` out of a `u b`, then -->

<!-- Namely, we are hunting for functions that involve both a `Fold r a` and an `a`, -->
<!-- in [opposite positions][polarity], as well as some other value. -->






<!-- We can write the instance based on this.  The two functions in `Adjunction` -->

<!-- ```haskell -->
<!-- import           Control.Foldl (Fold) -->
<!-- import qualified Control.Foldl as F -->

<!-- instance Adjunction (EnvList r) (Fold r) where -->
<!--     rightAdjunct :: (a -> Fold r b) -> (EnvList r a -> b) -->
<!--     rightAdjunct f (EnvList rs x) = F.fold (f x) rs -->

<!--     leftAdjunct :: (EnvList r a -> b) -> (a -> Fold r b) -->
<!--     leftAdjunct = ??? -->
<!-- ``` -->


<!-- What would `leftAdjunct` be?  We can simplify out the type by splitting out -->
<!-- `[r]` and `a`, to help us see what is going on: -->

<!-- ```haskell -->
<!-- leftAdjunct :: ([r] -> a -> b) -> (a -> Fold r b) -->
<!-- ``` -->

<!-- And, it seems that when `a ~ ()`, we get something that more closely matches -->
<!-- the spirit of how `Fold r` is used.  Let's make that substitution to help -->
<!-- de-clutter the type signature: -->

<!-- ```haskell -->
<!-- leftAdjunct :: ([r] -> b) -> Fold r b -->
<!-- ``` -->

<!-- This just tells us that, given any list processor, we can write a fold -->
<!-- representing that list processor.  We can use `foldMap` from the library to -->
<!-- write this: -->

<!-- ```haskell -->
<!-- F.foldMap (:[]) -->
<!--     :: ([r] -> b) -->
<!--     -> Fold r b -->
<!-- ``` -->

<!-- And so we can complete our instance: -->


<!-- ```haskell -->
<!-- instance Adjunction (EnvList r) (Fold r) where -->
<!--     rightAdjunct :: (a -> Fold r b) -> (EnvList r a -> b) -->
<!--     rightAdjunct f (EnvList rs x) = F.fold (f x) rs -->

<!--     leftAdjunct :: (EnvList r a -> b) -> (a -> Fold r b) -->
<!--     leftAdjunct f x = F.foldMap (:[]) (f (EnvList rs x)) -->
<!-- ``` -->

