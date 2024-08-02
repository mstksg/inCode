---
title: "Levels of Type Safety in Haskell: Lists"
categories: Haskell
tags: functional programming, dependent types, haskell, singletons, types
create-time: 2021/02/25 00:30:53
identifier: type-levels
slug: levels-of-type-safety-haskell-lists
---

One thing I always appreciate about Haskell is that you can often choose the
level of type-safety you want to work at.  Haskell offers tools to be able to
work at *both* extremes (which are often undesirable except in extraordinary
circumstances), whereas most languages only offer some limited part of the
spectrum.  No promises about the ergonomics of either extreme, though --- just
that they are possible!  Of course, often times Haskellers get a reputation of
always dialing it up to extreme safety just because they can, and not because
it's a good idea :)  To be fair, I'm not going to claim that I'm immune to this
effect; something that has helped me, though, is being consciously aware of the
different levels of type safety that are available, and the
benefits/drawbacks/unique advantages to each.

So, here is a rundown of "levels" of type safety that you can operate at
when working with the ubiquitous *strict* linked list data type, and how to use
them!  I genuinely believe all of these are useful (or useless) in their own
different circumstances.  When possible, we'll prefer "structural type safety"
(that is, enforced by the structure of the type itself) as opposed to
[restricted constructors][refined] (just to allow us to make more interesting
comparisons and contrasts) and also more flexible/parameterized options for
type safety instead of hard-coded restrictions.

[refined]: https://hackage.haskell.org/package/refined

This post is written for a late beginner or intermediate Haskeller, who is
already familiar with ADTs and defining their own custom list type like `data
List a = Nil | Cons a (List a)`.


Level 0: Could be anything
--------------------------

Let's start off with a baseline type that demonstrates a little bit of the
quirks of working at the unsafe extremes in Haskell.

A reasonable idea might be to just make a "black hole" data type that could be
anything:

```haskell
!!!type-levels/Level0.hs "data Any ::"
```

(This data type declaration written using [GADT Syntax][], and the name was
chosen because it resembles [the Any type in base][Any])

[GADT Syntax]: https://typeclasses.com/ghc/gadt-syntax
[Any]: https://hackage.haskell.org/package/base-4.14.1.0/docs/GHC-Exts.html#t:Any

So you can have values:

```haskell
!!!type-levels/Level0.hs "anyInt ::" "anyBool ::" "anyList ::"
```

A value of any type can be given to `MkAny`, and the resulting type
will have type `Any`.

However, this type is a black hole; you can't really do anything with the
values inside it because of parametric polymorphism: you must treat any value
inside it in a way that is compatible with a value of *any* type.  But there
aren't *too* many useful things you can do with something in a way that is
compatible with a value of any type (things like, `id :: a -> a`, `const 3 :: a
-> Int`).  In the end, it's essentially isomorphic to unit `()`.

So we can actually modify this slightly to match the typical dynamically typed
language picture of "could be anything": we should at least be able to query
the type for things we can do with it.

To get there, we can instead allow some sort of witness on the type of the
value:

```haskell
!!!type-levels/Level0.hs "data Sigma ::"
```


And the most classic witness is [`TypeRep`][TypeRep] from *base*, which is a
witness that lets you "match" on the type.

[TypeRep]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Type-Reflection.html#t:TypeRep

```haskell
!!!type-levels/Level0.hs "showIfBool ::"
```



Here we can use `TypeRep`'s interface to "match" (using `testEquality`) on if
the value inside is a `Bool`.  If the match works (and we get `Just Refl`) then
we can treat `x` as a `Bool` in that case.  If it doesn't (and we get
`Nothing`), then we do what we would want to do otherwise.

```haskell
ghci> let x = MkSigma typeRep True
ghci> let y = MkSigma typeRep (4 :: Int)
ghci> showIfBool x
"True"
ghci> showIfBool y
"Not a Bool"
```

This pattern is common enough that there's the *[Data.Dynamic][]* module in
base that is `Sigma TypeRep`, and wraps our `testEquality` dance above in a
function called `fromDynamic`:

[Data.Dynamic]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Dynamic.html

```haskell
!!!type-levels/Level0.hs "showIfDyanimc ::"
```

For make our life easier in the future, let's write a version of `fromDynamic`
for our `Sigma TypeRep`:

```haskell
!!!type-levels/Level0.hs "castSchema ::" "castSchema' ::"
```

But the reason why I'm presenting the more generic `Sigma` instead of the
specific `type Dynamic = Sigma TypeRep` is that you can swap out `TypeRep` to
get other interesting types.  For example, if you had a witness of showability:

```haskell
!!!type-levels/Level0.hs "data Showable ::" "showableInt ::" "showableBool ::"
```


(This type is related to `Dict Show` from the [constraints][] library; it's
technically `Compose Dict Show`)

[constraints]: https://hackage.haskell.org/package/constraints-0.13/docs/Data-Constraint.html#t:Dict

And now we have a type `Sigma Showable` that's a bit of a black hole, but we
can at least use `show` on it:

```haskell
!!!type-levels/Level0.hs "showSigma ::"
```

```haskell
ghci> let x = MkSigma WitShowable True
ghci> let y = MkSigma WitShowable 4
ghci> showSigma x
"True"
ghci> showSigma y
"4"
```

This is the "[existential typeclass antipattern][palmer]", but since we are
talking about different ways we can tease the type system, it's probably worth
mentioning!  Note that `Show` is a bit silly of a typeclass to use in this
context because a `Sigma Showable` is equivalent to just a `String`: once you
match on the constructor to get the value, the only thing you can do with the
value is `show` it anyway.

[palmer]: https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/

One fun thing we can do is provide a "useless witness", like `Proxy`:

```haskell
!!!type-levels/Level0.hs "data Proxy ::" "uselessBool ::"
```

So a value like `MkSigma Proxy True :: Sigma Proxy` is truly a useless data
type (basically our `Any` from before), since we know that `MkSigma` constrains
*some* value of *some* type, but there's no witness to give us any clue on how
we can use it.  A `Sigma Proxy` is isomorphic to `()`.

On the other extreme, we can use a witness to constrain the value to only be a
specific type, like `IsBool`:

```haskell
!!!type-levels/Level0.hs "data IsBool ::" "justABool ::"
```

So you can have a value of type `MkSigma ItsABool True :: Sigma IsBool`, or
`MkSigma ItsABool False`, but `MkSigma ItsABool 2` will not typecheck ---
remember, to make a `Sigma`, you need a `p a` and an `a`.  `ItsABool :: IsBool
Bool`, so the `a` you put in must be `Bool` to match.  `Sigma IsBool` is
essentially isomorphic to `Bool`.

There's a general version of this too, `(:~:) a` (from *[Data.Type.Equality][]*
in base).  `(:~:) Bool` is our `IsBool` earlier.  `Sigma ((:~:) a)` is
essentially exactly `a`...basically bringing us incidentally back to complete
type safety?  Weird.  Anyway.

```haskell
!!!type-levels/Level0.hs "justAnInt ::"
```

[Data.Type.Equality]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Type-Equality.html#t::-126-:

I think one interesting thing to see here is that being "type-unsafe" in
Haskell can be much less convenient than doing something similar in a
dynamically typed language like python --- that's why I don't fancy those
memes/jokes about how dynamically typed languages are just "static types with a
single type".  The actual way you use those types lend themselves to different
ergonomics, and the reductionist take doesn't quite capture that nuance.

Level 1: Heterogeneous List
---------------------------

The lowest level of safety in which a list might be useful is the dynamically
heterogeneous list.  This is the level where lists (or "arrays") live in most
dynamic languages.

```haskell
!!!type-levels/Level1.hs "type HList"
```

We tag values with a witness `p` for the same reason as before: if we don't
provide *some* type of witness, our type is useless.

The "dynamically heterogeneous list of values of any type" is `HList TypeRep`.
This is somewhat similar to how functions with positional arguments work in a
dynamic language like javascript.  For example, here's a function that connects
to a host (`String`), optionally taking a port (`Int`) and a method (`Method`).

```haskell
!!!type-levels/Level1.hs "data Method" "indexHList" "mkConnection ::"
```

Of course, this would *probably* be better expressed in Haskell as a function
of type `Maybe String -> Maybe Int -> Maybe Method -> IO ()`.  But maybe this
could be useful in a situation where you would want to offer the ability to
take arguments in any order?  We could "find" the first value of a given type:

```haskell
!!!type-levels/Level1.hs "findValueOfType ::"
```

Then we could write:

```haskell
!!!type-levels/Level1.hs "mkConnectionAnyOrder ::"
```


But is this a good idea?  Probably not.

One very common usage of this type is for "extensible" systems that let you
store components of different types in a container, as long as they all support
some common interface (ie, the widgets system from the [Luke Palmer][palmer]
post).

For example, we could have a list of any item as long as the item is an
instance of `Show`: that's `HList Showable`!

```haskell
!!!type-levels/Level1.hs "showAll ::"
```

```haskell
ghci> let xs = [MkSigma WitShowable 1, MkSigma WitShowable True]
ghci> showAll xs
["1", "True"]
```

For `Show`, this is, again, a rather silly thing to do because of the reasons
mentioned in the Palmer post --- namely, that you could always just have a
`[String]` directly instead of an `HList Showable`, since the only thing you
can do with a `Sigma Showable` is `show` it.

For fun, let's imagine some other things we could fill in for `p`.  If we use
`HList Proxy`, then we basically don't have any witness at all.  We can't use
the values in the list in any meaningful way; `HList Proxy` is essentially the
same as `Natural`, since the only information is the length.

If we use `HList IsBool`, we basically have `[Bool]`, since every item must be
a `Bool`!  In general, `HList ((:~:) a)` is the same as `[a]`.

Level 2: Homogeneous Dynamic List
---------------------------------

A next level of type safety we can add is to ensure that all elements in the
list are of the same type.  This adds a layer of usefulness because there are a
lot of things we might want to do with the elements of a list that are only
possible if they are all of the same type.

First of all, let's clarify a subtle point here.  It's very easy in Haskell to
*consume* lists where all elements are of the same (but not necessarily known)
type.  Functions like `sum :: Num a => [a] -> a` and `sort :: Ord a => [a] ->
[a]` do that.  This is polymorphism, where the function is written to not worry
about the type, and the ultimate *caller* of the function must pick the type they want
to use with it. For the sake of this discussion, we aren't talking about
*consuming* values -- we're talking about *producing* and *storing* values
where the *producer* (and not the consumer) controls the type variable.

To do this, we can flip the witness to *outside* the list:

```haskell
!!!type-levels/Level2.hs "data SomeList ::"
```

We can write some meaningful predicates on this list --- for example, we can
check if it is monotonic (the items increase in order)

```haskell
!!!type-levels/Level2.hs "data Comparable ::" "monotonic ::" "monotonicSomeList ::"
```

If we take a step back, we can see that this is a little silly...what
value does `monotonicSomeList` offer over `monotonic`? However, remember the
subtlety I pointed out before: `monotonic` (and `monotonicSomeList`) are not
the real advantage here, the advantage is writing a function that *produces*
"list of some sortable type". For example, we might want to query a database
for a column of values, but we only know the *name* of the column and not the
*type* of the column --- we just know that the values in that column are
sortable.

For a contrived one, let's think about pulling such a list from IO:

```haskell
!!!type-levels/Level2.hs "getItems ::" "analyzeGottenItems ::"
```

Consider also an example where process items different based on what type they
have:

```haskell
!!!type-levels/Level2.hs "processList ::"
```

I'm just kidding, this is not useful, please use a real closed ADT if you ever
want to do this.

Anyway, this pattern is overall similar to how lists are often used in practice
for dynamic languages: *usually* when we use lists, we expect them all to have
items of the same type or interface. However, using lists this way (in a
language without type safety) makes it really tempting to hop down into Level
1, where you start throwing "alternatively typed" things into your list, as
well, for convenience. All of a sudden, any consumers must now check the type
of the items, and a lot of things are going to start needing unit tests.

Now let us take a bit to talk about ascending and descending between each
levels.  In the general case we don't have much to work with, but let's assume
our constraint is `TypeRep` here, so we can match for type equality.

We can move from Level 2 to Level 1 by moving the `TypeRep` into the values of
the list, and we can move from Level 2 to Level 0 by converting our `TypeRep a`
into a `TypeRep [a]`:

```haskell
!!!type-levels/Level2.hs "someListToHList ::" "someListToSigma ::"
```

`App` here as a constructor lets us come `TypeRep`s: `App :: TypeRep f ->
TypeRep a -> TypeRep (f a)`.

Going the other way around is trickier. For `HList`, we don't even know if
every item has the same type, so we can only successfully move up if every item
has the same type.  So, first we get the `typeRep` for the first value, and
then cast the other values to be the same type if possible:

```haskell
!!!type-levels/Level2.hs "hlistToSomeList ::"
```

To go from `Sigma TypeRep`, we first need to match the `TypeRep` as some `f a`
application using the `App` pattern...then we can check if `f` is `[]` (list),
then we can create a `SomeList` with the `TypeRep a`.  *But*, `testEquality`
can only be called on things of the same kind, so we have to verify that `f`
has kind `Type -> Type` first, so that we can even call `testEquality` on `f`
and `[]`! Phew! Dynamic types are hard!

Level 3: Homogeneous Typed List
-------------------------------

Ahh, now right in the middle, we've reached Haskell's ubiquitous list type!  It
is essentially:

```haskell
data List :: Type -> Type where
    Nil  :: List a
    Cons :: a -> List a -> List a
```

I don't have too much to say here, other than to acknowledge that this is truly
a "sweet spot" in terms of safety vs. unsafety and usability.  This simple
`List a` / `[a]` type has so many benefits from type-safety:

*   It lets us write functions that can meaningfully say that the input and
    result types are the same, like `take :: Int -> [a] -> [a]`
*   It lets us write functions that can meaningfully link lists and the items
    in the list, like `head :: [a] -> a` and `replicate :: Int -> a -> [a]`.
*   It lets us write functions that can meaningfully state relationships
    between input and results, like `map :: (a -> b) -> [a] -> [b]`
*   We can require two input lists to have the same type of items, like `(++)
    :: [a] -> [a] -> [a]`
*   We can express complex relationships between inputs and outputs, like
    `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`.

The property of being able to state and express relationships between the
values of input lists and output lists and the items in those lists is
extremely powerful, and also extremely ergonomic to use in Haskell.  It can be
argued that Haskell, as a language, was tuned explicitly to be used with the
least friction at this exact level of type safety.

We're going to talk the least about this one because it's probably the one you
are all familiar with!

<!-- the rest of these are me in 2024 guessing as to what i was thinking in 2021 -->

Level 4: Fixed-size List
------------------------

For the next level of safety, we're not going to try to enforce anything on the
contents of the list, but we can try to enforce something on the *spline* of
the list: the number of items!

To me, this level still feels very natural in Haskell to write in, although in
terms of usability we are starting to bump into some of the things Haskell is
lacking for higher type safety ergonomics. I've talked about [fixed-length
vector types in depth before][fixvec-2], so this is going to be a high-level
view contrasting this level with the others.

[fixvec-2]: https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html


Level 5: Local Structure Enforced List
--------------------------------------

Level 6: Global structure Enforced List
---------------------------------------

