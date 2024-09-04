---
title: "Seven Levels of Type Safety in Haskell: Lists"
categories: Haskell
tags: functional programming, dependent types, haskell, singletons, types
create-time: 2021/02/25 00:30:53
date: 2024/09/04 10:35:47
identifier: type-levels
slug: levels-of-type-safety-haskell-lists
---

One thing I always appreciate about Haskell is that you can often choose the
level of type-safety you want to work at. Haskell offers tools to be able to
work at *both* extremes, whereas most languages only offer some limited part of
the spectrum. Picking the right level often comes down to being consciously
aware of the benefits/drawbacks/unique advantages to each.

So, here is a rundown of seven "levels" of type safety that you can operate at
when working with the ubiquitous linked list data type, and how to use them! I
genuinely believe all of these are useful (or useless) in their own different
circumstances, even though the "extremes" at both ends are definitely pushing
the limits of the language.

This post is written for an intermediate Haskeller, who is already familiar
with ADTs and defining their own custom list type like `data List a = Nil |
Cons a (List a)`. But, be advised that *most* of the techniques discussed in
this post (especially at both extremes) are considered esoteric at best and
harmful at worst for most actual real-world applications :)

All of the code here is [available online][code samples] here, and if you check
out the repo and run `nix develop` you should be able to load them all in ghci
as well:

!!![code-samples]:type-levels/flake.nix

```bash
$ cd code-samples/type-levels
$ nix develop
$ ghci
ghci> :load Level1.hs
```

Level 1: Could be anything
--------------------------

*[Code available here][level1]*

!!![level1]:type-levels/Level1.hs

What's the moooost type-unsafe you can be in Haskell? Well, we can make a
"black hole" data type that could be anything:

```haskell
!!!type-levels/Level1.hs "data Any ::"
```

(This data type declaration written using [GADT Syntax][], and the name was
chosen because it resembles [the Any type in base][Any])

[GADT Syntax]: https://typeclasses.com/ghc/gadt-syntax
[Any]: https://hackage.haskell.org/package/bas/docs/GHC-Exts.html#t:Any

So you can have values:

```haskell
!!!type-levels/Level1.hs "anyInt ::" "anyBool ::" "anyList ::"
```

A value of any type can be given to `MkAny`, and the resulting type
will have type `Any`.

However, this type is *truly* a black hole; you can't really do anything with
the values inside it because of parametric polymorphism: you must treat any
value inside it in a way that is compatible with a value of *any* type. But
there aren't *too* many useful things you can do with something in a way that
is compatible with a value of any type (things like, `id :: a -> a`,
`const 3 :: a -> Int`). In the end, it's essentially isomorphic to unit `()`.

However, this isn't really how dynamic types work. In other languages, we are
at least able to query and interrogate a type for things we can do with it
using runtime reflection. To get there, we can instead allow some sort of
witness on the type of the value. Here's `Sigma`, where `Sigma p` is a value
`a` paired with some witness `p a`:

```haskell
!!!type-levels/Level1.hs "data Sigma ::"
```


And the most classic witness is [`TypeRep`][TypeRep] from *base*, which is a
witness that lets you "match" on the type.

[TypeRep]: https://hackage.haskell.org/package/base/docs/Type-Reflection.html#t:TypeRep

```haskell
!!!type-levels/Level1.hs "showIfBool ::"
```

This uses *type application syntax*, `@Bool`, that lets us pass in the
*type* `Bool` to the function `typeRep :: Typeable a => TypeRep a`.

Now we can use `TypeRep`'s interface to "match" (using `testEquality`) on if
the value inside is a `Bool`. If the match works (and we get `Just Refl`) then
we can treat `x` as a `Bool` in that case. If it doesn't (and we get
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
base that is `Sigma TypeRep`, and `testEquality` is replaced with that module's
`fromDynamic`:

[Data.Dynamic]: https://hackage.haskell.org/package/base/docs/Data-Dynamic.html

```haskell
!!!type-levels/Level1.hs "showIfBoolDynamic ::"
```

For make our life easier in the future, let's write a version of `fromDynamic`
for our `Sigma TypeRep`:

```haskell
!!!type-levels/Level1.hs "castSigma ::" "castSigma' ::"
```

But the reason why I'm presenting the more generic `Sigma` instead of the
specific `type Dynamic = Sigma TypeRep` is that you can swap out `TypeRep` to
get other interesting types. For example, if you had a witness of showability:

```haskell
!!!type-levels/Level1.hs "data Showable ::" "showableInt ::" "showableBool ::"
```

(This type is related to `Dict Show` from the [constraints][] library; it's
technically `Compose Dict Show`)

[constraints]: https://hackage.haskell.org/package/constraints-0.13/docs/Data-Constraint.html#t:Dict

And now we have a type `Sigma Showable` that's kind of of "not-so-black": we
can at least use `show` on it:

```haskell
!!!type-levels/Level1.hs "showSigma ::"
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
talking about different ways we can push the type system, it's probably worth
mentioning. In particular, `Show` is a silly typeclass to use in this
context because a `Sigma Showable` is equivalent to just a `String`: once you
match on the constructor to get the value, the only thing you can do with the
value is `show` it anyway.

[palmer]: https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/

One fun thing we can do is provide a "useless witness", like `Proxy`:

```haskell
!!!type-levels/Level1.hs "data Proxy" "uselessBool ::"
```

So a value like `MkSigma Proxy True :: Sigma Proxy` is truly a useless data
type (basically our `Any` from before), since we know that `MkSigma` constrains
*some* value of *some* type, but there's no witness to give us any clue on how
we can use it. A `Sigma Proxy` is isomorphic to `()`.

On the other extreme, we can use a witness to constrain the value to only be a
specific type, like `IsBool`:

```haskell
!!!type-levels/Level1.hs "data IsBool ::" "justABool ::"
```

So you can have a value of type `MkSigma ItsABool True :: Sigma IsBool`, or
`MkSigma ItsABool False`, but `MkSigma ItsABool 2` will not typecheck ---
remember, to make a `Sigma`, you need a `p a` and an `a`. `ItsABool :: IsBool
Bool`, so the `a` you put in must be `Bool` to match. `Sigma IsBool` is
essentially isomorphic to `Bool`.

There's a general version of this too, `(:~:) a` (from *[Data.Type.Equality][]*
in base). `(:~:) Bool` is our `IsBool` earlier. `Sigma ((:~:) a)` is
essentially exactly `a`...basically bringing us incidentally back to complete
type safety? Weird. Anyway.

```haskell
!!!type-levels/Level1.hs "justAnInt ::"
```

[Data.Type.Equality]: https://hackage.haskell.org/package/base/docs/Data-Type-Equality.html#t::-126-:

I think one interesting thing to see here is that being "type-unsafe" in
Haskell can be much less convenient than doing something similar in a
dynamically typed language like python --- that's why I don't fancy those
memes/jokes about how dynamically typed languages are just "static types with a
single type". The actual way you use those types lend themselves to different
ergonomics, and the reductionist take doesn't quite capture that nuance.

Level 2: Heterogeneous List
---------------------------

*[Code available here][level2]*

!!![level2]:type-levels/Level2.hs

The lowest level of safety in which a list might be useful is the dynamically
heterogeneous list. This is the level where lists (or "arrays") live in most
dynamic languages.

```haskell
!!!type-levels/Level2.hs "type HList"
```

We tag values with a witness `p` for the same reason as before: if we don't
provide *some* type of witness, our type is useless.

The "dynamically heterogeneous list of values of any type" is `HList TypeRep`.
This is somewhat similar to how functions with positional arguments work in a
dynamic language like javascript. For example, here's a function that connects
to a host (`String`), optionally taking a port (`Int`) and a method (`Method`).

```haskell
!!!type-levels/Level2.hs "data Method" "indexHList" "mkConnection ::"
```

Of course, this would *probably* be better expressed in Haskell as a function
of type `Maybe String -> Maybe Int -> Maybe Method -> IO ()`. But maybe this
could be useful in a situation where you would want to offer the ability to
take arguments in any order? We could "find" the first value of a given type:

```haskell
!!!type-levels/Level2.hs "findValueOfType ::"
```

Then we could write:

```haskell
!!!type-levels/Level2.hs "mkConnectionAnyOrder ::"
```


But is this a good idea? Probably not.

Anyway, one very common usage of this type is for "extensible" systems that let
you store components of different types in a container, as long as they all
support some common interface (ie, the widgets system from the [Luke
Palmer][palmer] post).

For example, we could have a list of any item as long as the item is an
instance of `Show`: that's `HList Showable`!

```haskell
!!!type-levels/Level2.hs "showAll ::"
```

```haskell
ghci> let xs = [MkSigma WitShowable 1, MkSigma WitShowable True]
ghci> showAll xs
["1", "True"]
```

Again, `Show` is a bad typeclass to use for this because we might as well be
storing `[String]`. But for fun, let's imagine some other things we could fill
in for `p`. If we use `HList Proxy`, then we basically don't have any witness
at all. We can't use the values in the list in any meaningful way; `HList
Proxy` is essentially the same as `Natural`, since the only information is the
length.

If we use `HList IsBool`, we basically have `[Bool]`, since every item must be
a `Bool`! In general, `HList ((:~:) a)` is the same as `[a]`.

Level 3: Homogeneous Dynamic List
---------------------------------

*[Code available here][level3]*

!!![level3]:type-levels/Level3.hs

A next level of type safety we can add is to ensure that all elements in the
list are of the same type. This adds a layer of usefulness because there are a
lot of things we might want to do with the elements of a list that are only
possible if they are all of the same type.

First of all, let's clarify a subtle point here. It's very easy in Haskell to
*consume* lists where all elements are of the same (but not necessarily known)
type. Functions like `sum :: Num a => [a] -> a` and `sort :: Ord a => [a] ->
[a]` do that. This is "polymorphism", where the function is written to not
worry about the type, and the ultimate *caller* of the function must pick the
type they want to use with it. For the sake of this discussion, we aren't
talking about *consuming* values -- we're talking about *producing* and
*storing* values where the *producer* (and not the consumer) controls the type
variable.

To do this, we can flip the witness to *outside* the list:

```haskell
!!!type-levels/Level3.hs "data SomeList ::"
```

We can write some meaningful predicates on this list --- for example, we can
check if it is monotonic (the items increase in order)

```haskell
!!!type-levels/Level3.hs "data Comparable ::" "monotonic ::" "monotonicSomeList ::"
```

This is fun, but, as mentioned before, `monotonicSomeList` doesn't have any
advantage over `monotonic`, because the caller determines the type. What would
be more motivating here is a function that produces "any sortable type", and
the caller has to use it in a way generic over all sortable types. For example,
a database API might let you query a database for a column of values, but you
don't know ahead of time what the exact *type* of that column is. You only know
that it is "some sortable type". In *that case*, a `SomeList` could be useful.

For a contrived one, let's think about pulling such a list from IO:

```haskell
!!!type-levels/Level3.hs "getItems ::" "getAndAnalyze ::"
```

Consider also an example where process items different based on what type they
have:

```haskell
!!!type-levels/Level3.hs "processList ::"
```

(That's [pattern guard][] syntax, if you were wondering)

[pattern guard]: https://wiki.haskell.org/Pattern_guard

In this specific situation, using a closed ADT of all the types you'd actually
want is probably preferred (like `data Value = VBool Bool | VInt Int | VDouble
Double | VString String`), since we only ever get one of four different types.
Using `Comparable` like this gives you a *completely open* type that can take
*any* instance of `Ord`, and using `TypeRep` gives you a *completely open type*
that can take literally *anything*.

This pattern is overall similar to how lists are often used in practice for
dynamic languages: often when we use lists in dynamically typed situations, we
expect them all to have items of the same type or interface. However, using
lists this way (in a language without type safety) makes it really tempting to
hop down into Level 2, where you start throwing "alternatively typed" things
into your list, as well, for convenience. And then the temptation comes to also
hop down to Level 1 and throw a `null` in every once in a while. All of a
sudden, any consumers must now check the type of *every* item, and a lot of
things are going to start needing unit tests.

Now, let's talk a bit about ascending and descending between each levels. In
the general case we don't have much to work with, but let's assume our
constraint is `TypeRep` here, so we can match for type equality.

We can move from Level 3 to Level 2 by moving the `TypeRep` into the values of
the list, and we can move from Level 3 to Level 1 by converting our `TypeRep a`
into a `TypeRep [a]`:

```haskell
!!!type-levels/Level3.hs "someListToHList ::" "someListToSigma ::"
```

`App` here as a constructor lets us come `TypeRep`s: `App :: TypeRep f ->
TypeRep a -> TypeRep (f a)`.

Going the other way around is trickier. For `HList`, we don't even know if
every item has the same type, so we can only successfully move up if every item
has the same type. So, first we get the `typeRep` for the first value, and
then cast the other values to be the same type if possible:

```haskell
!!!type-levels/Level3.hs "hlistToSomeList ::"
```

To go from `Sigma TypeRep`, we first need to match the `TypeRep` as some `f a`
application using the `App` pattern...then we can check if `f` is `[]` (list),
then we can create a `SomeList` with the `TypeRep a`. *But*, `testEquality`
can only be called on things of the same kind, so we have to verify that `f`
has kind `Type -> Type` first, so that we can even call `testEquality` on `f`
and `[]`! Phew! Dynamic types are hard!

```haskell
!!!type-levels/Level3.hs "sigmaToHList ::"
```

Level 4: Homogeneous Typed List
-------------------------------

Ahh, now right in the middle, we've reached Haskell's ubiquitous list type! It
is essentially:

```haskell
data List :: Type -> Type where
    Nil  :: List a
    Cons :: a -> List a -> List a
```

I don't have too much to say here, other than to acknowledge that this is truly
a "sweet spot" in terms of safety vs. unsafety and usability. This simple
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
extremely powerful, and also extremely ergonomic to use in Haskell. It can be
argued that Haskell, as a language, was tuned explicitly to be used with the
least friction at *this* exact level of type safety. Haskell is a "Level 4
language".

Level 5: Fixed-size List
------------------------

*[Code available here][level5]*

!!![level5]:type-levels/Level5.hs

From here on, we aren't going to be "building up" linearly on safety, but
rather showing three structural type safety mechanism of increasing strength
and complexity.

For Level 5, we're not going to try to enforce anything on the contents of the
list, but we can try to enforce something on the *spline* of the list: the
number of items!

To me, this level still feels very natural in Haskell to write in, although in
terms of usability we are starting to bump into some of the things Haskell is
lacking for higher type safety ergonomics. I've talked about [fixed-length
vector types in depth before][fixvec-2], so this is going to be a high-level
view contrasting this level with the others.[^vector]

[fixvec-2]: https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html

[^vector]: Note that I don't really like calling these "vectors" any more,
because in a computer science context the word vector carries implications of
contiguous-memory storage. "Lists" of fixed length is the more appropriate
description here, in my opinion. The term "vector" for this concept arises from
linear algebra, where a vector is inherently defined by its vector _space_,
which [does have an inherent dimensionality][dimension]. But we are talking
about computer science concepts here, not mathematical concepts, so we should
pick the name that provides the most useful implicit connotations.

[dimension]: https://en.wikipedia.org/wiki/Dimension_theorem_for_vector_spaces

The essential concept is to introduce a *phantom type*, a type parameter that
doesn't do anything other than indicate something that we can use in
user-space. Here we will create a type that structurally encodes the natural
numbers 0, 1, 2...:

```haskell
!!!type-levels/Level5.hs "data Nat"
```

So, `Z` will represent zero, `S Z` will represent one, `S (S Z)` will represent
two, etc. We want to create a type `Vec n a`, where `n` will be a type of kind
`Nat` (promoted using DataKinds, which lets us use `Z` and `S` as type
constructors), representing a linked list with `n` elements of type `a`.

We can define `Vec` in a way that structurally matches how `Nat` is
constructed, which is the key to making things work nicely:

```haskell
!!!type-levels/Level5.hs "data Vec" "infixr 5 :+"
```

This is offered in the *[vec][]* library. Here are some example values:

[vec]: https://hackage.haskell.org/package/vec

```haskell
!!!type-levels/Level5.hs "zeroItems ::" "oneItem ::" "twoItems ::" "threeItems ::"
```

Note two things:

1.  `1 :+ 2 :+ VNil` gets automatically type-inferred to be a `Vec (S (S Z))
    a`, because every application of `:+` adds an `S` to the phantom type.
2.  There is *only one way* to construct a `Vec (S (S Z)) a`: by using `:+`
    twice. That means that such a value is a list of exactly two items.

However, the main benefit of this system is *not* so you can create a two-item
list...just use tuples or `data V2 a = V2 a a` from *[linear][]* for that. No,
the main benefit is that you can now encode how arguments in your functions
relate to each other with respect to length.

[linear]: https://hackage.haskell.org/package/linear

For example, the *type* alone of `map :: (a -> b) -> [a] -> [b]` does *not*
tell you that the length of the result list is the same as the length of the
input list. However, consider `vmap :: (a -> b) -> Vec n a -> Vec n b`. Here we
see that the output list must have the same number of items as the input list,
and it's enforced right there in the type signature!

```haskell
!!!type-levels/Level5.hs "vmap ::"
```

And how about `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`? It's not clear
or obvious at all how the final list's length depends on the input lists'
lengths. However, a `vzipWith` would ensure the input lengths are the same size
and that the output list is also the same length:

```haskell
!!!type-levels/Level5.hs "vzipWith ::"
```

Note that both of the inner pattern matches are known by GHC to be exhaustive:
if it knows that the first list is `VNil`, then it knows that `n ~ Z`, so the
second list *has to also* be `VNil`. Thanks GHC!

From here on out, we're now always going to assume that GHC's exhaustiveness
checker is on, so we always handle every branch that GHC tells us is necessary,
and skip handling branches that GHC tells us is unnecessary (through compiler
warnings).

We can even express more complicated relationships with type families
(type-level "functions"):

```haskell
!!!type-levels/Level5.hs "type family Plus" "type family Times" "vconcat ::" "vconcatMap ::"
```

Note that all of these only work in GHC because the structure of the functions
themselves match exactly the structure of the type families. If you follow the
pattern matches in the functions, note that they match exactly with the
different equations of the type family.

Famously, we can totally index into fixed-length lists, in a way that indexing
will not fail. To do that, we have to define a type `Fin n`, which represents
an index into a list of length `n`. So, `Fin (S (S (S Z)))` will be either 0,
1, or 2, the three possible indices of a three-item list.

```haskell
!!!type-levels/Level5.hs "data Fin" "vindex ::"
```

`Fin` takes the place of `Int` in `index :: Int -> [a] -> a`. You can use `FZ`
in any non-empty list, because `FZ :: Fin (S n)` will match any `Vec (S n)`
(which is necessarily of length greater than 0). You can use `FS FZ` only on
something that matches `Vec (S (S n))`. This is the type-safety.

We can also specify non-trivial relationships between lengths of lists, like
making a more type-safe `take :: Int -> [a] -> [a]`. We want to make sure that
the result list has a length less than or equal to the input list. We need
another "int" that can only be constructed in the case that the result length
is less than or equal to the first length. This called "proofs" or
"witnesses", and act in the same role as `TypeRep`, `(:~:)`, etc. did above for
our `Sigma` examples.

We want a type `LTE n m` that is a "witness" that `n` is less than or equal to
`m`. It can only be constructed for if `n` is less than or equal to `m`. For
example, you can create a value of type `LTE (S Z) (S (S Z))`, but not of `LTE
(S (S Z)) Z`

```haskell
!!!type-levels/Level5.hs "data LTE ::" "vtake ::"
```

Notice the similarity to how we would define `take :: Int -> [a] -> [a]`. We
just spiced up the `Int` argument with type safety.

Another thing we would like to do is use be able to *create* lists of
arbitrary length. We can look at `replicate :: Int -> a -> [a]`, and create a
new "spicy int" `SNat n`, so `vreplicate :: SNat n -> a -> Vec n a`

```haskell
!!!type-levels/Level5.hs "data SNat" "vreplicate ::"
```

Notice that this type has a lot more guarantees than `replicate`. For
`replicate :: Int -> a -> [a]`, we can't guarantee (as the caller) that the
return type does have the length we give it. But for `vreplicate :: SNat n -> a
-> Vec n a`, it does!

`SNat n` is actually kind of special. We call it a *singleton*, and it's useful
because it perfectly reflects the structure of `n` the type, as a
value...nothing more and nothing less. By pattern matching on `SNat n`, we can
exactly determine what `n` is. `SZ` means `n` is `Z`, `SS SZ` means `n` is `S
Z`, etc. This is useful because we can't directly pattern match on types at
runtime in Haskell (because of type erasure), but we *can* pattern match on
singletons at runtime.

We actually encountered singletons before in this post! `TypeRep a` is a
singleton for the type `a`: by pattern matching on it (like with `App`
earlier), we can essentially "pattern match" on the type `a` itself.

In practice, we often write typeclasses to automatically generate singletons,
similar to `Typeable` from before:

```haskell
!!!type-levels/Level5.hs "class KnownNat" "instance KnownNat Z" "instance KnownNat n => KnownNat (S n)" "vreplicate' ::"
```

One last thing: moving back and forth between the different levels. We can't
really write a `[a] -> Vec n a`, because in Haskell, the type variables are
determined by the *caller*. We want `n` to be determined by the list, and the
function itself. And now suddenly we run into the same issue that we ran into
before, when moving between levels 2 and 3.

We can do the same trick before and write an existential wrapper:

```haskell
!!!type-levels/Level5.hs "data SomeVec" "toSomeVec ::"
```

It is common practice (and a good habit) to always include a singleton (or a
singleton-like typeclass constraint) to the type you are "hiding" when you
create an existential type wrapper, even when it is not always necessary.
That's why we included `TypeRep` in `HList` and `SomeList` earlier.

`SomeVec a` is essentially isomorphic to `[a]`, except you can pattern match on
it and get the length `n` as a type you can use.

There's a slightly more light-weight method of returning an existential type:
by returning it in a continuation.

```haskell
!!!type-levels/Level5.hs "withVec ::"
```

That way, you can use the type variable within the continuation. Doing
`withSomeVec xs \n v -> ....` is identical to `case toSomeVec xs of SomeVec n v
-> ...`.

However, since you don't get the `n` itself until runtime, you might find
yourself struggling to use concepts like `Fin` and `LTE`. To do use them
comfortably, you have to write functions to "check" if your `LTE` is even
possible, known as "decision functions":

```haskell
!!!type-levels/Level5.hs "isLTE ::"
```

This was a very whirlwind introduction, and I definitely recommend reading
[this post on fixed-length lists][fixvec-2] for a more in-depth guide and tour
of the features.

Overall as you can see, at this level we gain some powerful guarantees and
tools, but we also run into some small inconveniences (like manipulating
witnesses and singletons). This level is fairly comfortable to work with in
modern Haskell tooling. However, if you live here long enough, you're going to
eventually be tempted to wander into...

Level 6: Local Structure Enforced List
--------------------------------------

*[Code available here][level6]*

!!![level6]:type-levels/Level6.hs

For our next level let's jump back back into constraints on the *contents* of
the list. Let's imagine a _priority queue_ on top of a list. Each value in the
list will be a `(priority, value)` pair. To make the `pop` operation (pop out
the value of lowest priority) efficient, we can enforce that the list is
_always sorted by priority_: the lowest priority is always first, the second
lowest is second, etc.

If we didn't care about type safety, we could do this by always inserting a new
item so that it is sorted:

```haskell
!!!type-levels/Level6.hs "insertSortedList ::"
```

This method enforces a *local* structure: between every item `x` and the
next item `y` in `x:y:zs`, the priority of `x` has to be less than the priority
`y`. Keeping our structure local means we only need to enforce local
invariants.

Writing it all willy nilly type unsafe like this could be good for a single
function, but we're also going to need some more complicated functions. What
if we wanted to "combine" (merge) two sorted lists together. Using a normal
list, we don't have any assurances that we have written it correctly, and it's
very easy to mess up. How about we leverage type safety to ask GHC to ensure
that our functions are always correct, and always preserve this local
structure? Now you're thinking in types!

Introducing level 6: enforcing local structure!

But, first, a quick note before we dive in: for the rest of this post, for the
sake of simplicity, let's switch from inductively defined types (like `Nat`
above) to GHC's built in [opaque `Nat` type][typenats]. You can think of it as
essentially the same as the `Nat` we wrote above, but *opaque* and provided by
the compiler. Under the hood, it's implemented using machine integers for
efficiency. And, instead of using concrete `S (S (S Z))` syntax, you'd use
abstract numeric literals, like `3`. There's a trade-off: because it's opaque,
we can't pattern match on it and create or manipulate our own witnesses -- we
are at the mercy of the API that GHC gives us. We get `+`, `<=`, `Min`, etc.,
but in total it's not that extensive. That's why I never use these without also
bringing typechecker plugins (*[ghc-typelits-natnormalise][]* and
*[ghc-typelits-knonwnnat][]*) to help automatically bring witnesses and
equalities and relationships into scope for us. Everything here could be done
using hand-defined witnesses and types, but we're using TypeNats here just for
the sake of example.

[typenats]: https://hackage.haskell.org/package/base/docs/GHC-TypeNats.html
[ghc-typelits-natnormalise]: https://hackage.haskell.org/package/ghc-typelits-natnormalise
[ghc-typelits-knonwnnat]: https://hackage.haskell.org/package/ghc-typelits-knownnat

```haskell
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
```

With that disclaimer out of the way, let's create our types! Let's make an
`Entry n a` type that represents a value of type `a` with priority `n`.

```haskell
!!!type-levels/Level6.hs "newtype Entry"
```

We'd construct this like `Entry @3 "hello"`, which produces `Entry 3 String`.
Again this uses *type application syntax*, `@3`, that lets us pass in the
*type* `3` to the constructor `Entry :: forall n a. a -> Entry n a`.

Now, let's think about what phantom types we want to include in our list. The
fundamental strategy in this, as I learned from [Conor McBride][]'s great
writings on this topic, are:

[Conor McBride]: http://strictlypositive.org/

* Think about what "type safe operations" you want to have for your structure
* Add just enough phantom types to perform those operations.

In our case, we want to be able to cons an `Entry n a` to the start of a sorted
list. To ensure this, we need to know that n is less than or equal to the
list's *current minimum priority*. So, we need our list type to be `Sorted n
a`, where `n` is the *current minimum priority*.

```haskell
!!!type-levels/Level6.hs "data Sorted"
```

To keep things simple, we are only going to talk about non-empty lists, so the
minimum priority is always defined.

So, a `Sorted n a` is either `SSingle (x :: Entry n a)`, where the single item
is a value of priority `n`, or `SCons x xs`, where `x` has priority `n` and
`xs :: Sorted m a`, where `n <= m`. In our previous inductive `Nat`, you could
imagine this as `SCons :: SNat m -> LTE n m -> Entry n a -> Sorted m a ->
Sorted n a`, but here we will use GHC's built-in `<=` typeclass-based witness
of less-than-or-equal-to-ness.

This works! You should be able to write:

```haskell
Entry @1 'a' `SCons` Entry @2 'b' `SCons` SSingle (Entry @4 'c')
```

This creates a valid list where the priorities are all sorted from lowest to
highest. You can now pop using pattern matching, which gives you the lowest
element *by construction*. If you match on `SCons x xs`, you *know* that no
entry in `xs` has a priority lower than `x`.

Critically, note that creating something out-of-order like the following would
be a compiler error:

```haskell
Entry @9 'a' `SCons` Entry @2 'b' `SCons` SSingle (Entry @4 'c')
```

Now, the *users* of our priority queue probably won't often care about having
the minimum priority in the type. In this case, we are using the phantom type
to ensure that our data structure is correct by construction, for our own sake,
and also to help us write internal functions in a correct way. So, for
practical end-user usage, we want to existentially wrap out `n`.

```haskell
!!!type-levels/Level6.hs "data SomeSorted" "popSomeSorted ::"
```

`popSomeSorted` takes an `Sorted n a` and returns the `Entry n a` promised at
the start of it, and then the rest of the list if there is anything left,
eliding the phantom parameter.

Now let's get to the interesting parts where we actually leverage `n`: let's
write `insertSortedList`, but the type-safe way!

First of all, what should the type be if we insert an `Entry n a` into a
`Sorted m a`? If `n <= m`, it would be `Sorted n a`. If `n > m`, it should be
`Sorted m a`. GHC gives us a type family `Min n m`, which returns the minimum
between `n` and `m`. So our type should be:

```haskell
insertSorted :: Entry n a -> Sorted m a -> Sorted (Min n m) a
```

To write this, we can use some helper functions: first, to decide *if* we
are in the `n <= m` or the `n > m` case:

```haskell
!!!type-levels/Level6.hs "data DecideInsert ::" "decideInsert ::"
```

We can use `decideInsert` to branch on if we are in the case where we insert
the entry at the head or the case where we have to insert it deeper.
`DecideInsert` here is our witness, and `decideInsert` constructs it using
`cmpNat`, provided by GHC to compare two `Nat`s. We use `Proxy :: Proxy n` to
tell it what nats we want to compare. `KnownNat` is the equivalent of our
`KnownNat` class we wrote from scratch, but with GHC's TypeNats instead of our
custom inductive Nats.

```haskell
cmpNat :: (KnownNat a, KnownNat b) => p a -> p b -> OrderingI a b

data OrderingI :: k -> k -> Type where
    LTI :: -- in this branch, a < b
    EQI :: -- in this branch, a ~ b
    GTI :: -- in this branch, a > b
```

Note that GHC and our typechecker plugins aren't smart enough to know we can
rule out `b > a` if `a > b` is true, so we have to leave an `error` that we
know will never be called. Oh well. If we were writing our witnesses by hand
using inductive types, we could write this ourselves, but since we are using
GHC's Nat, we are limited by what their API can prove.

Let's start writing our `insertSorted`:

```haskell
!!!type-levels/Level6.hs "insertSorted ::"
```

The structure is more or less the same as `insertSortedList`, but
now type safe! We basically use our handy helper function `decideInsert` to
dictate where we go. I also used a helper function `sConsMin` to insert into
the recursive case

```haskell
!!!type-levels/Level6.hs "sConsMin ::"
```

`sConsMin` isn't strictly necessary, but it saves a lot of unnecessary pattern
matching. The reason why we need it is because we *want* to write `SCons y
(insertSorted x ys)` in the last line of `insertSorted`. However, in this
case, `SCons` does not have a well-defined type. It can either be `Entry n ->
Sorted q a -> Sorted n a` or `Entry n -> Sorted r a -> Sorted n a`. Haskell
requires functions to be specialized at the place we actually *use* them, so
this is no good. We would have to pattern match on `cmpNat` and
`LTI`/`EQI`/`GTI` in order to know how to specialize `SCons`. So, we use
`sConsMin` to wrap this up for clarity.

How did I know this? I basically tried writing it out the full messy way,
bringing in as much witnesses and pattern matching as I could, until I got it
to compile. Then I spent time factoring out the common parts until I got what we
have now!

Note that we use a feature called "Type Abstractions" to "match on" the
existential type variable `q` in the pattern `SCons @q y ys`. Recall from the
definition of `SCons` that the first type variable is the minimum priority of the
tail.

And just like that, we made our `insertSortedList` *type-safe*! We can no
longer return an unsorted list: it always inserts sortedly, by *construction*,
enforced by GHC. We did cheat a little with `error`, that was only
because we used GHC's TypeNats...if we used our own inductive types, all
unsafety can be avoided.

Let's write the function to *merge* two sorted lists together. This is
essentially the merge step of a merge sort: take two lists, look at the head of
each one, cons the smaller of the two heads, then recurse.

```haskell
!!!type-levels/Level6.hs "mergeSorted ::"
```

Again, this looks a lot like how you would write the normal function to merge
two sorted lists...except this time, it's type-safe! You *can't* return an
unsorted list because the result list has to be sorted *by construction*.

To wrap it all up, let's write our conversion functions. First, an
`insertionSort` function that takes a normal non-empty list of priority-value
pairs and throws them all into a `Sorted`, which (by construction) is
guaranteed to be sorted:

```haskell
!!!type-levels/Level6.hs "insertionSort ::" "someSortedMin ::"
```

Some things to note:

1.  We're using the [nonempty list type][nonempty] type from *base*, because
    `Sorted` always has at least one element.
1.  We use `withSomeSNat` to turn a `Natural` into the type-level `n :: Nat`,
    the same way we wrote `withVec` earlier. This is just just the function
    that GHC offers to reify a `Natural` (non-negative `Integer`) to the type
    level.
2.  `someSortedMin` is used to clean up the implementation, doing the same job
    that `sConsMin` did.

[nonempty]: https://hackage.haskell.org/package/base/docs/Data-List-NonEmpty.html

```haskell
ghci> case insertionSort ((4, 'a') :| [(3, 'b'), (5, 'c'), (4, 'd')]) of
          SomeSorted xs -> print xs
SCons Entry @3 'b' (SCons Entry @4 'd' (SCons Entry @4 'a' (SSingle Entry @5 'c')))
```

Finally, a function to convert back down to a normal non-empty list, using
GHC's `natVal` to "demote" a type-level `n :: Nat` to a `Natural`

```haskell
!!!type-levels/Level6.hs "fromSorted ::"
```

Level 7: Global structure Enforced List
---------------------------------------

*[Code available here][level7]*

!!![level7]:type-levels/Level7.hs

For our final level, let's imagine a "weighted list" of `(Int, a)` pairs, where
each item `a` has an associated weight or cost. Then, imagine a "bounded
weighted list", where the _total cost_ must not exceed some limit value. Think
of it as a list of files and their sizes and a maximum total file size, or a
backpack for a character in a video game with a maximum total carrying weight.

There is a fundamental difference here between this type and our last type: we
want to enforce a *global* invariant (total cannot exceed a limit), and we
can't "fake" this using local invariants like last time.

Introducing level 7: enforcing *global* structure! This brings some extra
complexities, similar to the ones we encountered in Level 5 with our
fixed-length lists: whatever phantom type we use to enforce this "global"
invariant now becomes entangled to the overall structure of our data type
itself.

Let's re-use our `Entry` type, but interpret an `Entry n a` as a value of type
`a` with a weight `n`. Now, we'll again "let McBride be our guide" and ask
the same question we asked before: what "type-safe" operation do we want, and
what minimal phantom types do we need to allow this type-safe operation? In our
case, we want to *insert* into our bounded weighted list in a safe way, to
ensure that there is enough room. So, we need *two* phantom types:

1. One phantom type `lim` to establish the maximum weight of our container
2. Another phantom type `n` to establish the current used capacity of our
   container.

We want `Bounded lim n a`:

```haskell
!!!type-levels/Level7.hs "data Bounded"
```

*   The empty bounded container `BNil :: lim 0 a` can satisfy *any* `lim`, and
    has weight 0.
*   If we have a `Bounded lim m a`, then we can add an `Entry n a` to get a
    `Bounded lim (m + n) a` provided that `m + n <= lim` using `BCons`.

Let's try this out by seeing how the end user would "maybe insert" into a
bounded list of it had enough capacity:

```haskell
!!!type-levels/Level7.hs "data SomeBounded" "insertSomeBounded ::"
```

First we match on the `SomeBounded` to see what the current capacity `m` is.
Then we check using `cmpNat` to see if the `Bounded` can hold `m + n`. If it
does, we can return successfully. Note that we define `SomeBounded` using GADT
syntax so we can precisely control the order of the type variables, so
`SomeBounded @m xs` binds `m` to the capacity of the inner list.

Remember in this case that the *end user* here isn't necessarily using the
phantom types to their advantage (except for `lim`, which could be useful).
Instead, it's *us* who is going to be using `n` to ensure that if we ever
*create* any `Bounded` (or `SomeBounded`), it will *always* be within capacity
*by construction*.

Now that the usage makes sense, let's jump in and write some type-safe
functions using our fancy phantom types!

First, let's notice that we can always "resize" our `Bounded lim n a` to a
`Bounded lim' n a` as long as the total usage `n` fits within the new carrying
capacity:

```haskell
!!!type-levels/Level7.hs "reBounded ::"
```

Note that we have full type safety here! GHC will prevent us from using
`reBounded` if we pick a new `lim` that is *less* than what the bag currently
weighs!  You'll also see the general pattern here that changing any "global"
properties for our type here will require recursing over the entire structure
to adjust the global property.

How about a function to combine two bags of the same weight? Well, this should
be legal as long as the new combined weight is still within the
limit:

```haskell
!!!type-levels/Level7.hs "concatBounded ::"
```

::::: {.note}
**Aside**

This is completely unrelated to the topic at hand, but if you're a big nerd
like me, you might enjoy the fact that this function makes `Bounded lim n a`
the *arrows* of a [Category][] whose *objects* are the natural numbers, the
identity arrow is `BNil`, and arrow composition is `concatBounded`. Between
object `n` and `m`, if `n <= m`, its arrows are values of type `Bounded lim (m
- n) a`. Actually wait, it's the same thing with `Vec` and `vconcat` above
isn't it? I guess we were moving so fast that I didn't have time to realize it.

Anyway this is related to the [preorder category][], but not thin. A thicc
preorder category, if you will. Always nice to spot a category out there in
the wild.

:::::

[Category]: https://ncatlab.org/nlab/show/category
[preorder category]: https://ncatlab.org/nlab/show/preorder

It should be noted that the reason that `reBounded` and `concatBounded` look so
clean so fresh is that we are heavily leveraging typechecker plugins. But,
these are all still possible with normal functions if we construct the
witnesses explicitly.

Now for a function within our business logic, let's write `takeBounded`, which
*constricts* a `Bounded lim n a` to a `Bounded lim' q a` with a smaller limit
`lim'`, where `q` is the weight of *all of the elements that fit in the new
limit*. For example, if we had a bag of limit 15 containing items weighing 4,
3, and 5 (total 12), but we wanted to `takeBounded` with a new limit 10, we
would take the 4 and 3 items, but leave behind the 5 item, to get a new total
weight of 7.

It'd be nice to have a helper data type to existentially wrap the new `q`
weight in our return type:

```haskell
!!!type-levels/Level7.hs "data TakeBounded"
```

So the type of `takeBounded` would be:

```haskell
takeBounded ::
  (KnownNat lim, KnownNat lim', KnownNat n) =>
  Bounded lim n a ->
  TakeBounded lim' n a
```

Again I'm going to introduce some helper functions that will make sense soon:

```haskell
!!!type-levels/Level7.hs "bConsExpand ::" "withBoundedWit ::"
```

From the type, we can see `bCons` adds a new item while also increasing the
limit: `bConsExpand :: Entry n a -> Bounded lim m a -> Bounded (n + lim) (n +
m) a`. This is always safe conceptually because we can always add a new item
into any bag if we increase the limit of the bag: `Entry 100 a -> Bounded 5 3 a
-> Bounded 105 103 a`, for instance.

Next, you'll notice that if we write this as `BCons x (reBounded xs)` alone,
we'll get a GHC error complaining that this requires `m <= lim`. This is
something that we *know* has to be true (by construction), since there isn't
any constructor of `Bounded` that will give us a total weight `m` bigger than
the limit `lim`. However, this requires a bit of witness manipulation for GHC
to *know* this: we have to essentially enumerate over every constructor, and
within each constructor GHC knows that `m <= lim` holds. This is what
`withBoundedWit` does. We "know" `n <= lim`, we just need to enumerate over
the constructors of `Bounded lim n a` so GHC is happy in every case.

`withBoundedWit`'s type might be a little confusing if this is the first time
you've seen an argument of the form `(constraint => r)`: it takes a `Bounded
lim n a` and a "value that is only possible if `n <= lim`", and then gives you
that value.

With that, we're ready:

```haskell
!!!type-levels/Level7.hs "takeBounded ::"
```

Thanks to the types, we ensure that the returned bag must contain *at most*
`lim'`!

As an exercise, try writing `splitBounded`, which is like `takeBounded` but
also returns the items that were leftover. [Solution here.][splitBounded]

```haskell
!!!type-levels/Level7.hs "data SplitBounded ::" "splitBounded ::"5
```

!!![splitBounded]:type-levels/Level7.hs "splitBounded ::"

One final example, how about a function that *reverses* the `Bounded lim n a`?
We're going to write a "single-pass reverse", similar to how it's often written
for lists:

```haskell
!!!type-levels/Level7.hs "reverseList ::"
```

Now, reversing a `Bounded` should be legal, because reversing the order of the
items shouldn't change the total weight. However, we basically "invert" the
structure of the `Bounded` type, which, depending on how we set up our phantom
types, could mean a lot of witness reshuffling. Luckily, our typechecker
plugin handles most of it for us in this case, but it exposes one gap:

```haskell
!!!type-levels/Level7.hs "reverseBounded ::" "solveLte ::"
```

Due to how everything gets exposed, we need to prove that if `a + b <= n` and
`c <= b`, then `a + c <= n`. This is always true, but the typechecker plugin
needs a bit of help, and we have to resort to an unsafe operation to get this
to work. However, if we were using our manually constructed inductive types
instead of GHC's opaque ones, we could write this in a type-safe and total way.
We run into these kinds of issues a lot more often with global invariants than
we do with local invariants, because the invariant phantom becomes so entangled
with the structure of our data type.

And...that's about as far as we're going to go with this final level! If this
type of programming with structural invariants is appealing to you, check out
Conor McBride's famous [type-safe red-black trees in Haskell][pivotal] paper,
or Edwin Brady's [Type-Driven Development in Idris][tddi] for how to structure
entire programs around these principles.

[pivotal]: https://personal.cis.strath.ac.uk/conor.mcbride/Pivotal.pdf
[tddi]: https://www.manning.com/books/type-driven-development-with-idris

Evident from the fact that Conor's work is in Agda, and Brady's in Idris, you
can tell that in doing this, we are definitely pushing the boundaries of what
is ergonomic to write in Haskell. Well, depending on who you ask, we already
zipped that boundary long ago. Still, there's definitely a certain kind of joy
to defining invariants in your data types and then essentially *proving* to the
compiler that you've followed them. But, most people will be happier just
writing a property test to fuzz the implementation on a non type-safe
structure. And some will be happy with...unit tests. Ha ha ha ha. Good joke
right?

Anyway, hope you enjoyed the ride! I hope you found some new ideas for ways to
write your code in the future, or at least found them interesting or
eye-opening. Which level is your favorite, and what level do you *wish* you
could work at if things got a little more ergonomic?

Special Thanks
--------------

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts. Very special
thanks to my supporter at the "Amazing" level on [patreon][], Josh Vera! :)

[patreon]: https://www.patreon.com/justinle/overview
