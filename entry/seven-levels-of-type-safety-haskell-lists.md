Seven Levels of Type Safety in Haskell: Lists

==============================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/seven-levels-of-type-safety-haskell-lists.html)

One thing I always appreciate about Haskell is that you can often choose the
level of type-safety you want to work at. Haskell offers tools to be able to
work at *both* extremes (which are often undesirable except in extraordinary
circumstances), whereas most languages only offer some limited part of the
spectrum. No promises about the ergonomics of either extreme, though --- just
that they are possible! Of course, often times Haskellers get a reputation of
always dialing it up to extreme safety just because they can, and not because
it's a good idea :) To be fair, I'm not going to claim that I'm immune to this
effect; something that has helped me, though, is being consciously aware of the
different levels of type safety that are available, and the
benefits/drawbacks/unique advantages to each.

So, here is a rundown of seven "levels" of type safety that you can operate at
when working with the ubiquitous *strict* linked list data type, and how to use
them! I genuinely believe all of these are useful (or useless) in their own
different circumstances. When possible, we'll prefer "structural type safety"
(that is, enforced by the structure of the type itself) as opposed to
[restricted constructors](https://hackage.haskell.org/package/refined) (just to
allow us to make more interesting comparisons and contrasts) and also more
flexible/parameterized options for type safety instead of hard-coded
restrictions.

One interesting thing you might notice is the "bowl-shaped"
difficulty/ergonomics curve. At the unsafe extreme, usage can be a bit of a
hassle due to the nature of the language, at the safest extremes...well, you'll
see soon enough!

This post is written for a late beginner or intermediate Haskeller, who is
already familiar with ADTs and defining their own custom list type like
`data List a = Nil | Cons a (List a)`.

## Level 0: Could be anything

Let's start off with a baseline type that demonstrates a little bit of the
quirks of working at the unsafe extremes in Haskell.

A reasonable idea might be to just make a "black hole" data type that could be
anything:

``` haskell
data Any :: Type where
  MkAny :: a -> Any
```

(This data type declaration written using [GADT
Syntax](https://typeclasses.com/ghc/gadt-syntax), and the name was chosen
because it resembles [the Any type in
base](https://hackage.haskell.org/package/base-4.14.1.0/docs/GHC-Exts.html#t:Any))

So you can have values `MkAny 8 :: Any`, `MkAny True :: Any`,
`MkAny [1,2,3] :: Any`, etc. A value of any type can be given to `MkAny`, and
the resulting type will have type `Any`.

However, this type is a black hole; you can't really do anything with the values
inside it because of parametric polymorphism: you must treat any value inside it
in a way that is compatible with a value of *any* type. But there aren't *too*
many useful things you can do with something in a way that is compatible with a
value of any type (things like, `id :: a -> a`, `const 3 :: a -> Int`). In the
end, it's essentially isomorphic to unit `()`.

So we can actually modify this slightly to match the typical dynamically typed
language picture of "could be anything": we should at least be able to query the
type for things we can do with it.

To get there, we can instead allow some sort of witness on the type of the
value:

``` haskell
data Sigma :: (Type -> Type) -> Type where
    MkSigma :: p a -> a -> Sigma p
```

And the most classic witness is
[`TypeRep`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Type-Reflection.html#t:TypeRep)
from *base*, which is a witness that lets you "match" on the type.

``` haskell
showIfBool :: Sigma TypeRep -> String
showIfBool (Sigma tr x) = case testEquality tr (typeRep @Bool) of
    Just Refl -> case x of      -- in this branch, we know x is a Bool
      False -> "False"
      True  -> "True"
    Nothing -> "Not a Bool"
```

Here we can use `TypeRep`'s interface to "match" (using `testEquality`) on if
the value inside is a `Bool`. If the match works (and we get `Just Refl`) then
we can treat `x` as a `Bool` in that case. If it doesn't (and we get `Nothing`),
then we do what we would want to do otherwise.

``` haskell
ghci> let x = Sigma typeRep True
ghci> let y = Sigma typeRep 4
ghci> showIfBool x
"True"
ghci> showIfBool y
"Not a Bool"
```

This pattern is common enough that there's the
*[Data.Dynamic](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Dynamic.html)*
module in base that is `Sigma TypeRep`, and wraps our `testEquality` dance above
in a function called `fromDynamic`:

``` haskell
showIfBool :: Dynamic -> String
showIfBool dyn = case fromDynamic dyn of
    Just x -> case x of      -- in this branch, we know x is a Bool
      False -> "False"
      True  -> "True"
    Nothing -> "Not a Bool"
```

For make our life easier in the future, let's write a version of `fromDynamic`
for our `Sigma TypeRep`:

``` haskell
-- Typeable constraint is necessary to use typeRep @a
castSigma :: forall a. Typeable a => Sigma TypeRep -> Maybe a
castSigma (Sigma tr x) = case testEquality tr (typeRep @a) of
    Just Refl -> Just x
    Nothing   -> Nothing
```

But the reason why I'm presenting the more generic `Sigma` instead of the
specific `type Dynamic = Sigma TypeRep` is that you can swap out `TypeRep` to
get other interesting types. For example, if you had a witness of showability:

``` haskell
data Showable :: Type -> Type where
    WitShowable :: Show a => Showable a
```

(This type is related to `Dict Show` from the
[constraints](https://hackage.haskell.org/package/constraints-0.13/docs/Data-Constraint.html#t:Dict)
library; it's technically `Compose Dict Show`)

And now we have a type `Sigma Showable` that's a bit of a black hole, but we can
at least use `show` on it:

``` haskell
showSigma :: Sigma Showable -> String
showSigma (Sigma WitShowable x) = show x       -- here, we know x is Show
```

``` haskell
ghci> let x = MkSigma WitShowable True
ghci> let y = MkSigma WitShowable 4
ghci> showSigma x
"True"
ghci> showSigma y
"4"
```

This is the "[existential typeclass
antipattern](https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/)",
but since we are talking about different ways we can tease the type system, it's
probably worth mentioning! Note that `Show` is a bit silly of a typeclass to use
in this context because a `Sigma Showable` is equivalent to just a `String`:
once you match on the constructor to get the value, the only thing you can do
with the value is `show` it anyway.

One fun thing we can do is provide a "useless witness", like `Proxy`:

``` haskell
data Proxy a = Proxy
```

So a value like `MkSigma Proxy True :: Sigma Proxy` is truly a useless data
type, since we know that `MkSigma` contrains *some* value of *some* type, but
there's no witness to give us any clue on how we can use it. A `Sigma Proxy` is
isomorphic to `()`.

On the other extreme, we can use a witness to constrain the value to only be a
specific type, like `IsBool`:

``` haskell
data IsBool :: Type -> Type where
    ItsABool :: IsBool Bool
```

So you can have a value of type `MkSigma ItsABool True :: Sigma IsBool`, or
`MkSigma ItsABool False`, but `MkSigma ItsABool 2` will not typecheck ---
remember, to make a `Sigma`, you need a `p a` and an `a`.
`ItsABool :: IsBool Bool`, so the `a` you put in must be `Bool` to match.
`Sigma IsBool` is isomorphic to `Bool`.

There's a general version of this too, `(:~:) a` (from
*[Data.Type.Equality](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Type-Equality.html#t::-126-:)*
in base). `(:~:) Bool` is our `IsBool` earlier. `Sigma ((:~:) a)` is essentially
exactly `a`...basically bringing us incidentally back to complete type safety?
Weird. Anyway.

I think one interesting thing to see here is that being "type-unsafe" in Haskell
can be much less convenient than doing something similar in a dynamically typed
language like python --- that's why I don't fancy those memes/jokes about how
dynamically typed languages are just "static types with a single type". The
actual way you use those types lend themselves to different ergonomics, and the
reductionist take doesn't quite capture that nuance.

## Level 1: Heterogeneous List

The lowest level of safety in which a list might be useful is the heterogeneous
list. This is the level where lists (or "arrays") live in most dynamic
languages.

``` haskell
data Sigma :: (Type -> Type) -> Type where
    MkSigma :: p a -> a -> Sigma p

-- we can define this in terms of Haskell's built-in list
type HList p = [Sigma p]
```

We tag values with a witness `p` for the same reason as before: if we don't
provide *some* type of witness, our type is useless.

The "heterogeneous list of values of any type" is `HList TypeRep`. This is
usable in the implicit multiple-argument nature of functions in languages like
javascript. For example, here's a function that connects to a host (`String`),
optionally taking a port (`Int`) and a method (`Method`).

``` haskell
data Method = HTTP | HTTPS

indexMaybe :: Int -> HList p -> Maybe (Sigma p)
indexMaybe 0 []     = Nothing
indexMaybe 0 (x:_ ) = Just x
indexMaybe n (_:xs) = indexMaybe (n-1) xs

-- | Expects a String, an Int, then a Method.
mkConnection :: HList TypeRep -> IO ()
mkConnection args = ...
  where
    host :: Maybe String
    host = castSigma =<< indexHList 0 args
    port :: Maybe Int
    port = castSigma =<< indexHList 1 args
    method :: Maybe Method
    method = castSigma =<< indexHList 2 args
```

Of course, this would *probably* be better expressed in Haskell as a function of
type `Maybe String -> Maybe Int -> Maybe Method -> IO ()`. But maybe this could
be useful in a situation where you would want to offer the ability to take
arguments in any order? We could "find" the first value of a given type:

``` haskell
findValueOfType :: Typeable a => HList TypeRep -> Maybe a
findValueOfType []     = Nothing
findValueOfType (x:xs) = castSigma x <|> findValueOfType xs
```

Then we could write:

``` haskell
-- | Expects a String, an Int, then a Method, in any order.
mkConnection :: HList TypeRep -> IO ()
mkConnection args = ...
  where
    host :: Maybe String
    host = findValueOfType args
    port :: Maybe Int
    port = findValueOfType args
    method :: Maybe Method
    method = findValueOfType args
```

But is this a good idea? Probably not.

One very common usage of this type is for "extensible" systems that let you
store components of different types in a container, as long as they all support
some common interface (ie, the widgets system from the [Luke
Palmer](https://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/)
post).

For example, we could have a list of any item as long as the item is an instance
of `Show`: that's `HList Showable`!

``` haskell
showAll :: HList Showable -> [String]
showAll = map showSigma
  where
    showSigma (MkSigma WitShowable x) = show x
```

``` haskell
ghci> let xs = [MkSigma WitShowable 1, MkSigma WitShowable True]
ghci> showAll xs
["1", "True"]
```

For `Show`, this is a rather silly thing to do because of the reasons mentioned
in the Palmer post --- namely, that you could always just have a `[String]`
directly instead of an `HList Showable`, since the only thing you can do with a
`Sigma Showable` is `show` it.

For fun, let's imagine some other things we could fill in for `p`. If we use
`HList Proxy`, then we basically don't have any witness at all. We can't use the
values in the list in any meaningful way; `HList Proxy` is essentially the same
as `Natural`, since the only information is the length.

If we use `HList IsBool`, we basically have `[Bool]`, since every item must be a
`Bool`! In general, `HList ((:~:) a)` is the same as `[a]`.

## Level 2: Homogeneous Dynamic List

A next level of type safety we can add is to ensure that all elements in the
list are of the same type. This adds a layer of usefulness because there are a
lot of things we might want to do with the elements of a list that are only
possible if they are all of the same type --- for example, we might want to take
a (polymorphic) sum of a list, but we can't do that with `HList` because it
might mix values of different types.

To do this, we can flip the witness to *outside* the list:

``` haskell
data SomeList :: (Type -> Type) -> Type where
    MkSomeList :: p a -> [a] -> SomeList p
```

We can write some meaningful predicates on this list --- for example, we can
check if it is monotonic (the items increase in order)

``` haskell
-- | An Ord counterpart for Showable
data Comparable :: Type -> Type where
    WitOrd :: Ord a => Comparable a

monotonic :: SomeList Comparable -> Bool
monotonic (MkSomeList WitOrd [])       = True
monotonic (MkSomeList WitOrd [x])      = True
monotonic (MkSomeList WitOrd (x:y:xs)) =
    (x <= y) && monotonic (MkSomeList WitOrd (y:xs))
```

However, if we take a step back, we can see that this is a little silly because
we could just as easily write `monotonic` as a polymorphic function:

``` haskell
monotonic' :: Ord a => [a] -> Bool
monotonic' []       = True
monotonic' [x]      = True
monotonic' (x:y:xs) = (x <= y) && monotonic' (y:xs)
```

Instead, the we could maybe have a function return *any* type of `Ord`-instance
list, depending on what it was given.

``` haskell
getItems :: IO (SomeList Comparable)
getItems = ...
```

This is a "dependently typed" function, in that it could go into a database (or
query the user) for a list of items of a type that will not be known until
runtime --- the only guarantee we have is that it has an `Ord` instance.

``` haskell
analyzeGottenItems :: IO Bool
analyzeGottenItems = do
    items@(MkSomeList WitOrd xs) <- getItems
    putStrLn $ "Got " ++ show (length xs) ++ " items."
    let isMono    = monotonic items
        isRevMono = monotnoic (MkSomeList WitOrd (reverse xs))
    when isMono $
      putStrLn "The items are monotonic."
    when (isMono && isRevMono) $ do
      putStrLn "The items are monotonic both directions."
      putStrLn "This means the items are all identical."
```

This could also be useful if you have a few "valid" options for your list types,
and want to branch depending on which one you get. This can be useful for just
using `Either` if you expect your list of valid types to be "open" and
constantly expanding, and so your documentation is your main way of indicating
to the user what they can give. I'm just kidding, this is not useful, please use
a real closed ADT if you ever want to do this.

``` haskell
-- | Behavior depends on what is given.
--
-- * If it's a list of Bools, returns if they are all True
-- * If it's a list of Ints, returns if their sum is greater than 50
-- * If it's a list of Doubles, returns if their sum is greater than 5.0
-- * If it's a list of Strings, returns if it contains an 'e'
processList :: MkSomeList TypeRep -> Bool
processList (MkSomeList tr xs)
    | Just Refl <- testEquality tr (typeRep @Bool)   = and xs
    | Just Refl <- testEquality tr (TypeRep @Int)    = sum xs > 50
    | Just Refl <- testEquality tr (TypeRep @Double) = sum xs > 5.0
    | Just Refl <- testEquality tr (TypeRep @String) = 'e' `elem` xs
```

This pattern is overall similar to how lists are often used in dynamic
languages: just *some* list of the same type of each element is expected, but
what is *in* those lists is dynamically determined. Ultimately I think Level 1
more closely matches lists in dynamic languages than Level 2, if only because
the idea of a list of all elements of the same type is such a half-hearted sort
of type safety that neither dynamic languages nor statically typed languages
would ever embrace it --- it's a "worst of both worlds" in many ways.

For all of these levels, we'll take a moment to describe how to "ascend to" the
new level, and how to "descend from" the new level.

Moving from Level 1 to Level 2 is actually impossible in the general case; we
can only do for `HList TypeRep` and `SomeList TypeRep`:

``` haskell
hlistToSomeList :: HList TypeRep -> Maybe (SomeList TypeRep)
hlistToSomeList (HList [])     = Just $ MkSomeList (typeRep @()) []  -- this could be anything
hlistToSomeList (HList (x:xs)) = 
```

## Level 3: Homogeneous Typed List

At the halfway point, we've reached Haskell's ubiquitous list type! It is
essentially:

``` haskell
data List :: Type -> Type where
    Nil  :: List a
    Cons :: a -> List a -> List a
```

I don't have too much to say here, other than to acknowledge that this is truly
a "sweet spot" in terms of safety vs. unsafety and usability. This simple
`List a` / `[a]` type has so many benefits from type-safety:

-   It lets us write functions that can meaningfully say that the input and
    result types are the same, like `take :: Int -> [a] -> [a]`
-   It lets us write functions that can meaningfully link lists and the items in
    the list, like `head :: [a] -> a` and `replicate :: Int -> a -> [a]`.
-   It lets us write functions that can meaningfully state relationships between
    input and results, like `map :: (a -> b) -> [a] -> [b]`
-   We can require two input lists to have the same type of items, like
    `(++)     :: [a] -> [a] -> [a]`
-   We can express complex relationships between inputs and outputs, like
    `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`.

The property of being able to state and express relationships between the values
of input lists and output lists and the items in those lists is extremely
powerful, and also extremely ergonomic to use in Haskell. It can be argued that
Haskell, as a language, was tuned explicitly to be used with the least friction
at this exact level of type safety.

--------------------------------------------------------------------------------

Hi, thanks for reading! You can reach me via email at <justin@jle.im>, or at
twitter at [\@mstk](https://twitter.com/mstk)! This post and all others are
published under the [CC-BY-NC-ND
3.0](https://creativecommons.org/licenses/by-nc-nd/3.0/) license. Corrections
and edits via pull request are welcome and encouraged at [the source
repository](https://github.com/mstksg/inCode).

If you feel inclined, or this post was particularly helpful for you, why not
consider [supporting me on Patreon](https://www.patreon.com/justinle/overview),
or a [BTC donation](bitcoin:3D7rmAYgbDnp4gp4rf22THsGt74fNucPDU)? :)

