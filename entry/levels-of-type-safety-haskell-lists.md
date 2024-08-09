Seven Levels of Type Safety in Haskell: Lists

==============================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/levels-of-type-safety-haskell-lists.html)

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
different circumstances.

This post is written for a late beginner or intermediate Haskeller, who is
already familiar with ADTs and defining their own custom list type like
`data List a = Nil | Cons a (List a)`. But, be advised that the techniques
discussed in this post are considered esoteric at best and harmful at worst for
most actual real-world applications. Inside every Haskeller there are two
wolves.

## Level 1: Could be anything

Let's start off with a baseline type that demonstrates a little bit of the
quirks of working at the unsafe extremes in Haskell.

A reasonable idea might be to just make a "black hole" data type that could be
anything:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level1.hs#L12-L13

data Any :: Type where
  MkAny :: a -> Any
```

(This data type declaration written using [GADT
Syntax](https://typeclasses.com/ghc/gadt-syntax), and the name was chosen
because it resembles [the Any type in
base](https://hackage.haskell.org/package/base-4.14.1.0/docs/GHC-Exts.html#t:Any))

So you can have values:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level1.hs#L15-L22

anyInt :: Any
anyInt = MkAny (8 :: Int)

anyBool :: Any
anyBool = MkAny True

anyList :: Any
anyList = MkAny ([1, 2, 3] :: [Int])
```

A value of any type can be given to `MkAny`, and the resulting type will have
type `Any`.

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
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level1.hs#L24-L25

data Sigma :: (Type -> Type) -> Type where
  MkSigma :: p a -> a -> Sigma p
```

And the most classic witness is
[`TypeRep`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Type-Reflection.html#t:TypeRep)
from *base*, which is a witness that lets you "match" on the type.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level1.hs#L27-L32

showIfBool :: Sigma TypeRep -> String
showIfBool (MkSigma tr x) = case testEquality tr (typeRep @Bool) of
  Just Refl -> case x of -- in this branch, we know x is a Bool
    False -> "False"
    True -> "True"
  Nothing -> "Not a Bool"
```

Here we can use `TypeRep`'s interface to "match" (using `testEquality`) on if
the value inside is a `Bool`. If the match works (and we get `Just Refl`) then
we can treat `x` as a `Bool` in that case. If it doesn't (and we get `Nothing`),
then we do what we would want to do otherwise.

``` haskell
ghci> let x = MkSigma typeRep True
ghci> let y = MkSigma typeRep (4 :: Int)
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
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level1.hs#L40-L45

showIfBoolDynamic :: Dynamic -> String
showIfBoolDynamic dyn = case fromDynamic dyn of
  Just x -> case x of -- in this branch, we know x is a Bool
    False -> "False"
    True -> "True"
  Nothing -> "Not a Bool"
```

For make our life easier in the future, let's write a version of `fromDynamic`
for our `Sigma TypeRep`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level1.hs#L47-L53

castSigma :: TypeRep a -> Sigma TypeRep -> Maybe a
castSigma tr (MkSigma tr' x) = case testEquality tr tr' of
  Just Refl -> Just x
  Nothing -> Nothing

castSigma' :: Typeable a => Sigma TypeRep -> Maybe a
castSigma' = castSigma typeRep
```

But the reason why I'm presenting the more generic `Sigma` instead of the
specific `type Dynamic = Sigma TypeRep` is that you can swap out `TypeRep` to
get other interesting types. For example, if you had a witness of showability:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level1.hs#L55-L62

data Showable :: Type -> Type where
  WitShowable :: Show a => Showable a

showableInt :: Sigma Showable
showableInt = MkSigma WitShowable (3 :: Int)

showableBool :: Sigma Showable
showableBool = MkSigma WitShowable True
```

(This type is related to `Dict Show` from the
[constraints](https://hackage.haskell.org/package/constraints-0.13/docs/Data-Constraint.html#t:Dict)
library; it's technically `Compose Dict Show`)

And now we have a type `Sigma Showable` that's a bit of a black hole, but we can
at least use `show` on it:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level1.hs#L64-L65

showSigma :: Sigma Showable -> String
showSigma (MkSigma WitShowable x) = show x -- here, we know x is Show
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
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level1.hs#L67-L70

data Proxy a = Proxy

uselessBool :: Sigma Proxy
uselessBool = MkSigma Proxy True
```

So a value like `MkSigma Proxy True :: Sigma Proxy` is truly a useless data type
(basically our `Any` from before), since we know that `MkSigma` constrains
*some* value of *some* type, but there's no witness to give us any clue on how
we can use it. A `Sigma Proxy` is isomorphic to `()`.

On the other extreme, we can use a witness to constrain the value to only be a
specific type, like `IsBool`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level1.hs#L72-L76

data IsBool :: Type -> Type where
  ItsABool :: IsBool Bool

justABool :: Sigma IsBool
justABool = MkSigma ItsABool False
```

So you can have a value of type `MkSigma ItsABool True :: Sigma IsBool`, or
`MkSigma ItsABool False`, but `MkSigma ItsABool 2` will not typecheck ---
remember, to make a `Sigma`, you need a `p a` and an `a`.
`ItsABool :: IsBool Bool`, so the `a` you put in must be `Bool` to match.
`Sigma IsBool` is essentially isomorphic to `Bool`.

There's a general version of this too, `(:~:) a` (from
*[Data.Type.Equality](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Type-Equality.html#t::-126-:)*
in base). `(:~:) Bool` is our `IsBool` earlier. `Sigma ((:~:) a)` is essentially
exactly `a`...basically bringing us incidentally back to complete type safety?
Weird. Anyway.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level1.hs#L78-L79

justAnInt :: Sigma ((:~:) Int)
justAnInt = MkSigma Refl 10
```

I think one interesting thing to see here is that being "type-unsafe" in Haskell
can be much less convenient than doing something similar in a dynamically typed
language like python --- that's why I don't fancy those memes/jokes about how
dynamically typed languages are just "static types with a single type". The
actual way you use those types lend themselves to different ergonomics, and the
reductionist take doesn't quite capture that nuance.

## Level 2: Heterogeneous List

The lowest level of safety in which a list might be useful is the dynamically
heterogeneous list. This is the level where lists (or "arrays") live in most
dynamic languages.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level2.hs#L12-L12

type HList p = [Sigma p]
```

We tag values with a witness `p` for the same reason as before: if we don't
provide *some* type of witness, our type is useless.

The "dynamically heterogeneous list of values of any type" is `HList TypeRep`.
This is somewhat similar to how functions with positional arguments work in a
dynamic language like javascript. For example, here's a function that connects
to a host (`String`), optionally taking a port (`Int`) and a method (`Method`).

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level2.hs#L14-L30

data Method = HTTP | HTTPS

indexHList :: Int -> HList p -> Maybe (Sigma p)
indexHList 0 [] = Nothing
indexHList 0 (x : _) = Just x
indexHList n (_ : xs) = indexHList (n - 1) xs

mkConnection :: HList TypeRep -> IO ()
mkConnection args = pure () -- something with host, port, and method
  where
    host :: Maybe String
    host = castSigma' =<< indexHList 0 args
    port :: Maybe Int
    port = castSigma' =<< indexHList 1 args
    method :: Maybe Method
    method = castSigma' =<< indexHList 2 args
```

Of course, this would *probably* be better expressed in Haskell as a function of
type `Maybe String -> Maybe Int -> Maybe Method -> IO ()`. But maybe this could
be useful in a situation where you would want to offer the ability to take
arguments in any order? We could "find" the first value of a given type:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level2.hs#L32-L33

findValueOfType :: Typeable a => HList TypeRep -> Maybe a
findValueOfType = listToMaybe . mapMaybe castSigma'
```

Then we could write:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level2.hs#L36-L44

mkConnectionAnyOrder :: HList TypeRep -> IO ()
mkConnectionAnyOrder args = pure ()
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
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level2.hs#L49-L52

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

For `Show`, this is, again, a rather silly thing to do because of the reasons
mentioned in the Palmer post --- namely, that you could always just have a
`[String]` directly instead of an `HList Showable`, since the only thing you can
do with a `Sigma Showable` is `show` it.

For fun, let's imagine some other things we could fill in for `p`. If we use
`HList Proxy`, then we basically don't have any witness at all. We can't use the
values in the list in any meaningful way; `HList Proxy` is essentially the same
as `Natural`, since the only information is the length.

If we use `HList IsBool`, we basically have `[Bool]`, since every item must be a
`Bool`! In general, `HList ((:~:) a)` is the same as `[a]`.

## Level 3: Homogeneous Dynamic List

A next level of type safety we can add is to ensure that all elements in the
list are of the same type. This adds a layer of usefulness because there are a
lot of things we might want to do with the elements of a list that are only
possible if they are all of the same type.

First of all, let's clarify a subtle point here. It's very easy in Haskell to
*consume* lists where all elements are of the same (but not necessarily known)
type. Functions like `sum :: Num a => [a] -> a` and
`sort :: Ord a => [a] -> [a]` do that. This is polymorphism, where the function
is written to not worry about the type, and the ultimate *caller* of the
function must pick the type they want to use with it. For the sake of this
discussion, we aren't talking about *consuming* values -- we're talking about
*producing* and *storing* values where the *producer* (and not the consumer)
controls the type variable.

To do this, we can flip the witness to *outside* the list:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level3.hs#L17-L18

data SomeList :: (Type -> Type) -> Type where
  MkSomeList :: p a -> [a] -> SomeList p
```

We can write some meaningful predicates on this list --- for example, we can
check if it is monotonic (the items increase in order)

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level3.hs#L21-L32

data Comparable :: Type -> Type where
  WitOrd :: Ord a => Comparable a

monotonic :: Ord a => [a] -> Bool
monotonic [] = True
monotonic (x : xs) = go x xs
  where
    go y [] = True
    go y (z : zs) = (y <= z) && go z zs

monotonicSomeList :: SomeList Comparable -> Bool
monotonicSomeList (MkSomeList WitOrd xs) = monotonic xs
```

If we take a step back, we can see that this is a little silly...what value does
`monotonicSomeList` offer over `monotonic`? However, remember the subtlety I
pointed out before: `monotonic` (and `monotonicSomeList`) are not the real
advantage here, the advantage is writing a function that *produces* "list of
some sortable type". For example, we might want to query a database for a column
of values, but we only know the *name* of the column and not the *type* of the
column --- we just know that the values in that column are sortable.

For a contrived one, let's think about pulling such a list from IO:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level3.hs#L34-L54

getItems :: IO (SomeList Comparable)
getItems = do
  putStrLn "would you like to provide int or bool or string?"
  ans <- getLine
  case map toLower ans of
    "int" -> MkSomeList WitOrd <$> replicateM 3 (readLn @Int)
    "bool" -> MkSomeList WitOrd <$> replicateM 3 (readLn @Int)
    "string" -> MkSomeList WitOrd <$> replicateM 3 getLine
    _ -> throwIO $ userError "no"

getAndAnalyze :: IO ()
getAndAnalyze = do
  MkSomeList WitOrd xs <- getItems
  putStrLn $ "Got " ++ show (length xs) ++ " items."
  let isMono = monotonic xs
      isRevMono = monotonic (reverse xs)
  when isMono $
    putStrLn "The items are monotonic."
  when (isMono && isRevMono) $ do
    putStrLn "The items are monotonic both directions."
    putStrLn "This means the items are all identical."
```

Consider also an example where process items different based on what type they
have:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level3.hs#L62-L68

processList :: SomeList TypeRep -> Bool
processList (MkSomeList tr xs)
  | Just Refl <- testEquality tr (typeRep @Bool) = and xs
  | Just Refl <- testEquality tr (TypeRep @Int) = sum xs > 50
  | Just Refl <- testEquality tr (TypeRep @Double) = sum xs > 5.0
  | Just Refl <- testEquality tr (TypeRep @String) = "hello" `elem` xs
  | otherwise = False
```

I'm just kidding, this is not useful, please use a real closed ADT if you ever
want to do this.

Anyway, this pattern is overall similar to how lists are often used in practice
for dynamic languages: *usually* when we use lists, we expect them all to have
items of the same type or interface. However, using lists this way (in a
language without type safety) makes it really tempting to hop down into Level 1,
where you start throwing "alternatively typed" things into your list, as well,
for convenience. All of a sudden, any consumers must now check the type of the
items, and a lot of things are going to start needing unit tests.

Now let us take a bit to talk about ascending and descending between each
levels. In the general case we don't have much to work with, but let's assume
our constraint is `TypeRep` here, so we can match for type equality.

We can move from Level 3 to Level 2 by moving the `TypeRep` into the values of
the list, and we can move from Level 3 to Level 1 by converting our `TypeRep a`
into a `TypeRep [a]`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level3.hs#L75-L86

someListToHList :: SomeList TypeRep -> HList TypeRep
someListToHList (MkSomeList tr xs) = MkSigma tr <$> xs

someListToSigma :: SomeList TypeRep -> Sigma TypeRep
someListToSigma (MkSomeList tr xs) = MkSigma (typeRep @[] `App` tr) xs
```

`App` here as a constructor lets us come `TypeRep`s:
`App :: TypeRep f -> TypeRep a -> TypeRep (f a)`.

Going the other way around is trickier. For `HList`, we don't even know if every
item has the same type, so we can only successfully move up if every item has
the same type. So, first we get the `typeRep` for the first value, and then cast
the other values to be the same type if possible:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level3.hs#L70-L73

hlistToSomeList :: HList TypeRep -> Maybe (SomeList TypeRep)
hlistToSomeList = \case
  [] -> Nothing
  MkSigma tr x : xs -> MkSomeList tr . (x :) <$> traverse (castSigma tr) xs
```

To go from `Sigma TypeRep`, we first need to match the `TypeRep` as some `f a`
application using the `App` pattern...then we can check if `f` is `[]` (list),
then we can create a `SomeList` with the `TypeRep a`. *But*, `testEquality` can
only be called on things of the same kind, so we have to verify that `f` has
kind `Type -> Type` first, so that we can even call `testEquality` on `f` and
`[]`! Phew! Dynamic types are hard!

## Level 4: Homogeneous Typed List

Ahh, now right in the middle, we've reached Haskell's ubiquitous list type! It
is essentially:

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

We're going to talk the least about this one because it's probably the one you
are all familiar with!

## Level 5: Fixed-size List

For the next level of safety, we're not going to try to enforce anything on the
contents of the list, but we can try to enforce something on the *spline* of the
list: the number of items!

To me, this level still feels very natural in Haskell to write in, although in
terms of usability we are starting to bump into some of the things Haskell is
lacking for higher type safety ergonomics. I've talked about [fixed-length
vector types in depth
before](https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html), so
this is going to be a high-level view contrasting this level with the others.

The essential concept is to introduce a *phantom type*, a type parameter that
doesn't do anything other than indicate something that we can use in user-space.
Here we will create a type that structurally encodes the natural numbers 0, 1,
2...:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level5.hs#L15-L15

data Nat = Z | S Nat
```

So, `Z` will represent zero, `S Z` will represent one, `S (S Z)` will represent
two, etc. We want to create a type `Vec n a`, where `n` will be a type of kind
`Nat` (promoted using DataKinds, which lets us use `Z` and `S` as type
constructors), representing a linked list with `n` elements of type `a`. Why is
it called `Vec` and not `List`? I'm not sure, but it's probably a [Stranger
Things](https://strangerthings.fandom.com/wiki/Vecna) reference referring to a
dark, evil being.

We can define `Vec` in a way that structurally matches how `Nat` is constructed,
which is the key to making things work nicely:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level5.hs#L17-L21

data Vec :: Nat -> Type -> Type where
  VNil :: Vec Z a
  (:+) :: a -> Vec n a -> Vec (S n) a

infixr 5 :+
```

This is offered in the *[vec](https://hackage.haskell.org/package/vec)* library.
Here are some example values:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level5.hs#L23-L33

zeroItems :: Vec Z Int
zeroItems = VNil

oneItem :: Vec (S Z) Int
oneItem = 1 :+ VNil

twoItems :: Vec (S (S Z)) Int
twoItems = 1 :+ 2 :+ VNil

threeItems :: Vec (S (S (S Z))) Int
threeItems = 1 :+ 2 :+ 3 :+ VNil
```

Note two things:

1.  `1 :+ 2 :+ VNil` gets automatically type-inferred to be a
    `Vec (S (S Z))     a`, because every application of `:+` adds an `S` to the
    phantom type.
2.  There is *only one way* to construct a `Vec (S (S Z)) a`: by using `:+`
    twice. That means that such a value is a list of exactly two items.

However, the main benefit of this system is *not* so you can create a two-item
list...just use tuples or `data V2 a = V2 a a` from
*[linear](https://hackage.haskell.org/package/linear)* for that. No, the main
benefit is that you can now encode how arguments in your functions relate to
each other with respect to length.

For example, `map :: (a -> b) -> [a] -> [b]` does *not* tell you that the length
of the result list is the same as the length of the input list. You could argue
that because `fmap = map` follows the functor laws, it must return a list with
the same number of items. However, this fact is not encoded in the *type*
itself.

However, consider `vmap :: (a -> b) -> Vec n a -> Vec n b`. Here we see that the
output list must have the same number of items as the input list, and it's
enforced right there in the type signature!

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level5.hs#L35-L38

vmap :: (a -> b) -> Vec n a -> Vec n b
vmap f = \case
  VNil -> VNil
  x :+ xs -> f x :+ vmap f xs
```

And how about `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`? It's not clear or
obvious at all how the final list's length depends on the input lists' lengths.
However, a `vzipWith` would ensure the input lengths are the same size and that
the output list is also the same length:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level5.hs#L40-L45

vzipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vzipWith f = \case
  VNil -> \case
    VNil -> VNil
  x :+ xs -> \case
    y :+ ys -> f x y :+ vzipWith f xs ys
```

Note that both of the inner pattern matches are known by GHC to be exhaustive:
if it knows that the first list is `VNil`, then it knows that `n ~ Z`, so the
second list *has to also* be `VNil`. Thanks GHC!

We can even express more complicated relationships with type families:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level5.hs#L47-L63

type family Plus (x :: Nat) (y :: Nat) where
  Plus Z y = y
  Plus (S z) y = S (Plus z y)

type family Times (x :: Nat) (y :: Nat) where
  Times Z y = Z
  Times (S z) y = Plus y (Times z y)

vconcat :: Vec n a -> Vec m a -> Vec (Plus n m) a
vconcat = \case
  VNil -> id
  x :+ xs -> \ys -> x :+ vconcat xs ys

vconcatMap :: (a -> Vec m b) -> Vec n a -> Vec (Times n m) b
vconcatMap f = \case
  VNil -> VNil
  x :+ xs -> f x `vconcat` vconcatMap f xs
```

Note that all of these only work in GHC because the structure of the functions
themselves match exactly the structure of the type families. If you follow the
pattern matches in the functions, note that they match exactly with the
different equations of the type family.

Famously, we can totally index into fixed-length lists, in a way that indexing
will not fail. To do that, we have to define a type `Fin n`, which represents an
index into a list of length `n`. So, `Fin (S (S (S Z)))` will be either 0, 1, or
2, the three possible indices of a three-item list.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level5.hs#L65-L67

data Fin :: Nat -> Type where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)
```

You can think of this definition inductively. How do we have a value of type
`Fin n`?

1.  Well, if `n` is non-zero, we can always index into the first item, with
    `FZ`.
2.  If we can index `i` into the *i*-th index of a list of length `n`, then we
    can always index into the *i + 1*-th index of a list of length `S n`. That's
    `FS i`.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level5.hs#L69-L74

vindex :: Fin n -> Vec n a -> a
vindex = \case
  FZ -> \case
    x :+ _ -> x
  FS i -> \case
    _ :+ xs -> vindex i xs
```

In a way you can think of `Fin n` as a "spicy int", that takes the place of the
`Int` in `index :: Int -> [a] -> a`. You can use `FZ` in any non-empty list,
because `FZ :: Fin (S n)` will match any non-empty `Vec (S n)`. You can use
`FS FZ` only on something that matches `Vec (S (S n))`. This is the type-safety.

We can also specify non-trivial relationships between lengths of lists, like
making a more type-safe `take :: Int -> [a] -> [a]`. We want to make sure that
the result list has a length less than or equal to the input list. We need
another "spicy int" that can only be constructed in the case that the result
length is less than or equal to the first length. This is often called a "proof"
or a "witness"

We want a type `LTE n m` that is a "witness" that `n` is less than or equal to
`m`. It can only be constructed for if `n` is less than or equal to `m`. For
example, you can create a value of type `LTE (S Z) (S (S Z))`, but not of
`LTE (S (S Z)) Z`

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level5.hs#L76-L78

data LTE :: Nat -> Nat -> Type where
  LTEZ :: LTE Z m
  LTES :: LTE n m -> LTE ('S n) ('S m)
```

Again, let's this of this inductively. We can have a value of type `LTE n m` if:

1.  If `n` is `Z`, it is less than or equal to *any* `m`.
2.  If `n <= m`, then it should also be true that `n + 1 <= m + 1`.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level5.hs#L80-L83

vtake :: LTE n m -> Vec m a -> Vec n a
vtake = \case
  LTEZ -> \_ -> VNil
  LTES l -> \case x :+ xs -> x :+ vtake l xs
```

Notice the similarity to how we would define `take :: Int -> [a] -> [a]`. We
just spiced up the `Int` argument with type safety.

Another thing we would like to do is use be able to *create* vectors of
arbitrary length. We can look at `replicate :: Int -> a -> [a]`, and create a
new "spicy int" `SNat n`, so `vreplicate :: SNat n -> a -> Vec n a`

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level5.hs#L85-L92

data SNat :: Nat -> Type where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

vreplicate :: SNat n -> a -> Vec n a
vreplicate = \case
  SZ -> \_ -> VNil
  SS n -> \x -> x :+ vreplicate n x
```

Notice that this type has a lot more guarantees than `replicate`. For
`replicate :: Int -> a -> [a]`, we can't guarantee (as the caller) that the
return type does have the length we give it. But for
`vreplicate :: SNat n -> a -> Vec n a`, it does!

Note that `SNat n` is actually kind of special. We call it a *singleton*, in
that it perfectly reflects the structure of `n` the type, as a value. By pattern
matching on `SNat n`, we can exactly determine what `n` is. `SZ` means `n` is
`Z`, `SS SZ` means `n` is `S Z`, etc. This is useful because we can't directly
pattern match on types at runtime in Haskell (because of type erasure), but we
*can* pattern match on singletons at runtime.

We actually encountered singletons before in this post! That's right,
`TypeRep a` is a singleton for the type `a`: by pattern matching on it (like
with `App` earlier), we can essentially "pattern match" on the type `a` itself.

In practice, we often write typeclasses to automatically generate singletons:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level5.hs#L94-L104

class KnownNat n where
  nat :: SNat n

instance KnownNat Z where
  nat = SZ

instance KnownNat n => KnownNat (S n) where
  nat = SS nat

vreplicate' :: KnownNat n => a -> Vec n a
vreplicate' = vreplicate nat
```

One last thing: moving back and forth between the different levels. We can't
really write a `[a] -> Vec n a`, because in Haskell, the type variables are
determined by the *caller*. We want `n` to be determined by the list, and the
function itself. And now suddenly we run into the same issue that we ran into
before, when moving between levels 2 and 3.

We can do the same trick before and write an existential wrapper:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level5.hs#L106-L112

data SomeVec a = forall n. MkSomeVec (SNat n) (Vec n a)

toSomeVec :: [a] -> SomeVec a
toSomeVec = \case
  [] -> MkSomeVec SZ VNil
  x : xs -> case toSomeVec xs of
    MkSomeVec n ys -> MkSomeVec (SS n) (x :+ ys)
```

It is common practice (and a good habit) to always include a singleton to the
type you are "hiding" when you create an existential type wrapper, even when it
is not always necessary. That's why we included `TypeRep` before in `HList` and
`SomeList`.

`SomeVec a` is essentially isomorphic to `[a]`, except you can pattern match on
it and get the length `n` as a type you can use.

There's a slightly more light-weight method of returning an existential type: by
returning it in a continuation.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level5.hs#L114-L117

withVec :: [a] -> (forall n. SNat n -> Vec n a -> r) -> r
withVec = \case
  [] -> \f -> f SZ VNil
  x : xs -> \f -> withVec xs \n ys -> f (SS n) (x :+ ys)
```

That way, you can use the type variable within the continuation. Doing
`withSomeVec xs \n v -> ....` is identical to
`case toSomeVec xs of SomeVec n v -> ...`.

However, since you don't get the `n` itself until runtime, you might find
yourself struggling to use concepts like `Fin` and `LTE`. To do use them
comfortably, you have to write functions to "check" if your `LTE` is even
possible, like:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/type-levels/Level5.hs#L119-L124

isLTE :: SNat n -> SNat m -> Maybe (LTE n m)
isLTE = \case
  SZ -> \_ -> Just LTEZ
  SS n -> \case
    SZ -> Nothing
    SS m -> LTES <$> isLTE n m
```

This was a very whirlwind introduction, and I definitely recommend reading [this
post on fixed-length
lists](https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html) for
a more in-depth guide and tour of the features.

Overall as you can see, at this level we gain some powerful guarantees and
tools, but we also run into some small inconveniences (like manipulating
witnesses and singletons). This level is fairly comfortable to work with in
modern Haskell tooling. However, if you live here long enough, you're going to
eventually be tempted to wander into...

## Level 6: Local Structure Enforced List

For here, we are going to ditch the constraints on the *spine* of the list (we
can always add it back in) and now go back into constraints on the *contents* of
the list.

For a simple contrived example, we can imagine we have a list of items that we
want to be able to slice intervals out of. For example, if we had a list of
`Nat`s, let's say we want to be able to get all items between 3 and 6.

One terrible way to do this is to enforce that our list is always *sorted*. That
is, our list contains *local invariances*: any item must be less than or equal
to the next item. We can do this within Haskell by always inserting into the
list in a way that preserves sorting:

``` haskell
insertSortedList :: Ord a => a -> [a] -> [a]
insertSortedList x = \case
  [] -> [x]
  y:ys
    | x <= y = x : y : ys
    | otherwise = y : insertSorted x ys
```

However, the fact that this list is sorted is not apparent in its type. We want
to show that, yes, the list we take in is sorted, and the list we output is also
sorted. What eldritch abomination can we reach for here?

later: turn into a priorirty queue

## Level 7: Global structure Enforced List

This one will be: sum is less than or equal to a number. used for a bounded
priority queue

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

