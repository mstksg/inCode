---
title: Sums and Subtypes and Unions
categories: Haskell
tags: functional programming
create-time: 2025/02/24 21:13:02
identifier: sums-and-subtypes-and-unions
slug: sums-and-subtypes-and-unions
---

There's yet again been a bit of functional programming-adjacent twitter drama
recently, but it's actually sort of touched into some subtleties about sum
types that I am asked about (and think about) a lot nowadays.  So, I'd like to
take this opportunity to talk a bit about the "why" and nature of sum types and
how to use them effectively, and how they contrast with other related concepts
in programming and software development and when even cases where sum types
aren't the best option.

Sum Types at their Best
-----------------------

The quintessential sum type that you just can't live without is `Maybe`, now
adopted in a lot of languages as `Optional`:

```haskell
data Maybe a = Nothing | Just a
```

If you have a value of type `Maybe Int`, it means that its valid values are
`Nothing`, `Just 0`, `Just 1`, etc.

This is also a good illustration to why we call it a "sum" type: if `a` has `n`
possible values, then `Maybe a` has `1 + n`: we add the single new value
`Nothing` to it.

The "benefit" of the sum type is illustrated pretty clearly here too: every
time you _use_ a value of type `Maybe Int`, you are forced to consider the fact
that it could be `Nothing`:

```haskell
showMaybeInt :: Maybe Int -> String
showMaybeInt = \case
  Nothing -> "There's nothing here"
  Just i -> "Something is here: " <> show i
```

That's because usually in sum type implementations, they are implemented in a
way that forces you to to handle each case exhaustively.  Otherwise, sum types
are _much_ less useful.

At the most fundamental level this behaves like a compiler-enforced null check,
but built within the language in user-space (if it's implemented like a true
sum type) instead being compiler magic, ad-hoc syntax, or static analysis ---
and the fact that it can live in user-space is why it's been adopted so widely.
At a higher level, functional abstractions like Functor, Applicative, Monad,
Foldable, Traversable allow you to use a `Maybe a` like just a normal `a` with
the appropriate semantics, but that's [a topic for another time][ode].

[ode]: https://blog.jle.im/entry/inside-my-world-ode-to-functor-and-monad.html

This basic pattern can be extended to include more error information in your
`Nothing` branch, which is how you get the `Either e a` type in the Haskell
standard library, or the `Result<T,E>` type in rust.

As a quick aside, remember that this example is a "generic" type (it has a type
parameter), but that's unrelated to the fact that it's a sum type.  After all,
we could have a sum type `MaybeInt` that's either `NoInt` or `JustInt 3`,
`JustInt 4`, etc.  I just wanted to clarify that the "sum typeness" (possible
different distinguishable branches) is separate from the "parameterized typeenes"

Along different lines (not an error abstraction, not parameterized), we have
the common use case of defining syntax trees:

```haskell
data Expr =
    Lit Int
  | Negate Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr

eval :: Expr -> Int
eval = \case
    Lit i -> i
    Negate x -> -(eval x)
    Add x y -> eval x + eval y
    Sub x y -> eval x - eval y
    Mul x y -> eval x * eval y

pretty :: Expr -> String
pretty = go 0
  where
    wrap :: Int -> Int -> String -> String
    wrap prio opPrec s
      | prio > opPrec = "(" <> s <> ")"
      | otherwise = s
    go prio = \case
        Lit i -> show i
        Negate x -> wrap prio 2 $ "-" <> go 2 x
        Add x y -> wrap prio 0 $ go 0 x <> " + " <> go 1 y
        Sub x y -> wrap prio 0 $ go 0 x <> " - " <> go 1 y
        Mul x y -> wrap prio 1 $ go 1 x <> " * " <> go 2 y

main :: IO ()
main = do
    putStrLn $ pretty myExpr
    print $ eval myExpr
  where
    myExpr = Mul (Negate (Add (Lit 4) (Lit 5))) (Lit 8)
```

```
-(4 + 5) * 8
-72
```

Now, if we add a new command to the sum type, the compiler enforces us to
handle it.

```haskell
data Expr =
    Lit Int
  | Negate Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Abs Expr

eval :: Expr -> Int
eval = \case
    Lit i -> i
    Negate x -> -(eval x)
    Add x y -> eval x + eval y
    Sub x y -> eval x - eval y
    Mul x y -> eval x * eval y
    Abs x -> abs (eval x)

pretty :: Expr -> String
pretty = go 0
  where
    wrap :: Int -> Int -> String -> String
    wrap prio opPrec s
      | prio > opPrec = "(" <> s <> ")"
      | otherwise = s
    go prio = \case
        Lit i -> show i
        Negate x -> wrap prio 2 $ "-" <> go 2 x
        Add x y -> wrap prio 0 $ go 0 x <> " + " <> go 1 y
        Sub x y -> wrap prio 0 $ go 0 x <> " - " <> go 1 y
        Mul x y -> wrap prio 1 $ go 1 x <> " * " <> go 2 y
        Abs x -> wrap prio 2 $ "|" <> go 0 x <> "|"
```

Another example where things shine are as clearly-fined APIs between processes.
For example, we can imagine a "command" type that sends different types of
commands with different payloads. This can be interpreted as perhaps the result
of parsing command line arguments or the message in some communication
protocol.

For example, you could have a protocol that launches and controls processes:

```haskell
data Command a =
    Launch String (Int -> a)    -- ^ takes a name, returns a process ID
  | Stop Int (Bool -> a)        -- ^ takes a process ID, returns success/failure
```

This ADT is written in the "interpreter" pattern, where any arguments not
involving `a` are the command payload, any `X -> a` represent that the command
could respond with `X`.

Let's write a sample interpreter backing the state in an IntMap in an IORef:

```haskell
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)

runCommand :: IORef (IntMap String) -> Command a -> IO a
runCommand ref = \case
    Launch newName next -> do
        currMap <- readIORef ref
        let newId = case IM.lookupMax currMap of
              Nothing -> 0
              Just (i, _) -> i + 1
        modifyIORef ref $ IM.insert newId newName
        pure (next newId)
    Stop procId next -> do
        existed <- IM.member procId <$> readIORef ref
        modifyIORef ref $ IM.delete procId
        pure (next existed)

main :: IO ()
main = do
    ref <- newIORef IM.empty
    aliceId <- runCommand ref $ Launch "alice" id
    putStrLn $ "Launched alice with ID " <> show aliceId
    bobId <- runCommand ref $ Launch "bob" id
    putStrLn $ "Launched bob with ID " <> show bobId
    success <- runCommand ref $ Stop aliceId id
    putStrLn $
      if success
        then "alice succesfully stopped"
        else "alice unsuccesfully stopped"
    print =<< readIORef ref
```

```
Launched alice with ID 0
Launched bob with ID 0
alice succesfully stopped
fromList [(1, "bob")]
```

In a real application you might interpret this command in `IO`, but for
demonstration's sake, here's a way of "handling" this command purely using an
`IntMap` of id's to names to back the state:

Let's add a command to "query" a process id for its current status:

```haskell
data Command a =
    Launch String (Int -> a)    -- ^ takes a name, returns a process ID
  | Stop Int (Bool -> a)        -- ^ takes a process ID, returns success/failure
  | Query Int (String -> a)     -- ^ takes a process ID, returns a status message

runCommand :: IORef (IntMap String) -> Command a -> IO a
runCommand ref = \case
    Launch newName next -> do
        currMap <- readIORef ref
        let newId = case IM.lookupMax currMap of
              Nothing -> 0
              Just (i, _) -> i + 1
        modifyIORef ref $ IM.insert newId newName
        pure (next newId)
    Stop procId next -> do
        existed <- IM.member procId <$> readIORef ref
        modify $ IM.delete procId
        pure (next existed)
    Query procId next -> do
        procName <- IM.lookup procId <$> readIORef ref
        pure case procName of
          Nothing -> "This process doesn't exist, silly."
          Just n -> "Process " <> n <> " chugging along..."
```

<!-- examples: maybe a state machine example might be nice but oh well -->


### Relationship with Unions

To clarify a common confusion, sum types could be described as "tagged unions":
you have a tag to indicate which branch you are on (which can be case-matched
on), and then the rest of your data is conditionally present.

In many languages this can be implemented _literally_ as a struct with a tag
and a union of data.

Remember, it's not _exactly_ a union, because, ie, consider a type like:

```haskell
data Entity = User Int | Post Int
```

This data could represent a user at a user id, or a post at a post id. If we
considered it purely as a union of `Int` and `Int`:

```c
union Entity {
    int user_id;
    int post_id;
};
```
we'd lose the ability to branch on whether or not we have a user or an int. If
we have the tagged union, we recover the original tagged union semantics:

```c
struct Entity {
    bool is_user;
    union {
        int user_id;
        int post_id;
    } payload;
};
```

Of course, you still need an abstract interface like the visitor pattern to
actually be able to use this as a sum type with guarantees that you handle
every branch, but that's a story for another day.  Alternatively, if your
language supports dynamic dispatch nicely, that's another underlying
implementation that would work to back a higher-level visitor pattern
interface.

Subtypes Solve a Different Problem
----------------------------------

Now, sum types aren't exactly a part of common programming education
curriculum, but _subtypes_ and _supertypes_ definitely were drilled into every
CS student's brain and waking nightmares from their first year.

Informally (a la Liskov), `B` is a subtype of `A` (and `A` is a supertype of
`B`) if anywhere that expects an `A`, you could also provide a `B`.

In normal object-oriented programming, this often shows up in early lessons as
`Cat` and `Dog` being subclasses of an `Animal` class, or `Square` and `Circle`
being subclasses of a `Shape` class.

When people first learn about sum types, there is a tendency to understand them
as similar to subtyping. This is unfortunately understandable, since a lot of
introductions to sum types often start with something like

```haskell
-- | Bad Sum Type Example!
data Shape = Circle Double | Rectangle Double Double
```

While there are situations where this might be a good sum type (ie, for an API
specification or a state machine), on face-value this is a bad example on the
sum types vs. subtyping distinction.

You might notice the essential "tension" of the sum type: you declare all of
your options up-front, the functions that consume your value are open and
declared ad-hoc. And, if you add new options, all of the consuming functions
must be adjusted.

So, _subtypes_ (and supertypes) are more effective when they lean into the
opposite end: the universe of possible options are open and declared ad-hoc,
but the _consuming functions_ are closed. And, if you add new functions, all of
the members must be adjusted.

In typed languages with a concept of _classes_ on top of objects, subtyping is
often implemented using inheritance and interfaces.

```java
interface Widget {
    void draw();
    void handleEvent(String event);
    String getName();
}

class Button implements Widget {
    // ..
}

class InputField implements Widget {
    // ..
}

class Box implements Widget {
    // ..
}
```

So, a function like `processWidget(Widget widget)` that expects a `Widget`
would be able to be passed a `Button` or `InputField` or `Box`. And, if you had
a container like `List<Widget>`, you could assemble a structure using `Button`,
`InputField`, and `Box`. A perfect Liskov storm.

In typical library design, you're able to add new implementations of `Widget`
as an open universe easily: anyone that imports `Widget` can, and they can now
use it with functions taking `Widget`s. _But_, if you ever wanted to add new
functionality to the `Widget` interface, that would be a breaking change to all
downstream implementations.

However, this implementation of subtyping, while prevalent, is the most
mind-numbly boring realization of the concept, and it pained my soul to even
spend time talking about it.  So let's jump into the more interesting way that
subtype and supertype relationships manifest in the only language where
anything is interesting: Haskell.

### Subtyping via Parametric Polymorphism

In Haskell, subtyping is implemented in terms of parametric polymorphism and
sometimes typeclasses. This allows for us to work nicely with the concept of
functions and APIs as subtypes and supertypes of each other.

For example, let's look at a function that takes indexers and applies them:

```haskell
sumAtLocs :: ([Double] -> Int -> Double) -> [Double] -> Double
sumAtLocs ixer xs = ixer 1 xs + ixer 2 xs * ixer 3 xs
```

```haskell
ghci> sumAtLocs (!!) [1,2,3,4,5]
14
```

So, what functions could you pass to `sumAtLocs`? Can you _only_ pass `[Double]
-> Int -> Double`?

Well, not quite. Look at the above where we passed `(!!)`, which has type
`forall a. [a] -> Int -> a`!

In fact, what other types could we pass?  Here are some examples:

```haskell
(!!) :: forall a. [a] -> Int -> a
(\xs i -> reverse xs !! i) :: forall a. [a] -> Int -> a
(\xs i -> if length xs > i then xs !! i else pi) :: forall t a. (Foldable t, Floating a) => t a -> Int -> a
(\xs i -> sum (take i xs)) :: forall a. Num a => [a] -> Int -> a
(\xs i -> fromIntegral i) :: forall a b c. (Integral b, Num c) => a -> b -> c
(\xs i -> sum xs / fromIntegral i) :: forall t a b. (Foldable t, Fractional a, Integral b) => t a -> b -> a
(\xs i -> logBase (fromIntegral i) (sum xs)) :: forall t a b. (Foldable t, Integral b, Floating a) => t a -> b -> a
```

What's going on here? Well, the function _expects_ a `[Double] -> Int ->
Double`, but there are a lot of other types that could be passed instead.

And don't mistake this for some semantics or trickery: each of the above types
actually has a very different meaning and different possible behaviors!

1.  `forall a. [a] -> Int -> a` means that the `a` _must_ come from the given
    list. In fact, any function with that type is guaranteed to be partial: if
    you pass it an empty list, there is no `a` available to use.
2.  `Num a => [a] -> Int -> a` means that the result might actually come from
    outside of the list: the implementation could always return `0` or `1`,
    even if the list is empty. It also guarantees that it will only add,
    subtract, multiply, or abs: it will never divide.
3.  `Fractional a => [a] -> Int -> a` means that we could possibly do division
    on the result, but we can't do anything "floating" like square rooting or
    logarithms.
4.  `Floating a => [a] -> Int -> a` means that we can possibly start square
    rooting or taking the logarithms of our input numbers
5.  `[Double] -> Int -> Double` gives us the least guarantees about the
    behavior: the result could come from thin air (and not be a part of the
    list), and we can even inspect the machine representation of our inputs.

So, we have all of these types with completely different semantics and
meanings. And yet, they can all be passed to something expecting a `[Double] ->
Int -> Double`.  That means that they are all subtypes of `[Double] -> Int ->
Double`! `[Double] -> Int -> Double` is a supertype that houses multitudes of
possible values, uniting all of the possible values and semantics into one big
supertype.

Through the power of parametric polymorphism and typeclasses, you can actually
create an extensible hierarchy of _supertypes_, not just of subtypes.

Consider a common API for json serialization. You could have multiple functions
that serialize into JSON:

```haskell
fooToJson :: Foo -> Value
barToJson :: Bar -> Value
bazToJson :: Baz -> Value
```

Through typeclasses, you can create:

```haskell
toJSON :: ToJSON a => a -> Value
```

The type of `toJSON :: forall a. JSON a => a -> Value` is a subtype of
`Foo -> Value`, `Bar -> Value`, and `Baz -> Value`, because everywhere you
would _want_ a `Foo -> Value`, you could give `toJSON` instead. Every time you
_want_ to serialize a `Foo`, you could use `toJSON`.

This usage works well, as it gives you an extensible abstraction to design code
around.  When you write code polymorphic over `Monoid a`, it forces you to
reason about your values with respect to only the aspects relating to
monoidness. If you write code polymorphic over `Num a`, it forces you to reason
about your values only with respect to how they can be added, subtracted,
negated, or multiplied, instead of having to worry about things like their
machine representation.

The extensibility comes from the fact that you can create _even more
supertypes_ of `forall a. ToJSON a => a -> Value` easily, just by defining a
new typeclass instance.  So, if you need a `MyType -> Value`, you could _make_
it a supertype of `toJSON :: ToJSON a => a -> Value` by defining an instance of
the `ToJSON` typeclass, and now you have something you can use in its place.

But note the subtle trade-off: it's easy to create new supertypes (and
subtypes, as we'll see later), but if you want to add new methods or
functionality to the class, then that becomes a huge breaking change.

### Subtyping using Existential Types

What more closely matches the _spirit_ of subtypes in OOP and other languages
is the _existential type_: a value that can be a value of any type matching
some interface.

For example, let's imagine a value that could be any instance of `Num`:

```haskell
data SomeNum = forall a. Num a => SomeNum a

someNums :: [SomeNum]
someNums = [SomeNum (1 :: Int), SomeNum (pi :: Double), SomeNum (0xfe :: Word)]
```

This is _somewhat_ equivalent to Java's `List<MyInterface>` or `List<MyClass>`,
or python's `List[MyClass]`.

Note that to use this effectively with superclasses and subclasses, you need to
manually wrap and unwrap:

```haskell
data SomeFrational = forall a. Fractional a => SumFractional a

castUp :: SomeFractional -> SumNum
castUp (SomeFractional x) = SomeNum x
```

So, `SomeNum` is "technically" a supertype of `SomeFractional`: everywhere a
`SomeNum` is expected, a `SomeFractional` can be given...but in Haskell it's a
lot less convenient because you have to explicitly cast.

In OOP languages, you can often cast "down" using runtime reflection (SomeNum
-> Maybe SomeFractional). However, this is impossible the way we have written
it!

```haskell
castDown :: SomeNum -> Maybe SomeFractional
castDown = error "impossible!"
```

That's because of _type erasure_.  Haskell has no global type lookup table in
its runtime. When you create a value of type `SomeNum`, you are packing an
untyped pointer to that value as well as a "dictionary" of all the functions
you could use it with:

```haskell
data NumDict a = NumDict
    { (+) :: a -> a -> a
    , (*) :: a -> a -> a
    , negate :: a -> a
    , abs :: a -> a
    , fromInteger :: Integer -> a
    }

mkNumDict :: Num a => NumDict a
mkNumDict = NumDict (+) (*) negate abs fromInteger

data FractionalDict a = FractionalDict
    { numDict :: NumDict a
    , (/) :: a -> a -> a
    , fromRational :: Rational -> a
    }

-- | Essentially equivalent to the previous 'SomeNum'
data SomeNum = forall a. SomeNum
    { numDict :: NumDict a
    , value :: a
    }

-- | Essentially equivalent to the previous 'SomeFractional'
data SomeFractional = forall a. SomeFractional
    { fractionalDict :: FractionalDict a
    , value :: a
    }

castUp :: SomeFractional -> SomeNum
castUp (SomeFractional (FractionalDict {numDict}) x) = SomeNum d x
```

All of these function pointers essentially exist at runtime "inside" the
`SomeNum`. So, `SomeFractional` can be "cast up" to `SomeNum` by simply
dropping the `FractionalDict`. However, you cannot "cast down" from `SomeNum`
because there is no way to materialize the `FractionalDict`: the association
from type to instance is lost at runtime.  OOP languages get around this by
having the _value itself_ hold pointers to all of its interface implementations
at runtime, or some other form of runtime reflection. However, in Haskell, we
have type erasure by default: there are no tables carried around at runtime.
Most OOP languages also have a mechanism for type erasure to mimic the same
runtime representation, and with that you also lose the ability to downcast.

In the end, existential subtyping requires explicit wrapping/unwrapping instead
of implicit or lightweight casting possible in OOP languages optimized around
this sort of behavior. In the end, existential-based subtyping is just less
common in Haskell because parametric polymorphism offers a solution to most
similar problems.

This pattern (especially when you store existentials in a container, like
`[SomeNum]`) is often called the "widget pattern" because it's used in
libraries like *[xmonad][]* to allow extensible "widgets" stored alongside the
methods used to manipualte them. It's more common to explicitly store the
handler functions (a "dictionary") inside the type instead of of existential
typeclasses, but sometimes it can be nice to let the compiler handle generating
and passing your method tables implicitly for you. Using existential
typeclasses also allows you to bless certain methods and functions as
"canonical" to your type, and the compiler will make sure they are always
coherent.

[xmonad]: https://hackage.haskell.org/package/xmonad

I do mention in [a blog post about different types of existential
lists][type-safety], however, that this "container of instances" type is much
less useful in Haskell than in other languages for many reasons. For one,
because Haskell gives you a whole wealth of functionality to operate over
homogeneous parameters (like `[a]`, where all items have the same type) that
jumping to heterogeneous lists gives up so much. Another reason is that that
"widget container" patterns in other languages often resort to runtime
reflection of time information, which means in practice you'd have to add an
extra `Typeable` constraint to your existentials to use the same way you'd use
them in OOP languages, which usually implicitly include this.

[type-safety]: https://blog.jle.im/entry/levels-of-type-safety-haskell-lists.html

::::: {.note}
**Aside**

Let's briefly take a moment to talk about how typeclass hierarchies give us
subtle subtype/supertype relationships.

Let's look at the classic `Num` and `Fractional`:

```haskell
class Num a

class Num a => Fractional a
```

`Num` is a _superclass_ of `Fractional`, and `Fractional` is a _subclass_ of
`Num`. Everywhere a `Num` constraint is required, you can provide a
`Fractional` constraint to do the same thing.

However, in these two types:

```haskell
Num a => a
Fractional a => a
```

`forall a. Num a => a` is actually a _subclass_ of `forall a. Fractional a =>
a`! That's because if you need a `forall a. Fractional a => a`, you can provide
a `forall a. Num a => a` instead.  In fact, let's look at three levels:
`Double`, `forall a. Fractional a => a`, and `forall a. Num a => a`.


```haskell
-- can be used as `Double`
1.0 :: Double
1.0 :: Fractional a => a
1 :: Num a => a

-- can be used as `forall a. Fractional a => a`
1.0 :: Fractional a => a
1 :: Num a => a

-- can be used as `forall a. Num a => a`
1 :: Num a => a
```

So, `Double` is a supertype of `Fractional a => a` is a supertype of `Num a =>
a`.

The general idea here is that the more super- you go, the more you "know" about
the actual term you are creating. So, with `Num a => a`, you know
the _least_ (and, you have the most possible actual terms because there are
more instances of `Num` than of `Fractional`). And, with `Double`, you know the
_most_: you even know its machine representation!

So, `Num` is a superclass of `Fractional` but `forall a. Num a => a` is a
subclass of `forall a. Fractional a => a`.  This actually follows the typical
rules of subtyping: if something appears on the "left" of an arrow (`=>` in
this case), it gets flipped from sub- to super-.  We often call the left side
a "negative" (contravariant) position and the right side a "positive" position,
because a negative of a negative (the left side of a left size, like `a` in `(a
-> b) -> c`) is a positive.

Also note that our "existential wrappers":

```haskell
data SomeNum = forall a. Num a => SomeFractional a
data SomeFractional = forall a. Fractional a => SomeFractional a
```

can be CPS-transformed to their equivalent types:

```haskell
type SomeNum' = forall r. (forall a. Num a => a -> r) -> r
type SomeFractional' = forall r. (forall a. Fractional a => a -> r) -> r

toSomeNum' :: SomeNum -> SomeNum'
toSomeNum' (SomeNum x) f = f x

toSomeNum :: SomeNum' -> SomeNum
toSomeNum sn = sn SomeNum
```

And in those cases, `Num` and `Fractional` again appear in the covariant
(positive) position, since they're the negative of negative. So, this aligns
with our intuition that `SomeFractional` is a subtype of `SomeNum`.
:::::

The Expression Problem
----------------------

This tension that I described earlier is often called [The Expression
Problem][], and is a tension that is inherent to a lot of aspects of language
and abstraction design. However, in the context laid out in this post, it
serves as a good general guide to decide what pattern to go down:

[The Expression Problem]: https://en.wikipedia.org/wiki/Expression_problem

*   If you expect a canonical set of "inhabitants" and an open set of
    "operations", sum types can suit that end of the spectrum well.
*   If you expect a canonical set of "operations" and an open set of
    "inhabitants", consider subtyping and supertyping.

I don't really see the expression problem in the "difficult situation" sense of
the word problem. Instead, I see it in the "math problem" sort of way: by
adjusting how you approach things, you can make the most out of what
requirements you need in your design.

Looking Forward
---------------

A lot of frustration in Haskell (and programming in general) lies in trying to
force abstraction and tools to work in a way they weren't meant to.  Hopefully
this short run-down can help you avoid going _against_ the point of these
design patterns and start making the most of what they can offer!
