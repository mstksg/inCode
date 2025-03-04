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

### Extensible Abstractions

In Haskell, subtyping is implemented in terms of parametric polymorphism.

For example, consider a common API for json serialization.  You might have a
bunch of things that can be "empty":

```haskell
emptyString :: String
emptyString = ""

emptyList :: [a]
emptyList = []

emptyMaybe :: Maybe a
emptyMaybe = Nothing
```

But, consider the `Monoid` typeclass, which gives you:

```haskell
mempty :: Monoid a => a
```

Now, any function that expects a `String`, `[Int]`, `Maybe Double`, etc., you
can _also_ pass in `mempty`.  So, this means that the type `forall a. Monoid a
=> a` is a _subtype_ of `String`, `[Bool]`, etc.

Because of how typeclasses work, we can create a new supertype of `forall a.
Monoid a => a`:

```haskell
newtype SumInt = Sum Int

instance Semigroup SumInt where
    SumInt x <> SumInt y = SumInt (x + y)

instance Monoid SumInt where
    mempty = SumInt 0
```

Another common example is to put the typeclass constraint on a type in the
negative (contravariant) part of the type.  For example, you could have
multiple functions that serialize into JSON:

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

All in all, this is convenient because you can create _even more supertypes_ of
`forall a. ToJSON a => a -> Value` easily, just by defining a new typeclass
instance. So, if you want _more_ things you can use `toJSON` on (or more things
you can provide with `mempty`), you just need to define the typeclass instance.

If you ever added a new method to `Monoid` or `ToJSON`, this is a huge breaking
change for all instances. But adding a new instance is easy!

### Containers of an Interface

A common abstraction in languages that rely heavily on subtyping is a
heterogeneous "container" (like a list) of things of different type where all
you know about each value is that they instantiate some interface.

For example, in Haskell, you could have a list of things that are `Show`-able:

```haskell
data Showable = forall a. Show a => Showable a
```

So you could have a function like:

```haskell
printAll :: [Showable] -> IO ()
printAll = traverse_ \(Showable x) -> print x
```

```haskell
ghci> printAll [Showable 3, Showable True, Showable "hello"]
3
True
"hello"
```

This _specific_ example is pretty silly because `[Showable]` is just the
same as `[String]`:

```haskell
ghci> traverse_ putStrLn [show 3, show True, show "hello"]
3
True
"hello"
```

However, this "widget pattern" does become more useful when your typeclass is
more complicated, like the `Layout` class in *[xmonad][]*, where it can be used
to pass a container of "widgets" that are to be rendered, where the widgets can
all be different types but all implement a specific typeclass.  Usually you can
also get away with a container of handler functions instead (so you don't have
to use existential types), but using typeclasses in this case gives you
guarantees of canonicity (one instance per nominal type) and some convenient
wrapping/unwraping and interactions between subclasses.

[xmonad]: https://hackage.haskell.org/package/xmonad

I do mention in [a blog post about different types of existential
lists][type-safety], however, that this "container of instances" type is much
less useful in Haskell than in other languages for two main reasons. First,
because Haskell gives you a whole wealth of functionality to operate over
homogeneous parameters (like `[a]`, where all items have the same type) that
jumping to heterogeneous lists gives up so much. Second, that "widget
container" patterns in other languages often resort to runtime reflection of
time information, which means in practice you'd have to add an extra `Typeable`
constraint to your existentials to use the same way you'd use them in OOP
languages.

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
type SomeNum = forall r. (forall a. Num a => a -> r) -> r
type SomeFractional = forall r. (forall a. Fractional a => a -> r) -> r
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

As always, there are exceptions, but hopefully this helps you see situations
where one might be more useful than the other from a high-level abstraction
design point of view!
