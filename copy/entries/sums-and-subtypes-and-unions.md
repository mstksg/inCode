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

In this blog post we are going to talk about these things in terms of _design
patterns_, not necessarily in terms of language design. The examples of using
them will be presented first, and then we'll talk about how they might be
implemented in languages without native support.

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

Another example where things shine: we can imagine a "command" type that sends
different types of commands with different payloads. This can be interpreted as
perhaps the result of parsing command line arguments or the message in some
communication protocol.

For example, you could have a protocol that launches and controls processes:

```haskell
data Command a =
    Launch String (Int -> a)    -- ^ takes a name, returns a process ID
  | Stop Int (Bool -> a)        -- ^ takes a process ID, returns success/failure
```

This ADT is written in the "interpreter" pattern, where any arguments not
involving `a` are the command payload, any `X -> a`s represent that the command
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
        modify $ IM.delete procId
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

Alternatively, you can implement them using dynamic dispatch (which often
manifests as the "visitor pattern" in OOP languages implementing sum types
using subclasses). But, that's a story for another day.

Subtypes Solve a Different Problem
----------------------------------

<!-- "widgets" for games and plugins, database connectors, anything extensible:
show how typeclasses work well for subtyping in hasekll -->

The Expression Problem
----------------------

Surprise, it's the Expression Problem!

Implementations
---------------

### Unions with a Tag

### Visitor Pattern

Conclusion
----------

Ta dah
