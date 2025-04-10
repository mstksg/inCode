---
title: Sum Types and Subtypes and Unions
categories: Haskell
tags: functional programming
create-time: 2025/02/24 21:13:02
date: 2025/03/06 09:25:28
identifier: sum-types-and-subtypes-and-unions
slug: sum-types-and-subtypes-and-unions
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
way that forces you to handle each case exhaustively.  Otherwise, sum types
are _much_ less useful.

At the most fundamental level, this behaves like a compiler-enforced null
check, but built within the language in user-space instead being compiler
magic, ad-hoc syntax[^question], or static analysis --- and the fact that it
can live in user-space is why it's been adopted so widely. At a higher level,
functional abstractions like Functor, Applicative, Monad, Foldable, Traversable
allow you to use a `Maybe a` like just a normal `a` with the appropriate
semantics, but that's [a topic for another time (like 2014)][ode].

[ode]: https://blog.jle.im/entry/inside-my-world-ode-to-functor-and-monad.html
[^question]: `?`

This power is very special to me on a personal level.  I remember many years
ago on my first major haskell project changing a type from `String` to `Maybe
String`, and then GHC telling me every place in the codebase where something
needed to change in order for things to work still. Coming from dynamically
typed languages in the past, this sublime experience truly altered my brain
chemistry and Haskell-pilled me for the rest of my life. I still remember the
exact moment, what coffee shop I was at, what my order was, the weather that
day ... it was truly the first day of the rest of my life.

It should be noted that I don't consider sum types a "language feature" or a
compiler feature as much as I'd consider it a design pattern.  Languages that
don't have sum types built-in can usually implement them using typed unions
and an abstract visitor pattern interface (more on that later). Of course,
having a way to "check" your code before running it (like with a type system or
statically verified type annotations) does make a lot of the features much more
useful.

Anyway, this basic pattern can be extended to include more error information in your
`Nothing` branch, which is how you get the `Either e a` type in the Haskell
standard library, or the `Result<T,E>` type in rust.

Along different lines, we have the common use case of defining syntax trees:

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

launch :: String -> Command Int
launch nm = Launch nm id

stop :: Int -> Command Bool
stop pid = Stop pid id
```

This ADT is written in the "interpreter" pattern (used often with things like
free monad), where any arguments not involving `a` are the command payload,
any `X -> a` represent that the command could respond with `X`.

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
    aliceId <- runCommand ref $ launch "alice"
    putStrLn $ "Launched alice with ID " <> show aliceId
    bobId <- runCommand ref $ launch "bob"
    putStrLn $ "Launched bob with ID " <> show bobId
    success <- runCommand ref $ stop aliceId
    putStrLn $
      if success
        then "alice succesfully stopped"
        else "alice unsuccesfully stopped"
    print =<< readIORef ref
```

```
Launched alice with ID 0
Launched bob with ID 1
alice succesfully stopped
fromList [(1, "bob")]
```

Let's add a command to "query" a process id for its current status:

```haskell
data Command a =
    Launch String (Int -> a)    -- ^ takes a name, returns a process ID
  | Stop Int (Bool -> a)        -- ^ takes a process ID, returns success/failure
  | Query Int (String -> a)     -- ^ takes a process ID, returns a status message

query :: Int -> Command String
query pid = Query pid id

runCommand :: IORef (IntMap String) -> Command a -> IO a
runCommand ref = \case
    -- ...
    Query procId next -> do
        procName <- IM.lookup procId <$> readIORef ref
        pure case procName of
          Nothing -> "This process doesn't exist, silly."
          Just n -> "Process " <> n <> " chugging along..."
```

### Relationship with Unions

To clarify a common confusion: sum types can be described as "tagged unions":
you have a tag to indicate which branch you are on (which can be case-matched
on), and then the rest of your data is conditionally present.

In many languages this can be implemented under the hood as a struct with a tag
and a union of data, along with some [abstract visitor pattern
interface][visitor] to ensure exhaustiveness.

[visitor]: https://en.wikipedia.org/wiki/Visitor_pattern

Remember, it's not _exactly_ a union, because, ie, consider a type like:

```haskell
data Entity = User Int | Post Int
```

An `Entity` here could represent a user at a user id, or a post at a post id.
If we considered it purely as a union of `Int` and `Int`:

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

In typed languages with a concept of "objects" and "classes", subtyping is
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
sumAtLocs ixer xs = ixer xs 1 + ixer xs 2 * ixer xs 3
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
fun1 :: [a] -> Int -> a
fun1 = (!!)

fun2 :: [a] -> Int -> a
fun2 xs i = reverse xs !! i

fun3 :: (Foldable t, Floating a) => t a -> Int -> a
fun3 xs i = if length xs > i then xs !! i else pi

fun4 :: Num a => [a] -> Int -> a
fun4 xs i = sum (take i xs)

fun5 :: (Integral b, Num c) => a -> b -> c
fun5 xs i = fromIntegral i

fun5 :: (Foldable t, Fractional a, Integral b) => t a -> b -> a
fun5 xs i = sum xs / fromIntegral i

fun5 :: (Foldable t, Integral b, Floating a) => t a -> b -> a
fun5 xs i = logBase (fromIntegral i) (sum xs)
```

What's going on here? Well, the function _expects_ a `[Double] -> Int ->
Double`, but there are a lot of other types that could be passed instead.

At first this might seem like meaningless semantics or trickery, but it's
deeper than that: remember that each of the above types actually has a very
different meaning and different possible behaviors!

1.  `forall a. [a] -> Int -> a` means that the `a` _must_ come from the given
    list. In fact, any function with that type is guaranteed to be partial: if
    you pass it an empty list, there is no `a` available to use.
2.  `forall a. Num a => [a] -> Int -> a` means that the result might actually come from
    outside of the list: the implementation could always return `0` or `1`,
    even if the list is empty. It also guarantees that it will only add,
    subtract, multiply, or abs: it will never divide.
3.  `forall a. Fractional a => [a] -> Int -> a` means that we could possibly do division
    on the result, but we can't do anything "floating" like square rooting or
    logarithms.
4.  `forall a. Floating a => [a] -> Int -> a` means that we can possibly start square
    rooting or taking the logarithms of our input numbers
5.  `[Double] -> Int -> Double` gives us the least guarantees about the
    behavior: the result could come from thin air (and not be a part of the
    list), and we can even inspect the machine representation of our inputs.

So, we have all of these types with completely different semantics and
meanings. And yet, they can all be passed to something expecting a `[Double] ->
Int -> Double`.  That means that they are all _subtypes_ of `[Double] -> Int ->
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

_Practically_ this is used by many libraries.  For example, [ad][] uses it for
automatic differentiation: its `diff` function looks scary:

[ad]: https://hackage.haskell.org/package/ad

```haskell
diff :: (forall s. AD s ForwardDouble -> AD s ForwardDouble) -> Double -> Double
```

But it relies on the fact that that `(forall s. AD s ForwardDouble -> AD s
ForwardDuble)` is a _superclass_ of `(forall a. Floating a => a -> a)`,
`(forall a. Num a => a -> a)`, etc., so you can give it functions like `\x ->
x * x` (which is a `forall a. Num a => a -> a`) and it will work as that `AD s`
type:

```haskell
ghci> diff (\x -> x * x) 10
20      -- 2*x
```

This "numeric overloading" method is used by libraries for GPU programming, as
well, to accept numeric functions to be optimized and compiled to GPU code.

Another huge application is in the _[lens][]_ library, which uses subtyping to
unite its hierarchy of optics.

[lens]: https://hackage.haskell.org/package/lens

For example, an `Iso` is a subtype of `Traversal` which is a subtype of `Lens`,
and `Lens` is a supertype of `Fold` and `Traversal`, etc. In the end the system
even allows you to use `id` from the *Prelude* as a lens or a traversal,
because the type signature of `id :: a -> a` is actually a subtype of all of
those types!

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

Note that to use this effectively in Haskell with superclasses and subclasses,
you need to manually wrap and unwrap:

```haskell
data SomeFrational = forall a. Fractional a => SumFractional a

castUp :: SomeFractional -> SumNum
castUp (SomeFractional x) = SomeNum x
```

So, `SomeNum` is "technically" a supertype of `SomeFractional`: everywhere a
`SomeNum` is expected, a `SomeFractional` can be given...but in Haskell it's a
lot less convenient because you have to explicitly cast.

In OOP languages, you can often cast "down" using runtime reflection (`SomeNum
-> Maybe SomeFractional`). However, this is impossible in Haskell the way we
have written it!

```haskell
castDown :: SomeNum -> Maybe SomeFractional
castDown = error "impossible!"
```

That's because of type erasure: Haskell does not (by default) couple a value
at runtime with all of its associated interface implementations.  When you
create a value of type `SomeNum`, you are packing an untyped pointer to that
value as well as a "dictionary" of all the functions you could use it with:

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

castDown :: SomeNum -> Maybe SomeFractional
castDown (SomeNum nd x) = error "not possible!"
```

All of these function pointers essentially exist at runtime _inside_ the
`SomeNum`. So, `SomeFractional` can be "cast up" to `SomeNum` by simply
dropping the `FractionalDict`. However, you cannot "cast down" from `SomeNum`
because there is no way to materialize the `FractionalDict`: the association
from type to instance is lost at runtime.  OOP languages usually get around
this by having the _value itself_ hold pointers to all of its interface
implementations at runtime. However, in Haskell, we have type erasure by
default: there are no tables carried around at runtime.[^erasure]

[^erasure]: Must OOP languages also have mechanisms for type erasure, but the
_default_ is unerased, which is opposite of Haskell.

In the end, existential subtyping requires explicit wrapping/unwrapping instead
of implicit or lightweight casting possible in OOP languages optimized around
this sort of behavior.[^existentialprop] Existential-based subtyping is just less common in
Haskell because parametric polymorphism offers a solution to most similar
problems.  For more on this topic, Simon Peyton Jones has [a nice lecture][spj]
on the topic.

[spj]: https://www.youtube.com/watch?v=6COvD8oynmI

[^existentialprop]: Note that there are current [GHC proposals][existentials]
that attempt to allow "naked" existentials without newtype wrappers, so we
could actually get the same seamless and implicit up-casting as we would get in
OOP languages. However, the jury is out on whether or not this is a good idea.

[existentials]: https://github.com/ghc-proposals/ghc-proposals/pull/473

The pattern of _using_ existentially qualified data in a container (like
`[SomeNum]`) is often called the "widget pattern" because it's used in
libraries like *[xmonad][]* to allow extensible "widgets" stored alongside the
methods used to manipualte them. It's more common to explicitly store the
handler functions (a "dictionary") inside the type instead of of existential
typeclasses, but sometimes it can be nice to let the compiler handle generating
and passing your method tables implicitly for you. Using existential
typeclasses instead of explicit dictionaries also allows you to bless certain
methods and functions as "canonical" to your type, and the compiler will make
sure they are always coherent.

[xmonad]: https://hackage.haskell.org/package/xmonad

I do mention in [a blog post about different types of existential
lists][type-safety], however, that this "container of instances" type is much
less useful in Haskell than in other languages for many reasons, including the
up/downcasting issues mentioned above. In addition, Haskell gives you a whole
wealth of functionality to operate over homogeneous parameters (like `[a]`,
where all items have the same type) that jumping to heterogeneous lists gives
up so much.

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

This tension that I described earlier is closely related to [the expression
problem][], and is a tension that is inherent to a lot of different aspects of
language and abstraction design. However, in the context laid out in this post,
it serves as a good general guide to decide what pattern to go down:

[the expression problem]: https://en.wikipedia.org/wiki/Expression_problem

*   If you expect a canonical set of "inhabitants" and an open set of
    "operations", sum types can suit that end of the spectrum well.
*   If you expect a canonical set of "operations" and an open set of
    "inhabitants", consider subtyping and supertyping.

I don't really think of the expression problem as a "problem" in the sense of
"some hindrance to deal with". Instead, I see it in the "math problem" sort of
way: by adjusting how you approach things, you can play with the equation make
the most out of what requirements you need in your design.

Looking Forward
---------------

A lot of frustration in Haskell (and programming in general) lies in trying to
force abstraction and tools to work in a way they weren't meant to.  Hopefully
this short run-down can help you avoid going _against_ the point of these
design patterns and start making the most of what they can offer. Happy
Haskelling!

Special Thanks
--------------

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts. Very special
thanks to my supporter at the "Amazing" level on [patreon][], Josh Vera! :)

[patreon]: https://www.patreon.com/justinle/overview
