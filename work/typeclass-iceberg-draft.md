---
title: "Typeclasses and How They Are Used"
categories: Haskell
tags: functional programming, typeclasses, haskell, design patterns
create-time: 2026/01/30 12:00:00
identifier: typeclass-iceberg
slug: typeclasses-and-how-they-are-used
---

Typeclasses were invented to solve a humble problem: making `+` work on both
`Int` and `Double`.  Wadler and Blott introduced them in their [1989 POPL
paper][wadler], "How to make ad-hoc polymorphism less ad hoc", and the title
says it all --- they wanted to tame the mess of operator overloading.

[wadler]: https://people.csail.mit.edu/dnj/teaching/6898/papers/wadler88.pdf

But like all good ideas, typeclasses grew.  People started finding uses far
beyond `+` and `==`.  They used them for lawful abstractions, then for generic
programming, then for type-level computation, then for things that would make
Wadler and Blott's heads spin.  Along the way, the community also discovered
the limits --- places where typeclasses are tempting but harmful.

I want to walk through the history of typeclasses by example --- not just what
they _are_, but the surprising variety of ways people have found to use them,
from the obvious to the downright weird.  Think of it as a field guide to
typeclass usage in the wild, with some notes on which specimens are healthy and
which are invasive.

The Original Problem
====================

You're writing code.  A nice little polynomial function:

```haskell
poly :: Int -> Int
poly x = x * x + 1
```

Works great.  But then you also want it to work with `Double`:

```haskell
polyDouble :: Double -> Double
polyDouble x = x * x + 1
```

And complex numbers.  And vectors.  And matrices.  And now you're staring down
the barrel of writing the same function over and over again, once for each
type, identical in every way except the type signature.

This is the _ad-hoc polymorphism_ problem, and every language has some answer
to it.  Java gives you method overloading (resolved at compile time by argument
types).  Python says "just hope `+` works and find out at runtime".  C++ has
template specialization (which gets messy fast).

Haskell's answer is _typeclasses_.

The Typeclass Solution
----------------------

A typeclass defines a set of operations that a type must support:

```haskell
class Num a where
    (+) :: a -> a -> a
    (*) :: a -> a -> a
    (-) :: a -> a -> a
    fromInteger :: Integer -> a
    -- ...
```

And types opt in by providing instances:

```haskell
instance Num Int where
    -- ...

instance Num Double where
    -- ...
```

Now we can write our polynomial _once_:

```haskell
poly :: Num a => a -> a
poly x = x * x + 1
```

And it works for any type that has a `Num` instance:

```haskell
ghci> poly 3
10
ghci> poly 3.5
13.25
ghci> poly (2 :+ 1)  -- Complex numbers
4.0 :+ 4.0
ghci> poly (V2 1 2)  -- 2D vectors
V2 2 5
```

One function, many types, zero copy-pasting.  So far, this might look like
just a fancier version of interfaces or traits.  But the real power is in what
people discovered next.

How It Actually Works
---------------------

What the compiler actually does when it sees a typeclass constraint is pass a
_dictionary_ of operations.  When you write:

```haskell
poly :: Num a => a -> a
poly x = x * x + 1
```

The compiler desugars this into something like:

```haskell
data NumDict a = NumDict
    { plusOp        :: a -> a -> a
    , timesOp       :: a -> a -> a
    , fromIntegerOp :: Integer -> a
    -- ...
    }

poly :: NumDict a -> a -> a
poly dict x = plusOp dict (timesOp dict x x) (fromIntegerOp dict 1)
```

At each call site, the compiler looks up the unique dictionary for the concrete
type and passes it in.  This is compile-time dispatch --- the dictionary is
determined statically, so there's no runtime overhead beyond the dictionary
passing itself (which GHC often optimizes away through specialization and
inlining).

This is all very clean.  But the people who designed this also gave it a
property that turned out to be far more important than anyone realized at the
time.

The Power of Coherence
======================

Coherence means that for any given type, there is _exactly one_ instance of a
typeclass.  You can't have two different `Num Int` instances floating around in
different modules.  This might sound like a limitation, but it is in fact one of
the most important properties of the entire system.

To see why, consider Haskell's `Set` type --- a binary search tree.  Imagine
if, instead of using a typeclass, we passed the comparison function explicitly:

```haskell
Set.member :: (a -> a -> Ordering) -> a -> Set a -> Bool
```

```haskell
mySet = Set.fromList compare [1,5,3,2]

ghci> Set.member compare 3 mySet
True
ghci> Set.member (\x y -> compare y x) 3 mySet
???
```

The `Set` was built with one comparator, but we're searching with another.  The
tree is structured around `compare`, so searching with a reversed comparator
will look in the wrong branches and give the wrong answer.  This is a real class
of bugs --- it's not hypothetical.  It's the same category of error as using the
wrong hash function for a hash map, or the wrong equality for a deduplication
pass.

The typeclass way eliminates this entirely:

```haskell
Set.member :: Ord a => a -> Set a -> Bool
```

Every `Set Int` uses the _same_ `Ord Int` instance, always.  Every time you see
`Set X` in a type signature, you know _exactly_ which `Ord` is being used.
There is no room for mismatch.  This is coherence: one type, one instance,
every `Set` value is safe to use with every `Set` function.

This property is so fundamental that it's worth pausing on.  In a world without
coherence, every container, every sorted structure, every hash-based lookup
becomes a potential source of silent, hard-to-diagnose bugs.  Coherence turns
these into _impossible_ bugs.

Laws and Lawful Abstractions
============================

Pretty early on, people realized typeclasses weren't just interfaces.  They
could carry _laws_.

```haskell
class Monoid m where
    mempty :: m
    (<>) :: m -> m -> m

-- Laws:
-- mempty <> x = x         (left identity)
-- x <> mempty = x         (right identity)
-- (x <> y) <> z = x <> (y <> z)  (associativity)
```

These laws aren't enforced by the compiler (Haskell's type system isn't powerful
enough for that, at least not yet).  But they are the _social contract_ of the
typeclass, and they are what make the entire ecosystem of polymorphic utilities
actually _work_.

Consider what the law of associativity buys us.  Because `(<>)` is associative,
we can:

- **Parallelize** --- split a list in half, fold each half independently, and
  combine the results.  This is the foundation of MapReduce.
- **Chunk** --- process a stream in chunks, accumulating with `(<>)`, and the
  result is the same as processing the whole stream at once.
- **Rebalance** --- restructure a fold tree without changing the result.

And these guarantees hold for _any_ `Monoid`, which means libraries can write
powerful generic utilities:

```haskell
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
mconcat :: Monoid m => [m] -> m
stimes :: (Semigroup a, Integral b) => b -> a -> a
```

The `Writer` monad works because of associative accumulation.  Parallel
fold-map works because associativity lets you split the work arbitrarily.  None
of this would be possible if `Monoid` were just an interface without laws.

The Abstraction Tower
---------------------

`Monoid` is just one example.  Haskell's standard library is built on a tower
of lawful abstractions, each unlocking its own ecosystem of generic operations:

- **Functor** enables `fmap`, `<$>`, `void`, `($>)`.  Laws: identity
  (`fmap id = id`) and composition (`fmap (f . g) = fmap f . fmap g`).
- **Applicative** enables `liftA2`, `*>`, `<*`, `traverse`, `sequence`.  Laws:
  identity, composition, homomorphism, interchange.
- **Monad** enables `>>=`, `>>`, `=<<`, `replicateM`, `when`, `guard`.  Laws:
  left identity, right identity, associativity.
- **Alternative** enables `<|>`, `many`, `some`, `optional`, `asum`.  Laws:
  monoid laws on the applicative structure.

Each of these is a set of operations _plus_ laws, and the laws are what let
library authors write utilities that work correctly for _any_ instance.
Without laws, you'd just have a bag of methods with no guarantees --- and no
reason to write generic code over them.

This was the first major evolution of typeclass usage: from "overload my
operators" to "define algebraic contracts that unlock entire ecosystems of
generic code".

The Newtype Trick
=================

Here's a tension that comes up immediately in practice once you embrace
coherence: what do you do when a type has _more than one_ valid instance of a
typeclass?

What's the `Monoid` instance for `Int`?  Addition?

```haskell
mempty = 0
(<>) = (+)
```

Or multiplication?

```haskell
mempty = 1
(<>) = (*)
```

Both are perfectly valid monoids.  They both satisfy all the laws.  But
coherence says we can only have one.

The solution is one of Haskell's most elegant idioms: the _newtype wrapper_.

```haskell
newtype Sum a = Sum a

instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    Sum x <> Sum y = Sum (x + y)

newtype Product a = Product a

instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x <> Product y = Product (x * y)
```

Different types, different instances.  Coherence is preserved, and we can
choose which monoid we want by choosing which wrapper to use:

```haskell
ghci> foldMap Sum [1,2,3,4]
Sum 10
ghci> foldMap Product [1,2,3,4]
Product 24
ghci> foldMap (First . Just) [1,2,3,4]
First (Just 1)
```

The key insight is that a newtype is _structurally identical_ to the type it
wraps --- same runtime representation, zero overhead --- but it's a _different
type_ as far as the compiler is concerned, and so it can have different
typeclass instances.  This is one of those ideas that seems almost trivially
simple but turns out to be extraordinarily powerful in practice.

Newtypes Everywhere
-------------------

This pattern shows up everywhere once you start looking.  `Down` gives you
reversed `Ord`:

```haskell
ghci> sort [1,5,3,2]
[1,2,3,5]
ghci> sortBy (comparing Down) [1,5,3,2]
[5,3,2,1]
```

`Ap` and `Backwards` give you different `Applicative` behavior:

```haskell
newtype Ap f a = Ap (f a)          -- Use Applicative instance instead of Monad
newtype Backwards f a = Backwards (f a)  -- Reverse effect order
```

```haskell
ghci> traverse print [1,2,3]
1
2
3
ghci> forwards $ traverse (Backwards . print) [1,2,3]
3
2
1
```

Monad Transformers Are Newtypes
-------------------------------

If you've used monad transformers, you've already been using this pattern
without necessarily thinking about it this way.  `MaybeT` is just a newtype
over `m (Maybe a)`:

```haskell
newtype MaybeT m a = MaybeT (m (Maybe a))
```

`MaybeT IO a` is _exactly_ `IO (Maybe a)` at runtime.  But the newtype gives
it completely different `Monad` and `Alternative` instances:

```haskell
-- >>= short-circuits on Nothing
action :: MaybeT IO String
action = do
    x <- MaybeT $ pure Nothing      -- stops here!
    y <- MaybeT $ print "never prints" >> pure (Just "hello")
    pure (x ++ y)

-- <|> short-circuits on success
fallback :: MaybeT IO String
fallback = MaybeT (pure (Just "success"))
       <|> MaybeT (print "never prints" >> pure Nothing)
```

Every monad transformer is fundamentally the same trick: take a type, wrap it
in a newtype, and give it new instances that provide the behavior you want.
`StateT s m a` is `s -> m (a, s)`.  `ReaderT r m a` is `r -> m a`.  `ExceptT
e m a` is `m (Either e a)`.  The newtype is doing all of the heavy lifting.

Associated Types
================

As people's ambitions grew, they found that sometimes a typeclass needs to talk
about types that are _related to_ the instance type but not the same.  Type
families within typeclasses --- associated types --- let you express this:

```haskell
class Container c where
    type Elem c
    empty :: c
    insert :: Elem c -> c -> c
    member :: Elem c -> c -> Bool
```

```haskell
instance Container (Set a) where
    type Elem (Set a) = a
    empty = Set.empty
    insert = Set.insert
    member = Set.member
```

The `Elem` type family maps each container type to its element type.

### MonoFoldable: In the Wild

A great real-world example is `MonoFoldable` from the *[mono-traversable][]*
package.  Haskell's standard `Foldable` class works on `* -> *` types like
`[]` and `Maybe`, but some containers have a fixed element type: `Text` always
contains `Char`, `ByteString` always contains `Word8`, `IntSet` always contains
`Int`.

[mono-traversable]: https://hackage.haskell.org/package/mono-traversable

`MonoFoldable` uses an associated type to handle this:

```haskell
class MonoFoldable mono where
    type Element mono
    ofoldMap :: Monoid m => (Element mono -> m) -> mono -> m
    ofoldr :: (Element mono -> b -> b) -> b -> mono -> b
    -- ...
```

```haskell
instance MonoFoldable Text where
    type Element Text = Char

instance MonoFoldable ByteString where
    type Element ByteString = Word8

instance MonoFoldable IntSet where
    type Element IntSet = Int
```

Without associated types, you'd either have to give up on folding these types
generically, or reach for something much more convoluted.

Liskov Substitution via Constraints
===================================

Haskell's typeclass hierarchy creates a form of subtyping through constraint
implications.  If `Monad` implies `Applicative`, then any `Monad` can be used
wherever an `Applicative` is expected:

```haskell
processApplicative :: Applicative f => f Int -> f Int
processApplicative = fmap (* 2)

useWithMonad :: IO Int -> IO Int
useWithMonad = processApplicative  -- works!
```

This is Liskov substitution at the constraint level, and it shows up in
practice all the time.  The *[lens][]* library is built almost entirely on this
principle: a `Lens` can be used as a `Traversal`, a `Traversal` as a `Fold`, a
`Fold` as a `Getter`.  The hierarchy of optic types is a hierarchy of
constraints, and the subtyping falls out naturally.

[lens]: https://hackage.haskell.org/package/lens

Classy Lenses
-------------

A related pattern worth mentioning is `makeClassy` from *lens*.  With
`makeLenses`, you generate lenses for each field and have to export them
individually:

```haskell
data Config = Config
    { _baseUrl :: String
    , _dbPort :: Int
    , _apiKey :: String
    , _timeout :: Int
    , _retries :: Int
    }

makeLenses ''Config
-- generates: baseUrl, dbPort, apiKey, timeout, retries
```

Your export list gets long:

```haskell
module Foo (Config(..), baseUrl, dbPort, apiKey, timeout, retries) where
```

With `makeClassy`, you instead generate a _typeclass_:

```haskell
data Config = Config
    { _configBaseUrl :: String
    , _configDbPort :: Int
    , _configApiKey :: String
    , _configTimeout :: Int
    , _configRetries :: Int
    }

makeClassy ''Config
```

This generates:

```haskell
class HasConfig a where
    config :: Lens' a Config
    configBaseUrl :: Lens' a String
    configDbPort :: Lens' a Int
    -- ... all fields as methods with default implementations
```

Now the export is clean: `module Foo (HasConfig(..)) where`.  But more
importantly, any _larger_ config type that contains a `Config` can also get a
`HasConfig` instance for free, and all functions that work with `HasConfig`
will work with the larger type too.  This is subtyping through typeclasses
again.

Where People Go Wrong
=====================

With so many uses, it's natural that people overreached.  The community's
collective experience has identified a number of places where typeclasses are
tempting but actively harmful.  The question of "should this be a typeclass?"
is one of the most important design questions you'll face, and getting it wrong
causes real pain.

Let me lay out the principles I use to evaluate whether something should be a
typeclass:

1. **Coherence must be meaningful** --- Is there genuinely one "correct"
   instance per type?  If you can imagine multiple valid instances for the same
   type, a typeclass might be the wrong tool.
2. **Laws should exist** --- Can you state algebraic properties that all
   instances must satisfy?  Laws are what make polymorphic code over your
   typeclass actually _useful_.
3. **Polymorphic code must be writable** --- Can someone write a function with
   your typeclass as a constraint and have it do something meaningful?  If every
   call site just uses a concrete type, you don't need a typeclass.

Let's apply these to some real cases.

Good: Serialization with a Specific Format
------------------------------------------

```haskell
class Serialise a where
    encode :: a -> ByteString
    decode :: ByteString -> Either String a
```

This is good when tied to a _specific_ format (like [CBOR][] in the
*[serialise][]* library):

[CBOR]: https://cbor.io/
[serialise]: https://hackage.haskell.org/package/serialise

- For any given type, there's only one correct CBOR representation
- Coherence matters: everyone serializing `Foo` should produce the same bytes
- You can write genuinely useful polymorphic code:
  `serializeToFile :: Serialise a => FilePath -> a -> IO ()`

Bad: Serialization without a Specific Format
---------------------------------------------

```haskell
class Binary a where
    put :: a -> Put
    get :: Get a
```

The *[binary][]* library's `Binary` class is less well-motivated as a
typeclass.  What format?  What endianness?  What version of the protocol?
There are many valid implementations for the same type, because the class
itself doesn't commit to a specific wire format.  It is meant for "raw spec of
protocols" but is too unspecified for coherence to be meaningful.

[binary]: https://hackage.haskell.org/package/binary

Better: explicit encoder/decoder values that you pass around, so you can have
different encodings for different protocols without fighting the typeclass
system.

Good: Explicit Generators (Hedgehog)
------------------------------------

[Hedgehog][]-style property testing uses explicit generators:

[Hedgehog]: https://hackage.haskell.org/package/hedgehog

```haskell
genUser :: Gen User
genUser = User
    <$> genName
    <*> Gen.int (Range.linear 0 120)
    <*> genEmail
```

This is explicit and clear.  Different tests can use different generators for
the same type.  A test for edge cases might generate extreme values; a test for
typical behavior might generate realistic distributions.  There's no reason
these should be unified into a single canonical generator.

Bad: Typeclass-Based Generators (QuickCheck)
--------------------------------------------

```haskell
class Arbitrary a where
    arbitrary :: Gen a
```

QuickCheck's `Arbitrary Int` --- should it generate small ints?  Large ints?
Positive ints?  Non-zero ints?  The answer depends entirely on what you're
testing.

The library acknowledges this by providing newtype wrappers like `Large`,
`Small`, `Positive`, and `NonZero`, but nobody writes business logic with those
wrappers.  You end up either using `arbitrary` and hoping it generates useful
values, or ignoring the typeclass entirely and writing explicit generators
anyway.  The typeclass adds almost no value here --- it fails all three
principles.  There's no meaningful coherence (many valid generators per type),
no laws, and polymorphic code over `Arbitrary` is rarely useful.

Borderline: Default Values
--------------------------

```haskell
class Default a where
    def :: a
```

What does "default" mean?  The zero element?  An arbitrary choice?  The most
common value?  The "empty" value?  The answer varies by type in ways that
don't correspond to any coherent semantic.

The *[data-default][]* library provides this, and it sees real use.  But it's
borderline because the lack of clear semantics means you can't reason about
`def` in polymorphic code --- you don't know _what_ you'll get, only that
you'll get _something_.

[data-default]: https://hackage.haskell.org/package/data-default

Borderline: String Conversions
------------------------------

```haskell
class ToString a where
    toString :: a -> String

class IsString a where
    fromString :: String -> a
```

These are ad-hoc: there are many valid ways to convert most types to strings.
What precision for floats?  What format for dates?  What escaping for special
characters?  And conversions like `Text` to/from `ByteString` can fail on
invalid UTF-8, making them partial.

That said, `IsString` sees genuine use for polymorphic string literals (via
`OverloadedStrings`), which is a legitimate convenience.  It's one of those
cases where the practical benefit outweighs the theoretical concerns.

Design Tradeoffs: Equality
--------------------------

Even Haskell's most fundamental typeclasses have design tradeoffs.  Consider
`Eq`:

```haskell
instance Eq Double where
    (==) = primEqDouble

ghci> (0/0) == (0/0)
False  -- NaN /= NaN
```

This violates reflexivity (`x == x` should be `True`).  IEEE 754 floating
point semantics say `NaN /= NaN`, which means `Double`'s `Eq` instance doesn't
satisfy the laws of an equivalence relation.

Rust handled this better by splitting the concept in two:

```rust
trait PartialEq {
    fn eq(&self, other: &Self) -> bool;
}

trait Eq: PartialEq {}  // Eq is PartialEq + full equivalence relation
```

`f64` implements `PartialEq` but not `Eq`, which means you can't use floats as
keys in a `HashSet` or `BTreeMap` without wrapping them in a newtype.  Rust
forces you to be explicit about the tradeoff.

Haskell's approach --- pretending `Double` has a well-behaved `Eq` --- is
pragmatic but leads to real bugs.  It's a genuine design tradeoff: convenience
for the common case at the cost of correctness guarantees in edge cases.

### The Other Extreme: PureScript's Numeric Hierarchy

PureScript went the opposite direction with its fine-grained numeric hierarchy:

```purescript
class Semiring a              -- Has + and *
class Semiring a <= Ring a                    -- Also has -
class Ring a <= CommutativeRing a             -- Commutative *
class CommutativeRing a <= IntegralDomain a   -- Cancellation law
class IntegralDomain a <= EuclideanRing a     -- Has mod and div
class Ring a <= DivisionRing a                -- Multiplicative inverse
class DivisionRing a <= Field a               -- Commutative division
```

This is technically more precise --- you know _exactly_ which algebraic
properties each type satisfies.  But in my experience, I've never been able to
remember where an operation comes from.  Is `div` in `EuclideanRing` or
`IntegralDomain`?  Which one implies `CommutativeRing`?  The cognitive overhead
of the fine-grained hierarchy often outweighs the precision it provides.

There's a sweet spot between too coarse and too fine, and it's hard to find.

Anti-Pattern: Ad-Hoc Container Interfaces
-----------------------------------------

```haskell
class MutableRef r where
    newRef :: a -> IO (r a)
    readRef :: r a -> IO a
    writeRef :: r a -> a -> IO ()
```

On the surface, this looks reasonable --- abstract over different mutable
reference types.  But the implementations have wildly different semantics:

- `IORef`: No synchronization at all
- `MVar`: Blocking, can be empty, has fairness guarantees
- `TVar`: Transactional, can retry, composes with other STM operations

These aren't interchangeable!  Code written against `MutableRef` will behave
completely differently depending on which instance it runs with, in ways that
matter deeply for correctness.  An `IORef`-based solution might have data
races; an `MVar`-based one might deadlock; a `TVar`-based one might live-lock
on retries.

The key question: is it possible to write polymorphic code over this typeclass
and actually _reason_ about what it does?  If you can't, the typeclass is
worse than useless --- it's actively misleading, because it suggests an
interchangeability that doesn't exist.

Anti-Pattern: Map Interfaces
----------------------------

```haskell
class MapLike m where
    empty :: m k v
    insert :: k -> v -> m k v -> m k v
    lookup :: k -> m k v -> Maybe v
    toList :: m k v -> [(k, v)]
```

Between `Map`, `HashMap`, and `IntMap`:

- **Ordering guarantees** differ: `Map` iterates in key order; `HashMap` has
  arbitrary, non-deterministic iteration order
- **Strictness semantics** differ
- **Performance characteristics** differ (which is fine for a typeclass, but
  combined with the above, it's a problem)

Watch what happens:

```haskell
buildMap :: MapLike m => m Int String
buildMap = insert 3 "c" $ insert 1 "a" $ insert 2 "b" empty

ghci> toList (buildMap :: Map Int String)
[(1,"a"), (2,"b"), (3,"c")]       -- Always sorted

ghci> toList (buildMap :: HashMap Int String)
[(2,"b"), (3,"c"), (1,"a")]       -- Arbitrary, can change between runs!
```

You can't write polymorphic code that works correctly over all three, because
`toList`'s behavior is observably different.  This isn't about performance
--- it's about _semantic_ differences.  Better: just use the specific container
type you need.  The "abstraction" here doesn't abstract over anything
meaningful.

Anti-Pattern: Effect Typeclasses
--------------------------------

```haskell
class MonadLogger m where
    logInfo :: String -> m ()
    logError :: String -> m ()

class MonadDB m where
    query :: Query -> m [Row]
    execute :: Command -> m ()
```

This is controversial in the Haskell community, and I know I'm going to get
pushback here.  But I think in most cases, you should just pass the handle:

```haskell
myFunction :: Logger -> DBConnection -> IO Result
```

Why?

- Handles are _values_: you can create them, pass them, store them, inspect
  them.  Typeclass constraints are _not_ values --- they're compiler-resolved
  and invisible at runtime.
- Handles compose naturally: you can have a `Logger` that wraps another
  `Logger`, or a `DBConnection` pool.  Typeclass dispatch doesn't give you this
  flexibility.
- Effect typeclasses add indirection without clear semantics.  What _are_ the
  laws of `MonadLogger`?  What guarantees does it give you?  Usually: none.

That said, some MTL-style classes _do_ work well.  `MonadState` has clear
semantics, well-defined laws (get-put, put-get, put-put), and a functional
dependency that means the monad determines the state type unambiguously.  The
key difference is that `MonadState` describes a _computational pattern_ with
algebraic properties, not just a bag of operations.

But `MonadWriter` and `MonadError`?  The semantics are murkier --- different
people expect different behavior around how errors interact with accumulated
output, or whether `catchError` should roll back writer state.  When the
semantics are unclear, the typeclass is doing more harm than good.

Design Tradeoff: Foldable for Tuples
-------------------------------------

The `Foldable` instance for `((,) w)` is one of the most contentious instances
in the standard library:

```haskell
instance Foldable ((,) w) where
    foldMap f (_, y) = f y
```

This looks innocuous until you run into it in practice:

```haskell
ghci> let xs = [1,2,3] in length xs
3
ghci> let xs = (1, [1,2,3]) in length xs
1  -- counts the tuple's foldable elements, not the list!
```

The issue is that refactoring from a list to a tuple with context shouldn't
silently compile.  The tuple only has ONE foldable element (the second
component), regardless of what it contains.  This leads to surprises:

```haskell
ghci> sum (10, 5)
5  -- ignores the 10!
```

But we _need_ `Foldable` for tuples in order to have `Traversable` for tuples,
which is genuinely useful (it lets you apply an effectful function to the second
component while preserving the first).  This is a real design tradeoff:
convenience and compositional consistency versus safety from silent bugs.

Algebraic Typeclasses and Generic Programming
=============================================

Around the same time people were discovering what typeclasses _shouldn't_ be
used for, others were discovering a deeper structural property that made them
far more powerful: algebraic compositionality.

A typeclass is _algebraic_ if its instances compose over products (tuples) and
sums (`Either`).  This might sound abstract, but it's surprisingly important,
because it determines whether instances can be _automatically derived_.

Using `Binary` as an example (setting aside my earlier criticism of it as a
typeclass --- the algebraic property is orthogonal to the design question):

```haskell
-- Products: instances compose over tuples
instance (Binary a, Binary b) => Binary (a, b) where
    put (x, y) = put x >> put y
    get = (,) <$> get <*> get

-- Sums: instances compose over Either
instance (Binary a, Binary b) => Binary (Either a b) where
    put (Left x)  = putWord8 0 >> put x
    put (Right y) = putWord8 1 >> put y
    get = getWord8 >>= \tag -> case tag of
        0 -> Left <$> get
        1 -> Right <$> get
```

If you can handle products and sums, you can handle _any_ algebraic data type,
because every ADT is built from products and sums.  This is what makes
`Generic` derivation possible, and it's one of the most consequential
realizations in the history of typeclass usage.

Ix: Products Only
-----------------

Some typeclasses are algebraic over products but not sums.  `Ix`, for building
index ranges, composes over tuples:

```haskell
class Ix a where
    range :: (a, a) -> [a]
    index :: (a, a) -> a -> Int

instance (Ix a, Ix b) => Ix (a, b)

ghci> range ((1,0), (3,2))
[(1,0),(1,1),(1,2),(2,0),(2,1),(2,2),(3,0),(3,1),(3,2)]
```

This lets you build multi-dimensional indices from single-dimensional ones.
But there's no natural `Ix` instance for `Either` --- what would the "range" of
`Left 1` to `Right 5` be?

Finitary: Products AND Sums
----------------------------

The *[finitary][]* library's `Finitary` class is fully algebraic --- it works
with both products and sums:

[finitary]: https://hackage.haskell.org/package/finitary

```haskell
class Finitary a where
    type Cardinality a :: Nat
    toFinite :: a -> Finite (Cardinality a)
    fromFinite :: Finite (Cardinality a) -> a
```

The cardinality composes algebraically:

- `Cardinality (Either a b) = Cardinality a + Cardinality b`
- `Cardinality (a, b) = Cardinality a * Cardinality b`

And you can enumerate all inhabitants of any `Finitary` type:

```haskell
ghci> inhabitants :: [(Bool, Bool)]
[(False,False), (False,True), (True,False), (True,True)]

ghci> inhabitants :: [Either Bool Bool]
[Left False, Left True, Right False, Right True]

ghci> inhabitants :: [(Bool, Either Bool Bool)]
[(False,Left False), (False,Left True), (False,Right False), ...]
-- 2 * (2 + 2) = 8 inhabitants
```

Generic: The Automated Solution
-------------------------------

If a typeclass is algebraic, we can derive it automatically using `Generic`.
The *[GHC.Generics][]* system represents any algebraic data type as a
combination of sums (`:+:`) and products (`:*:`):

[GHC.Generics]: https://hackage.haskell.org/package/base/docs/GHC-Generics.html

```haskell
data Shape = Circle Double | Rectangle Double Double

-- The Generic representation:
type Rep Shape =
    Double                     -- Circle
    :+:                        -- OR
    (Double :*: Double)        -- Rectangle (product of two Doubles)
```

Once you write a single generic instance that handles `:+:` and `:*:`, it works
for _all_ algebraic types:

```haskell
data User = User { name :: String, age :: Int }
  deriving Generic

instance Binary User      -- Derived automatically!
instance ToJSON User
instance FromJSON User
```

The `Generic` representation is the bridge between the algebraic structure of
your types and the algebraic composition of typeclass instances.  You define
the logic once for sums and products, and GHC handles the rest.

This was a major milestone in the history of typeclass usage: typeclasses
became the mechanism for _automatic code generation_, driven by the algebraic
structure of types themselves.

Bridging Types and Values
=========================

Perhaps the most surprising chapter in the history of typeclasses is their
adoption as the _bridge_ between type-level and term-level computation.
This is territory that most other languages' type systems can't reach, and
typeclasses turned out to be the natural mechanism for it.

KnownNat: Type-Level Numbers at Runtime
----------------------------------------

`KnownNat` is the canonical example:

```haskell
newtype Vec (n :: Nat) a = Vec (Vector a)

replicate :: KnownNat n => a -> Vec n a
```

The type says the length is `n`, but we need the actual integer at runtime to
call `Vector.replicate`.  `KnownNat` provides the bridge:

```haskell
class KnownNat (n :: Nat) where
    natVal :: proxy n -> Integer

replicate :: KnownNat n => a -> Vec n a
replicate x = Vec (V.replicate (fromIntegral $ natVal (Proxy @n)) x)
```

The typeclass constraint `KnownNat n` means "the compiler knows what `n` is and
can give it to you as a runtime `Integer`".  This is the fundamental mechanism
for connecting type-level computation to runtime computation, and it shows up in
any library that does type-level numerics.

Typeable: Runtime Type Information
----------------------------------

`Typeable` provides runtime type representations:

```haskell
class Typeable a where
    typeRep :: proxy a -> TypeRep
```

This enables dynamic typing and type-safe casting:

```haskell
cast :: (Typeable a, Typeable b) => a -> Maybe b
```

Use cases include serialization with type information, dynamic loading of
plugins, and heterogeneous collections.  Every concrete type in GHC
automatically has a `Typeable` instance (you can't write them yourself), which
ensures that `TypeRep` faithfully represents the actual type.

Singletons: Type-Level Witnesses
---------------------------------

Here's a more advanced pattern.  Suppose you have a door that can be in
different states:

```haskell
data DoorState = Opened | Closed | Locked
```

And you want `openDoor` to behave differently depending on the door's state.
But the state exists only at the _type_ level:

```haskell
openDoor :: SingI s => Door s -> Maybe (Door 'Opened)
```

How do you branch on `s` if it's a type, not a value?  You need a _singleton_
--- a type that carries a type-level value down to the term level:

```haskell
data Sing (s :: DoorState) where
    SOpened :: Sing 'Opened
    SClosed :: Sing 'Closed
    SLocked :: Sing 'Locked

class SingI (s :: DoorState) where
    sing :: Sing s

instance SingI 'Opened where sing = SOpened
instance SingI 'Closed where sing = SClosed
instance SingI 'Locked where sing = SLocked
```

Now we can case-analyze the type:

```haskell
openDoor :: SingI s => Door s -> Maybe (Door 'Opened)
openDoor door = case sing @s of
    SOpened -> Just door
    SClosed -> Just (openClosed door)
    SLocked -> Nothing
```

The `SingI` typeclass provides the bridge: given a type-level `DoorState`, it
gives you the corresponding term-level witness.  I go into much more detail
about this in my [Introduction to Singletons][singletons] series.

[singletons]: https://blog.jle.im/entries/series/+introduction-to-singletons.html

In practice, you wouldn't write the boilerplate yourself.  The
*[singletons][singletons-lib]* library uses Template Haskell to generate
everything from your data type declaration:

[singletons-lib]: https://hackage.haskell.org/package/singletons

```haskell
$(singletons [d|
    data DoorState = Opened | Closed | Locked
  |])
```

Reflection: Runtime to Type-Level
---------------------------------

`KnownNat` and `SingI` let you go from types to values.  The
*[reflection][]* library's `Reifies` class goes in the _opposite_ direction:
it lets you promote a runtime value into a type-level constraint.

[reflection]: https://hackage.haskell.org/package/reflection

```haskell
class Reifies s a | s -> a where
    reflect :: proxy s -> a

reify :: a -> (forall s. Reifies s a => Proxy s -> r) -> r
```

The key operation is `reify`: you give it a value and a continuation, and
inside the continuation, that value is available as a type-level constraint.
The `forall s` ensures that the `s` is fresh --- it can't escape the
continuation or be confused with any other reified value.

### Enforcing Coherence with Reflection

One use is ensuring that a configuration is consistent across a computation:

```haskell
data Config = Config { baseUrl :: String, dbPort :: Int }

fetchUser :: Reifies s Config => Proxy s -> UserId -> IO User
fetchUser p uid = do
    let Config url port = reflect p
    -- Use url and port...

main = do
    config <- loadConfig
    reify config $ \p -> do
        user1 <- fetchUser p 123
        user2 <- fetchUser p 456  -- Guaranteed same config!
```

The type system guarantees that all code using `Proxy s` within the same
`reify` block sees the _same_ config.  With normal parameters, you could
accidentally pass different configs to different calls.  With `reify`, the `s`
type parameter ties everything together.

### Picking Typeclass Instances at Runtime

The deepest application of `reflection` is choosing _which_ typeclass instance
to use at runtime.  Normally, typeclass instances are fixed at compile time.
But what if you want to choose a `Monoid` strategy based on user input?

```haskell
data LogStrategy = AllLogs | FirstLog | LastLog | LastN Int | Reverse
```

You can't do this with normal newtypes:

```haskell
main = do
    strategy <- getStrategyFromArgs
    -- Doesn't typecheck! Different branches have different types:
    case strategy of
        AllLogs  -> runWriter (processData :: Writer [String] Int)
        FirstLog -> runWriter (processData :: Writer (First [String]) Int)
        LastLog  -> runWriter (processData :: Writer (Last [String]) Int)
```

Each branch has a different type, and `LastN` and `Reverse` don't even _have_
corresponding newtypes.

The solution is to define a type whose `Monoid` instance is determined by a
reified value:

```haskell
data ReifiedMonoid a = ReifiedMonoid
    { reifiedMappend :: a -> a -> a
    , reifiedMempty :: a
    }

newtype ReflectedMonoid s a = RM a

instance Reifies s (ReifiedMonoid a) => Semigroup (ReflectedMonoid s a) where
    RM x <> RM y = RM (reifiedMappend (reflect (Proxy @s)) x y)

instance Reifies s (ReifiedMonoid a) => Monoid (ReflectedMonoid s a) where
    mempty = RM (reifiedMempty (reflect (Proxy @s)))
```

Now we can choose the combining function at runtime:

```haskell
processData :: Reifies s (ReifiedMonoid [String])
            => Writer (ReflectedMonoid s [String]) Int
processData = do
    tell (RM ["Starting"])
    tell (RM ["Processing"])
    tell (RM ["Done"])
    return 42

main = do
    strategy <- getStrategyFromArgs
    let monoidOp = ReifiedMonoid
            { reifiedMappend = case strategy of
                AllLogs  -> (++)
                FirstLog -> const
                LastLog  -> flip const
                LastN n  -> \xs ys -> take n (xs ++ ys)
                Reverse  -> \xs ys -> reverse xs ++ reverse ys
            , reifiedMempty = []
            }
    reify monoidOp $ \p -> runWriter (processData @p)
```

This is one of the most mind-bending typeclass techniques: using `reflection`
to bridge the gap between runtime values and compile-time instance selection.
We could have an entire post about `reflection` alone.

Looking at the Full Picture
===========================

Let's step back and look at what we've covered, as a rough chronology of how
typeclasses evolved from a simple mechanism into something nobody anticipated:

1. **Operator overloading** --- The original use case.  Make `+` work on
   different types.
2. **Coherent interfaces** --- The realization that one-type-one-instance
   guarantees, like `Set` with `Ord`, prevent entire classes of bugs.
3. **Lawful abstractions** --- `Functor`, `Monad`, `Monoid`, etc.  Laws let
   library authors write generic code that works _correctly_ for any instance.
4. **Newtype-driven dispatch** --- The discovery that zero-cost newtype wrappers
   let you have multiple "instances" for the same underlying type while
   preserving coherence.
5. **Associated types** --- Typeclasses that talk about related types, enabling
   things like `MonoFoldable`.
6. **Generic programming** --- The realization that algebraic compositionality
   lets you _derive_ instances automatically for any ADT.
7. **Type-level/runtime bridges** --- `KnownNat`, `Typeable`, singletons.
   Typeclasses as the mechanism for connecting type-level and term-level
   computation.
8. **Reflection** --- Going the other direction: promoting runtime values to
   type-level constraints, enabling runtime selection of typeclass instances.

All from wanting to overload `+`.

Principles
----------

Through all of this history, a few design principles have emerged:

1. **Coherence matters** --- one type, one instance.  If you can imagine
   multiple valid instances, use newtypes or explicit values instead.
2. **Laws enable reasoning** --- lawful abstractions unlock ecosystems of
   generic code.  Without laws, polymorphic code over your typeclass is
   meaningless.
3. **Prefer explicit over implicit** --- pass handles when you can, use
   typeclasses when you must.
4. **Newtypes for multiple instances** --- different behaviors deserve different
   types, and newtypes are free.
5. **Clear semantics or don't bother** --- ambiguity is the enemy of useful
   abstraction.
6. **Don't over-abstract** --- not everything needs a typeclass.  Sometimes a
   function is just a function.

The lesson I take from this history is that good language design doesn't just
solve the problem at hand --- it creates a foundation that enables solutions to
problems nobody anticipated.  Typeclasses are one of Haskell's most successful
examples of this phenomenon.

Use them for coherent interfaces with clear semantics.  Use them for lawful
abstractions that enable ecosystems.  Use them for generic programming and
type-level computation.  And don't use them when you just need a function.

Special Thanks
--------------

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts.  Very special
thanks to my supporter at the "Amazing" level on [patreon][], Josh Vera! :)

[patreon]: https://www.patreon.com/justinle/overview
