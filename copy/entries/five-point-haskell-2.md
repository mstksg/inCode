---
title: "\"Five Point Haskell\": Unconditional Election (via Parametricity)"
categories: Haskell
tags: functional programming, parametric polymorphism
create-time: 2026/01/01 21:51:17
identifier: five-point-haskell-2
slug: five-point-haskell-part-2-unconditional-election
series: five-point-haskell
---

Welcome back to *[Five-Point Haskell][]*! This is my attempt to codify
principles of writing robust, maintainable, correct, clear, and effective code
in Haskell and to dispel common bad practices or heresies I have run into in my
time.

[Five-Point Haskell]: https://blog.jle.im/entries/series/+five-point-haskell.html

In the last post, we talked about [Total Depravity][], which is about treating
any mentally-tracked constraint or condition as inevitably leading to a
catastrophe and denouncing the reliance on our flawed mental context windows.

[Total Depravity]: https://blog.jle.im/entry/five-point-haskell-part-1-total-depravity.html

However, stopping here gives us an incomplete picture. Firstly, types aren't
just about preventing bad behaviors. They're about designing good code.
Secondly, there is only so much you can do by picking careful structures and
making invalid states unrepresentable. These are still human tools with human
flaws.

The next point, to me, is about an aspect of the type system that I see little
coverage of, but is a principle of design that I reach for in almost everything
I write. It's about leveraging the unyielding properties of math _itself_ to
take care of our fate, even when we are unable to structure our types well.

So, when writing Haskell, remember **Unconditional Election**.

> Unconditional Election: The power of the `forall` to elect or reprobate
> instantiations and implementations through parametric polymorphism. These
> properties aren't based on any conditional ad-hoc aspect of types, but are
> truly unconditional, predestined by universal quantification.
>
> Surrender your control to parametric polymorphism in all things. Embrace the
> "free"-dom of "Free Theorems" from one of Haskell's greatest unexpected
> strengths: the type parameter.

Choice is a Prison
------------------

### Conditional Election

Learning Haskell can be a journey full of surprises, but this was one of the
ones that blew my mind the most.

Let's think of a polymorphic function in Java that takes a value of any type
and returns something of that same type:

```java
static <T> T foo(T x)
```

What could that function do?

Well, it could do IO or throw an exception, mutate the input, or possibly be
non-terminating, but let's assume all it (and every other example here) does is
purely return a value without mutation. What could it do?

The answer: pretty much anything. It could return the same value it was given,
except if it is an `Integer`, in which case it negates it:

```java
static <T> T foo(T x) {
  if (x instanceof Integer i) {
    return (T) Integer.valueOf(-i);
  }
  return x;
}
```

Same deal in TypeScript or pretty much any other typed language without
parametricity:

```typescript
function foo<T>(x: T): T
```

Yet again, we have conditional election:

```typescript
function foo<T>(x: T): T {
  if (typeof x === "number") {
    return (-x) as any as T;
  }
  return x;
}
```

But, how about Haskell?

```haskell
foo :: a -> a
```

(For the rest of this post, let's ignore [non-termination][fastandloose] and
other [escape hatches][^hatches])

[^fastandloose]: This is the "Fast and Loose Reasoning" condition, popularized
by [Danielsson et al.][danielsson]
[^hatches]: Excluding `unsafePerformIO`, `unsafeCoerce`, etc.

Because Haskell has type erasure and no runtime reflection, so the _only_
possible implementation is simply

[danielsson]: https://www.cse.chalmers.se/~nad/publications/danielsson-popl2006-tr.pdf

```haskell
foo :: a -> a
foo x = x
```

So there is an invariant that happened here: a function of type `forall a. a ->
a` _must_ leave its value unchanged!

But wait...says who? Did we insert some sort of `const` compiler annotation?
Did we add some sort of annotation or pre- and post-condition that the value
cannot change? Are we relying on any sort of foreseeable property of the value
given?

No. This behavior is intrinsically fixed! We got this theorem _for free_.
Without any need for any sort of works. We didn't even have to _write_ the
function before knowing all it possibly could be.

This is the power of the `forall`, for you. Note that the above `foo :: a -> a`
is "sugar" for:

```haskell
foo :: forall a. a -> a
```

If you don't add a `forall a`, it is implicitly added. Some languages, like
PureScript and Dhall, require the `forall` in every case to be explicit. This is
basically Haskell's equivalent of `<T>` in Java, `template <typename T>` in
C++, etc.

Anyway, let's consider another type signature:

```java
static <T> String foo(T x)
```

In Java (and most other languages), this again could literally be anything. You
can serialize the object with `toString`, or you can get its class using
`getClass`...

```java
static <T> String foo(T x) { return "hello"; }
static <T> String foo(T x) { return x.toString(); }
static <T> String foo(T x) { return x.getClass().getSimpleName(); }
```

In Haskell? A `forall a. a -> String` can _not_ use its input! It _must_ be a
constant string!

```haskell
foo :: a -> String
foo _ = "hello"
-- or
foo _ = "goodbye"
-- or
foo _ = "i always ignore my input"
```

In fact, there is an isomorphism between `String` and `forall a. a -> String`
(fun exercise: write it!)

You can "selectively" bring in capabilities using typeclasses:

```haskell
foo :: Show a => a -> String
foo _ = "hello"
-- or
foo x = show x
-- or
foo x = "reversed: " <> reverse (show x)
```

But there are still properties you can enforce: the resulting string can _only_
depend on the input as far as what is revealed in its `Show` instance.
Any property not in its `Show` instance is off-limits.

Alternatively, you can rephrase it as:

```haskell
foo :: (a -> String) -> a -> String
foo showVal x = -- ...
```

and you can see that the only way you can ever inspect `a` is through the
singular inspecting-lens `a -> String` that you are given. No `getClass()`, no
back doors, etc.

### The Guessing Game

One game Haskellers get used to playing is "guess the properties that the
`forall` ensures" on different type signatures. Let's try it out!

For example:

```haskell
mystery :: a -> b -> a
```

`mystery` _has_ to be `\x _ -> x` --- there is no other option.

How about:

```haskell
turmeric :: ((a, b) -> c) -> a -> b -> c
```

If you think about it, the only option is:

```haskell
turmeric :: ((a, b) -> c) -> a -> b -> c
turmeric f x y = f (x, y)
```

You can go pretty far down this lane as a way of [proving theorems][], in that
the type signature represents a claim and the implementation represents a proof
of that claim. But we're not going to go down that route for now, since most
practical code is not theorem-proving.

[proving theorems]: https://blog.jle.im/entry/the-baby-paradox-in-haskell.html#haskell-as-a-theorem-prover

But let's look at something a bit more structural. How about:

```haskell
theThing :: [a] -> [a]
```

What could this do?

Well, we know that all items from the result list _must_ be from the input
list. It must be a subset --- but the ordering can change, or the multiplicity.
And, more importantly, it _can't_ depend on anything about the properties of any `a`.
We also know that if the input is empty, so must be the output.

From this, we can derive what are called [free theorems][] to look at
properties that _any implementation_ must take. Namely, _mapping_ a function
over the list and calling `theThing` must be equivalent to calling `theThing`
and then mapping:

[free theorems]: https://people.mpi-sws.org/~dreyer/tor/papers/wadler.pdf

```haskell
theThing . map f
  == map f . theThing
```

Can you see why? Think of any possible implementation --- `reverse`, `take 3`,
etc. --- and see how this must be the case. However, this is _not_ true for,
for example, `sort :: [Int] -> [Int]`. Because `sort` depends on the actual
properties of the items, so `map f` could change the properties that `sort`
depends on!

```haskell
ghci> sort . map abs $ [5,-1,3,-7]
[1,3,5,7]
ghci> map abs . sort $ [5,-1,3,-7]
[7,1,3,5]
```

Compared to an actual function with the polymorphic type, like `take 3`:

```haskell
ghci> take 3 . map abs $ [5,-1,3,-7]
[5,1,3]
ghci> map abs . take 3 $ [5,-1,3,-7]
[5,1,3]
```

How about:

```haskell
doIt :: [a] -> Maybe a
```

Think about what this _can't_ do. It clearly selects a single item, but:

1.  The single item cannot be determined based on any quality or merit of that
    item --- it can't be the smallest, the largest, etc.; it has to purely
    depend on the position in the list and the length of the list
2.  If given an empty list, it _must_ return `Nothing`

And again we have the same free theorem, `doIt . map f == fmap f . doIt`. No
matter how you implement `doIt`, it is guaranteed to commute with `map` and
`fmap`!

```haskell
-- no free theorem: `maximumMay`
ghci> minimumMay . map abs $ [5,-1,3,-7]
Just 1
ghci> map abs . minimumMay $ [5,-1,3,-7]
Just 7

-- free theorem: `listToMaybe`
ghci> listToMaybe . map abs $ [5,-1,3,-7]
Just 5
ghci> map abs . listToMaybe $ [5,-1,3,-7]
Just 5
```

Let's try another one:

```haskell
collapse :: [a] -> Int
```

What could this possibly do? Well, we can rule out things like `sum` because we
can't use any property of the values itself. The only things that this could
return are constant functions and functions that depend on the _length_ but not
the _contents_ of the list. We also have another free theorem, `collapse . map
f == collapse`: mapping a function shouldn't change the output, because none of
the actual values matter.

```haskell
-- no free theorem: `sum`
ghci> sum . map abs $ [5,-1,3,-7]
16
ghci> sum [5,-1,3,-7]
0

-- free theorem: `length`
ghci> length . map abs $ [5,-1,3,-7]
4
ghci> length [5,-1,3,-7]
4
```

How about something in the opposite direction:

```haskell
dupper :: a -> [a]
```

From this type signature, we can conclude that the final list must contain the
_same item_! The only possible inhabitants are `replicate n` for some `n`. In
fact, `forall a. a -> [a]` is isomorphic to the natural numbers
(`Natural`)...see if you can prove it for fun?

Again, we have a free theorem: `map f . dupper == dupper . f`

```haskell
-- no free theorem: `take 3 . iterate (+1)`
ghci> take 3 . iterate (+1) . negate $ 4
[-4,-3,-2]
ghci> map negate . take 3 . iterate (+1) $ 4
[-4,-5,-6]

-- free theorem: `replicate 3`
ghci> replicate 3 . negate $ 4
[-4,-4,-4]
ghci> map negate . replicate 3 $ 4
[-4,-4,-4]
```

One final one, with a higher-rank variable:

```haskell
traverseIO :: (a -> IO b) -> [a] -> IO [b]

-- vs

traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
```

What invariant does the second add over the first? Even _if_ you only ever plan
on calling things with `IO`, the second gives you a new invariant: there won't
be any "stray" `IO` actions other than what is given in the `a -> f b`. In the
first one, you never know if the resulting `IO` action might include a
`putStrLn "hello"` or a `launchMissiles`. You definitely don't want any
functions doing sneaky IO behind your back!

### The More you Surrender

Practically, this becomes similar to the principle of least power, the idea
that you should use the tools with the least power necessary to do your job.
Say you _are_ writing a function that shuffles a list of items, important for
your business logic. You can encode exactly _what_ business logic is being done
by adding more and more parametricity.

*   If your type is `[Int] -> [Int]`, you know your function has pretty much no
    restriction on what it can do. It can even look at the machine
    representation of your values.
*   If your type is `Num a => [a] -> [a]`, you know that it can possibly
    numerically transform the items in your list, or even conjure up new items.
*   If your type is `Ord a => [a] -> [a]`, you know that your business logic
    is allowed to look at the ordering between items in the list, but cannot
    return any items that weren't in the original list.
*   If your type is `[a] -> [a]`, you know that your logic can only affect the
    permutation and multiplicity of items in your list.
*   If your type is `Functor f => f Int -> f Int`, you know that the length of
    the result will be preserved, and also any mappings of `Int`s will be done
    purely.
*   If your type is `Monad m => m a -> m a`, if you call with `[]`, you know
    that the lengths of your results will always be integer powers of the
    length of the input.

By switching from concrete types slowly to parametric types, you surrender
control of what your functions can do, and create stronger and stronger
guarantees. In other languages, or with refinement types, you might have to
explicitly declare a post-condition like "the final values must all come from
the original list". With parametric polymorphism, this is already guaranteed
and elected, no matter what the implementation is.

### It's only Natural

As an aside, did you wonder where I got those free theorems from?  In the
examples above, they come from "naturality". Basically, any `forall a. (Functor
f, Functor g) => f a -> g a` corresponds to a [natural transformation][] from
Category Theory, and so must commute with any `fmap`.

[natural transformation]: https://en.wikipedia.org/wiki/Natural_transformation

Basically, if you have a natural transformation `h :: forall a. F a -> G a`, with
`Functor F` and `Functor G`, then we have:

```haskell
h . fmap f
    == fmap f . h
```

The above examples, `forall a. [a] -> [a]`, `forall a. [a] -> Maybe a`, etc.
all fit into this. But you might have to think carefully to see that `forall a.
a -> [a]` is really `forall a. Identity a -> [a]`. And, can you think of the
Functor that gives us naturality for `forall a. [a] -> Int`?[^const]

[^const]: It's `forall a. [a] -> Const Int a`!

Maybe more surprising than the fact that these free theorems exist is the fact
that their root is intrinsically tied to a branch of math as obscure and
esoteric as "category theory"!

Add a Type Variable
-------------------

Let's say I had a data type like:

```haskell
data User = User
    { userId :: Int
    , userName :: String
    , userAge :: Int
    }
```

And we have a function to process the user, like:

```haskell
processUser :: User -> IO User
```

How can we enforce that the `userId` is not changed?

Maybe if we were in C, we could have a `const` field:

```c
struct User {
  const int userId;
  char userName[20];
  int userAge;
};
```

But, this applies to _all_ usage of the `User` struct...what if we only wanted
to preserve this property on a single function? You can't declare struct-level
`const` on a single argument!

Instead, we can enforce this by making `userId`'s type parameterized:

```haskell
data User uid = User
    { userId :: uid
    , userName :: String
    , userAge :: Int
    }

-- | Guaranteed not to change the ID
processUser :: User uid -> IO (User uid)
```

Is this constraint enforced because we carefully designed the structure of our
type? Is it constrained because we added compiler annotations or refinement
types or static analysis? Not quite! It truly did come for free.

Or, consider a checklist item:

```haskell
data Checklist = Checklist
  { updated :: UTCTime
  , items :: [(Status, String)]
  }
```

What if I wanted to write a function that processed items without adding or
removing any? Just each item in-place?

```haskell
-- | Invariant: Preserves the ordering of items, and their number.
updateItems :: Checklist -> IO Checklist
```

How can we make sure all our implementations are _elected_ to only be
implementations that don't modify the length of `items`?

Again the answer can be: add quantification!

```haskell
data Checklist t = Checklist
  { updated :: UTCTime
  , items :: t (Status, String)
  }

-- | Guaranteed not to add or remove or re-arrange items, but can still perform
-- IO to get the new Status and String
updateItems :: Traversable t => Checklist t -> IO (Checklist t)
```

For an example of a possible implementation:

```haskell
updateSingleItem :: (Status, String) -> IO (Status, String)

updateItems :: Traversable t => Checklist t -> IO (Checklist t)
updateItems c0 = do
  newItems <- traverse updateSingleItem (items c0)
  newUpdated <- getCurrentTime
  pure (Checklist newUpdated newItems)
```

Try as you might, you can't make an implementation that rearranges the items.

You can add even further guarantees: what if we wanted `updateItems` to only
apply _pure_ functions to the checklist items? In that case, we can pick:

```haskell
-- | Guaranteed not to add or remove or re-arrange items, and can only get the
-- Status and String purely
updateItems :: Functor t => Checklist t -> IO (Checklist t)
```

And an example:

```haskell
updateSingleItem :: (Status, String) -> (Status, String)

updateItems :: Functor t => Checklist t -> IO (Checklist t)
updateItems c0 = do
  let newItems = fmap updateItem (items c0)
  newUpdated <- getCurrentTime
  pure (Checklist newUpdated newItems)
```

Note, we are _not_ adding type parameters for abstraction or to be able to use
"exotic checklists" (`Checklist Maybe`). Instead, we are intentionally using
them universally quantified in functions that process them, in order to take
advantage of these automatically enforced properties.

This intersects a lot with the [Higher-Kinded Data][hkd] pattern. Maybe we _do_
have data we want to have multiple structural versions of:

[hkd]: https://reasonablypolymorphic.com/blog/higher-kinded-data/

```haskell
data UserF f = User
    { userName :: f String
    , userAge :: f Int
    }

type User = UserF Identity
type NullableUser = UserF Maybe
type UserParser = UserF Parser
type UserDocs = UserF (Const Doc)
type UserPrinter = UserF (Op String)
```

In this case, a function like

```haskell
processUser :: Functor f => UserF f -> UserF f
```

will give you a different, unique guarantee for every "shape" your user has:

*   For `UserF Maybe`, the quantification ensures that the null-or-present
    property of each field is preserved
*   For `UserF Parser`, it ensures that all of the "parsing" logic, and the set
    of strings that are validly parsed, is preserved
*   For `UserF (Const Doc)`, it ensures that the per-field `Doc`/documentation
    is never changed or updated.

For an example, we can write:

```haskell
processUser :: Functor f => User f -> User f
processUser user = User
    { userName = fmap (map toUpper) (userName user)
    , userAge = fmap (+ 1) (userAge user)
    }
```

This is guaranteed to keep nullable fields null if `UserF Maybe`, preserve all
successful parses if `UserF Parser`, and leave any field-level documentation
unchanged if `UserF (Const Doc)`.

All of these properties are _mathematically_ enforced, _unconditionally_. It
doesn't depend on any foreseen property of the types or values we use. These
guarantees free us to be able to confidently use these functions without
fear of invariants breaking.

This game becomes even stronger when you consider dependent typing, where we
can express more complex relationships between type variables. For example, in
the case where you have a phantom type (like in [this singletons
tutorial][singletons]):

[singletons]: https://blog.jle.im/entry/introduction-to-singletons-1.html

```haskell
data DoorState = Opened | Closed | Locked

data Door (s :: DoorState)

processDoor :: Door s -> IO (Door s)
```

`processDoor`, by nature of taking `forall s`, _must leave_ the door state
unchanged! It can never open a closed door, unlock a locked door, etc.

For things like [fixed length vectors][vectors], where the length `n` parameter
is the size, what invariant do you think is preserved in:

[vectors]: https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html

```haskell
something :: Vector n a -> Vector n a
```

We know that the length of the result must be the same as the length of the
input. Furthermore, with the `forall a`, we know that every item in the result
must come from the input, but we might rearrange or change the multiplicity of
the occurrences as long as they add to the same original total number. This
might be a good candidate for a function like `reverse`.

Or, consider:

```haskell
somethingElse :: Vector n a -> Vector (n - 1) a
```

From this, we know that the original vector _must_ be non-empty! Because of how
flow of the types must work for whatever `n` you give it, this flow requires
`n - 1 > 0` and so `n > 1`.

Ranking Up
----------

Now that you see how useful it is to use type parameters and `forall`, can we
_use_ this fact at the meta-level even within our code itself?

### Ensuring structural preservation

Let's say we want to map an IO function over every item in our `UserF`, and
return a new one. We know that whatever IO function we use _must_ leave the
actual "result" type unchanged. So that means we must take a `forall a. f a ->
IO (f a)`

```haskell
traverseUser
    :: Applicative h
    => (forall a. f a -> h (g a))
    -> UserF f
    -> h (UserF g)
traverseUser f u = User <$> f (userName u) <*> f (userAge u)
```

Here again we use the trick above to generalize for all `Applicative h` instead
of concretely `IO`, so we can know that the final action can't sneak in stray
IO.

### Ensuring memory safety

We can also use this property in phantom types to enforce memory regions.
Let's say we are simulating memory in an `IntMap`:

```haskell
newtype Var = Var Int
newtype Memory v = Memory { getMemory :: IntMap v }

initVar :: v -> State (Memory v) Var
initVar x = state $ \(Memory mp) ->
  case IM.lookupMax mp of
    Nothing -> (Var 0, Memory $ IM.insert 0 x mp)
    Just i -> (Var (i + 1), Memory $ IM.insert (i + 1) x mp)

readVar :: Var -> State (Memory v) v
readVar (Var i) = gets ((IM.! i) . getMemory)

writeVar :: Var -> v -> State (Memory v) ()
writeVar (Var i) x = modify (Memory . IM.insert i x . getMemory)

runWithMemory :: State (Memory v) a -> a
runWithMemory = (`evalState` Memory IM.empty)
```

(By the way, what do we gain from having the state be `IntMap v` parametric on
`v`? What guarantees/invariants do we get, what sort of actions do we forbid
the library itself from doing? Is it possible to have a default-initialized
variable?)

We can run operations like:

```haskell
getFib :: Int -> State (Memory Int) Int
getFib n = do
    a <- initVar 0
    b <- initVar 1
    replicateM_ n $ do
        newSum <- (+) <$> readVar a <*> readVar b
        writeVar a =<< readVar b
        writeVar b newSum
    readVar b
```

```haskell
ghci> runWithMemory (getFib 10)
55
```

But now our memory regions are pretty unsafe. We could, for instance, do:
run `runWithMemory` _inside_ itself:

```haskell
myAction :: State (Memory String) a
myAction = do
  v <- initVar "oneRegion"
  let x = runWithMemory $ do
        readVar v -- runtime error!
        -- ..
  -- ..
```

And now `readVar v` makes no sense! Remember that `v` is `Var 0`, but that `0`
key refers to the state map in the outer `IntMap`, and is not undefined
in the internal one.

We can also do something silly like returning a `Var`:

```haskell
ghci> runWithMemory (initVar "hello")
Var 0
```

And now that var exists without a context...its memory is gone, it refers to
something that no longer has any meaning.

We can prevent this by associating a variable and its associated memory region
with a phantom. Then we can ensure that `runWithMemory` requires the phantom to
never be a part of the final output:

```haskell
newtype Var s = Var Int
newtype Memory s v = Memory { getMemory :: IntMap v }

initVar :: v -> State (Memory s v) (Var s)

readVar :: Var s -> State (Memory s v) v

writeVar :: Var s -> v -> State (Memory s v) ()

runWithMemory :: (forall s. State (Memory s v) a) -> a
runWithMemory = (`evalState` Memory IM.empty)
```

Here, a `Var s` must come from a memory bank `Memory s v` with the _same_ `s`.
It is associated with that region, and no others. The `forall` here ensures
that the action being given cannot unify with any external `s`: it _must_ be
treated as fresh from `runWithMemory.`

Right off the bat, this prevents passing variables into nested calls (the first
var's `s` is different than the inner memory bank's `s`), but this also
prevents variables from leaking. That's because the result type `a` must be
fully _independent_ of the `s`, so returning a `Var s` is illegal, since that
would require the `a` to depend on `s`. (This is exactly how the `ST` monad
works in GHC standard libraries, actually)

By requiring the _caller_ to give up control of the `s`, we ensure safety both
of the library and of the user-given continuation.  Now our memory-safety
doesn't come from carefully tracking variables and their source memory blocks.
Instead, it is assured through the universality of the `forall` and the
unconditional properties it enforces.

Habits to Build
---------------

Let's look at what it looks like to recognize this principle in practice, and
use it in your code. Let's imagine we have a function that you can use to
deploy a new `Config` in your environment:

```haskell
deployConfig :: Config -> IO ()
```

But, deployment is a bit expensive. So we want to de-duplicate our deploys:
deploying the same `Config` twice would be a no-op. We can do this by keeping a
`Config` in an `IORef`:

```haskell
-- | returns True if changed, otherwise False if already deployed
updateConfig :: IORef Config -> Config -> IO Bool
updateConfig cache newConfig = do
    oldConfig <- readIORef cache
    if oldConfig == newConfig
        then pure False
        else do
            deployConfig newConfig
            writeIORef cache newConfig
            pure True
```

This _works_, but after learning about the principles in this post, that type
synonym should feel a little bit suspicious to you. Note that our function
never actually _inspects_ the `Config` at all.  The logic is independent.
Would there be any value in pulling out the caching logic generically?

```haskell
cachedUpdate :: Eq a => (a -> IO ()) -> IORef a -> a -> IO Bool
cachedUpdate action cache newVal = do
    oldVal <- readIORef cache
    if oldVal == newVal
        then pure False
        else do
            action newVal
            writeIORef cache newVal
            pure True

updateConfig :: IORef Config -> Config -> IO Bool
updateConfig = cachedUpdate deployConfig
```

Let's presume that we never intend on re-using `cachedUpdate`. So, we just
increased our total lines of code...and for what? What does `cachedUpdate` get
us?

Firstly, in the original monomorphic `updateConfig`, written directly against
`Config`, there is so much that could go wrong. Maybe you could mis-handle the
configuration or accidentally modify it. You could set certain fields to
fixed values. You might end up deploying a configuration that was never passed
in directly.

In our `cachedUpdate` implementation, we are _sure_ that any `Config`
deployed will _only_ come _directly_ from calls to `updateConfig`. No other
operations are possible.

Secondly, the type signature of `cachedUpdate` tells us a lot more about what
`cachedUpdate`'s intended logic is and what exactly it can support. Let's say
in the future, a new requirement comes: Deploy a "default" configuration if
`deployConfig` ever fails.

You _want_ something as drastic like this to require you to change your types
and your contracts. In fact, if a new requirement comes along and you are able
to implement it without changing your types, that should be extremely scary to
you, because you previously allowed way too many potentially invalid programs
to compile.

If we were to add such a change ("deploy a default `Config`"), it _should_ have
us go back to the type signature of `cachedUpdate` and see how it must change
in order for us to support it. That process of interrogation makes us think
about what we actually want to do and how it would fundamentally change our
data flow.

If you subscribe to ["SOLID" programming][solid], this should all remind you of
"Dependency Inversion".

[solid]: https://en.wikipedia.org/wiki/SOLID

Basically: treat all monomorphic code with suspicion. It may be a symptom of
you trying to hold on to more control, when you should be letting go.

Embracing Unconditional Election
--------------------------------

What sort of control are you trying to hang on to in life, in a way
that puts you in your own prison?

To me, the fact that making code more polymorphic and giving up information is
valuable not just for abstraction, but for taking advantage of universal
properties, was a surprising one.  But ever since I started writing Haskell,
it's a fact that I take advantage of every day. So, next time you see the
opportunity, try thinking about what that parametric `forall` can do for you.
Take advantage of the doctrine of Haskell predestination that arrives from
properties of logic determined before our universe ever existed.

### The Next Step

Embracing Total Depravity and Unconditional Election should redefine your
relationship with your code. But not all code lives in the nice pure world
where we can cordon off effects. `forall a. [a] -> [a]` is very different than
`forall a. [a] -> IO [a]`, after all.

To extend these boundaries to useful code, we have to deal with that boundary
between the world of the pure and the world where [things actually happen for
real][xkcd]. We'll explore the nuances of that boundary in the next chapter of
[Five-Point Haskell][Five-Point Haskell], **Limited Atonement**.

[xkcd]: https://xkcd.com/1312/

Special Thanks
--------------

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts. Very special
thanks to my supporter at the "Amazing" level on [patreon][], Josh Vera! :)

[patreon]: https://www.patreon.com/justinle/overview
