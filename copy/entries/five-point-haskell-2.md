---
title: "\"Five Point Haskell\": Unconditional Election (Parametric Polymorphism)"
categories: Haskell
tags: functional programming, parametric polymorphism
create-time: 2026/01/01 21:51:17
identifier: five-point-haskell-2
slug: five-point-haskell-part-2-unconditional-election
series: five-point-haskell
---

Welcome back *[Five-Point Haskell][]*! This is my attempt to codify principles
of writing robust, maintainable, correct, clear, and effective code in Haskell
and to dispel common bad practices or heresies I have ran into in my time.

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
> Surrender to your control to parametric polymorphism in all things. Embrace
> the free-dom of "Free Theorems" from one of Haskell's greatest unexpected
> strengths: the type parameter.

Choice is a Prison
------------------

### Conditional Election

Learning Haskell can be a journey full of surprises, but this was one of the
ones that blew my mind the most.

Let's think of a polymorphic function in java that takes a value of any type
and returns something of that same type:

```java
static <T> T foo(T)
```

What could that function do?

Well, it could do IO or throw an exception, or possibly be non-terminating, but
let's assume all it (and every other example here) does is purely return a
value. What could it do?

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

Same deal in typescript or pretty much any other typed language without
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

Haskell has type erasure and no runtime reflection, so the _only_ impossible
terminating implementation is simply

```haskell
foo :: a -> a
foo x = x
```

So there is an invariant that happened here: a function of type `forall a. a ->
a` _must_ leave its value unchanged!

But wait...says who? Did we insert some sort of `const` compiler annotation?
Did we add some sort of annotation or pre- and post-condition that the value
cannot change? Are we relying on any sort of foreseeable property or merit of
the value given?

No. This behavior is intrinsically fixed! We got this theorem _for free_.
Without any need for any sort of works. We didn't even have to _write_ the
function before knowing all it possibly could be.

Consider another type signature:

```java
static <T> String foo(T x)
```

In java (and most other languages), this again could literally be anything. You
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
depend on the input as far was what is revealed in its `Show` instance.
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
tumeric :: ((a, b) -> c) -> a -> b -> c
```

If you think about it, the only option is:

```haskell
tumeric :: ((a, b) -> c) -> a -> b -> c
tumeric f x y = f (x, y)
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
And, more importantly, it _can't_ depend anything on the properties of any `a`.

From this, we can derive what are called [free theorems][] to look at
properties that _any implementation_ must take. Namely, _mapping_ a function
over the list and calling `theThing` must be equivalent to calling `theThing`
and then mapping:

[free theorems]: https://people.mpi-sws.org/~dreyer/tor/papers/wadler.pdf

```haskell
theThing . map f
  === map f . theThing
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

How about:

```haskell
doIt :: [a] -> Maybe a
```

Think about what this _can't_ do. It clearly selects a single item, but:

1.  The single item cannot be determined based on any quality of that item ---
    it can't be the smallest, the largest, etc.; it has to purely depend on the
    position on the list
2.  If given an empty list, it _must_ return `Nothing`

And again we have the same free theorem, `doIt . map f === fmap f . doIt`. No
matter how you implement `doIt`, it is guaranteed to commute with `map` and
`fmap`!

### Surrendering Control

Practically, this becomes similar to the principal of least power. Say you
_are_ writing a function that shuffles a list of items, important for your
business logic. You can encode exactly _what_ business logic is being done by
adding more and more parametricity.

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
*   If your type is `Monad f => m a -> m a`, you know that the lengths of your
    results will always be integer powers of the length of the input.

By switching from concrete types slowly to parametric types, you surrender
control of what your functions can do, and create stronger and stronger
guarantees. In other languages, or with refinement types, you might have to
explicitly declare a post-condition like "the final values must all come from
the original list". With parametric polymorphism, this is already guaranteed
and elected, no matter what the implementation is.

Preserving Structures
---------------------

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

Maybe if we were in `c`, we could have a `const` field:

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

Instead, we can make enforce this by making `userId` a type parameter:

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
-- | Invariant: does not add or remove items
updateItems :: Checklist -> IO Checklist
```

How can we make sure all our implementations are _elected_ to only be
implementations that don't modify the length of `items`?

Again the answer can be: add quantification!

```haskell
data Checklist f = Checklist
  { updated :: UTCTime
  , items :: f (Status, String)
  }

-- | Guaranteed not to add or remove items
updateItems :: Functor f => Checklist f -> IO (Checklist f)
```

Your values would always be `Checklist []` in practice. We are _not_ using the
type parameter to be able to "customize" our types and their structure, like
sometimes having `Checklist Maybe` and `Checklist IO`. No, instead we are
picking them to intentionally use them universally quantified in functions that
process them, in order to take advantage of these automatically enforced
properties.

This intersects a lot with the [Higher-Kinded Data][hkd] pattern. Maybe we _do_
have data we want to have multiple structural versions of:

[hkd]: https://reasonablypolymorphic.com/blog/higher-kinded-data/

```haskell
data UserF f = UserF
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
processUser :: Functor f => User f -> User f
```

will give you a different, unique guarantee for every "shape" your user has:

*   For `UserF Maybe`, the quantification ensures that the null-or-present
    property of each field is preserved
*   For `UserF Parser`, it ensures that all of the "parsing" logic, and the set
    of strings that are validly parsed, is preserved
*   For `UserF (Const Doc)`, it ensures that the per-field `Doc`/documentation
    is never changed or updated.

All of these properties are _mathematically_ enforced, _unconditionally_. It
doesn't depend on any foreseen property of the types or values we use. These
guarantees free us to be able to confidently use these functions without
fear of invariants breaking.

<!-- bump abstraction, updator -->

<!-- *   Parametric polymorphism --- guess the implementation -->
<!--     *   `[Int] -> [Int]` vs `[a] -> [a]` -->
<!--     *   Compare with refinement types -->
<!-- *   higher-kinded data, parametric over functors -->
<!-- *   Subtyping via parametric polymorphism -->
<!-- *   Phantoms + parametric polymoirphism, restrictions, ST trick -->
<!-- *   Princple of least strength, Monad vs Applicative -->
<!--     *   typeclass-based limitation of functions -->

