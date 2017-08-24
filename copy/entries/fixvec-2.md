---
title: Fixed-Length Vector Types in Haskell (an Update for 2017)
categories: Haskell, Tutorials, Reference
tags: haskell, types
create-time: 2017/08/23 13:34:12
date: never
identifier: fixvec-2
slug: fixed-length-vector-types-in-haskell
---

This post is a follow-up to my [fixed-length vectors in haskell in 2015][2015]
post!  When I was writing the post originally, I was new to the whole
type-level game in Haskell; I didn't know what I was talking about, and that
post was a way for me to push myself to learn more.

[2015]: https://blog.jle.im/entry/fixed-length-vector-types-in-haskell-2015.html

Immediately after it was posted, people taught me where I went wrong in the
idioms I explained, and better and more idiomatic ways to do things.
Unfortunately, I have noticed people referring to the post in a
canonical/authoritative way...so the post became an immediate regret to me.  I
tried correcting things with my [practical dependent types in
haskell][deptypes] series the next year, which incorporated what I had learned.
But I still saw my 2015 post being used as a reference, so I figured that
writing a direct replacement/follow-up as the only way I would ever fix this!

[deptypes]: https://blog.jle.im/entries/series/+practical-dependent-types-in-haskell.html

So here we are in 2017.  What's the "right" way to do fixed-length vectors in
Haskell?

As a Wrapper with Smart Constructors
------------------------------------

In most situations, if you use vectors, you want some sort of constant-time
indexed data structure.  The best way to do this in Haskell is to wrap the
heavily optimized *[vector][]* library with a newtype wrapper that contains its
length as a phantom type parameter.

[vector]: http://hackage.haskell.org/package/vector

```haskell
import qualified Data.Vector as V

data Vec (n :: Nat) a = UnsafeMkVec { getVector :: V.Vector a }
    deriving Show
```

A `Vec n a` will represent an `n`-element vector of `a`s.  So, a `Vec 5 Int`
will be a vector of five `Int`s, a `Vec 10 String` is a vector of 10 `String`s,
etc.

For our numeric types, we're using the fancy "type literals" that GHC offers us
with the `DataKinds` extension.  Basically, alongside the normal kinds `*`, `*
-> *`, etc., we also have the `Nat` kind; type literals `1`, `5`, `100`, etc.
are all *types* with the *kind* `Nat`.

```haskell
ghci> :k 5
Nat
ghci> :k Vec
Vec :: Nat -> * -> *
```

You can "reflect" the type-level numeral as a value using the `KnownNat`
typeclass, provided by GHC, which lets you gain back the number as a run-time
value using `natVal`:

```haskell
natVal :: KnownNat n => p n -> Integer
```

```haskell
ghci> natVal (Proxy @10)   -- or, natVal (Proxy :: Proxy 10)
10
ghci> natVal (Proxy @7)
7
```

### The Smart Constructor

We can use `natVal` with the `KnownNat` typeclass to write a "smart
constructor" for our type -- make a `Vec` from a `Vector`, but only if the
length is the correct type:

```haskell
mkVec :: forall n. KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVec v | V.length v == l = Just (UnsafeMkVec v)
        | otherwise       = Nothing
  where
    l = natVal (Proxy @n)
```

Here, we use `ScopedTypeVariables` so we can refer to the `n` in the type
signature in the function body (for `natVal (Proxy @n)`).  We need to use an
explicit forall, then, to bring the `n` into scope.

### Indexing

We need an appropriate type for indexing these, but we'd like a type where
indexing is "safe" -- that is, that you can't compile a program that will
result in an index error.

For this, we can use the *[finite-typelits][]* package, which provides the
`Finite n` type.

[finite-typelits]: http://hackage.haskell.org/package/finite-typelits

A `Finite n` type is a type with exactly `n` distinct inhabitants/values.  For
example, `Finite 4` contains four "anonymous" inhabitants.  For convenience,
sometimes we like to name them 0, 1, 2, and 3.  In general, we sometimes refer
to the values of type `Finite n` as 0 ... (n - 1).

So, we can imagine that `Finite 6` has inhabitants corresponding to 0, 1, 2, 3,
4, and 5.  We can convert back and forth between a `Finite n` and its `Integer`
representation using `packFinite` and `getFinite`:

```haskell
packFinite :: KnownNat n => Integer  -> Maybe (Finite n)
getFinite  ::               Finite n -> Integer
```

```haskell
ghci> map packFinite [0..3] :: [Finite 3]
[Just (finite 0), Just (finite 1), Just (finite 2), Nothing]
ghci> getFinite (finite 2 :: Finite 5)
2
```

We can use a `Finite n` to "index" a `Vector n a`.  A `Vector n a` has exactly
`n` slots, and a `Finite n` has `n` possible values.  Clearly, `Finite n` only
contains valid indices into our vector!

```haskell
index :: Vec n a -> Finite n -> a
index v i = getVector v V.! fromIntegral (getFinite i)
```

`index` will never fail at runtime due to a bad index --- do you see why?
Valid indices of a `Vector 5 a` are the integers 0 to 4, and that is precisely
the exact things that `Finite 5` can store!

### Appending and type-level arithmetic

Another operation we might want to do with vectors is append them and do things
with them that might change their length.  We might want the type of our
vectors to describe the nature of the operations they are undergoing.  For
example, if you saw a function:

```haskell
someFunc :: Vec n a -> Vec m a -> Vec (n + m) a
```

You can see that it takes a vector of length `n` and a vector of length `m`,
and returns a vector of length `n + m`.  Clearly, this function must be
appending/concatenating the two input vectors!

In this situation, we can write such an appending function in an "unsafe" way,
and then give it our type signature as a form of documentation.

```haskell
append :: Vec n a -> Vec m a -> Vec (n + m) a
append v w = UnsafeMkVec $ getVector v V.++ getVector w
```

The compiler didn't help us write this function, and we have to be pretty
careful that the guarantees we specify in our types are reflected in the actual
unsafe operations.  This is because our types don't *structurally* enforce
their type-level lengths.

So, why bother?  For us, here, our fixed-length vector types basically act as
"active documentation", in a way.  Compare:

```haskell
-- | Appends a vector of length n with a vector of length m to get a vector of
length (n + m).
append :: V.Vector a -> V.Vector a -> V.Vector a
```

We have to rely on the documentation to *tell* us what the length of the final
resulting vector is, even though it can be known statically if you know the
length of the input vectors.  The vectors have a *static relationship* in their
length, but this isn't specified in a way that the compiler can take advantage
of.

By having our `append :: Vec n a -> Vec m a -> Vec (n + m) a`, the relationship
between the input lengths and output length is right there in the types, when
you *use* `append`, GHC is aware of the relationships and can give you help in
the form of typed hole suggestions and informative type errors.  You can even
catch errors in logic at compile-time instead of runtime!

Here, `(+)` comes from GHC, which provides it as a type family (type-level
function) we can use, with proper meaning and semantics.

Some other examples include:

```haskell
-- the end vector has the same length as the starting vector
map :: (a -> b) -> Vec n a -> Vec n b
-- you must zip two vectors of the same length
zip :: Vec n a -> Vec n b -> Vec n (a, b)
-- type-level arithmetic to let us 'take'
take :: Vec (n + m) a -> Vec n a
-- splitAt, as well
splitAt :: Vec (n + m) a -> (Vec n a, Vec m a)
```

### Generating

We can directly generate these vectors in interesting ways.  Using return-type
polymorphism, we can have the user *directly* request a vector length, *just*
by using type inference or a type annotation. (kind of like `read`)

For example, we can write a version of `replicate`:

```haskell
replicate :: forall n a. KnownNat n => a -> Vec n a
replicate x = UnsafeMkVec $ V.replicate l x
  where
    l = fromIntegral $ natVal (Proxy @n)
```

```haskell
ghci> replicate 'a' :: Vec 5 Char
UnsafeMkVec (V.fromList ['a','a','a','a','a'])
```

Note that normally, `replicate` takes an `Int` argument so that the user can
give how long the resulting vector needs to be.  However, with our new
`replicate`, we don't need that `Int` argument --- the size of the vector we
want can more often than not be inferred automatigically using type inference!

With this new cleaner type signature, we can actually see that `replicate`'s
type is something very similar.  Look at it carefuly:

```haskell
replicate :: KnownNat n => a -> Vec n a
```

You might recognize it as the haskellism `pure`:

```haskell
pure :: Applicative f => a -> f a
```

`replicate` is actually `pure` for the Applicative instance of `Vec n`!  As an
extra challenge, what would `<*>` be?

#### Generating with indices

We can be a little more fancy with `replicate`, to get what we normally call
`generate`:

```haskell
generate :: forall n. KnownNat n => (Finite n -> a) -> Vec n a
generate f = UnsafeMkVec $ V.generate l (f . finite)
  where
    l = fromIntegral $ natVal (Proxy @n)
```

#### A discussion on the advantages of type-safety

I think it's an interesting point that we're using `Finite n` in a different
sense here than in `index`, for different reasons.  In `index`, `Finite` is
in the "negative" position --- it's something that the function "takes".  In
`generate`, `Finite` is in the "positive" position --- it's something that the
function "gives" (to the `f` in `generate f`).

In the negative position, `Finite n` and type-safety is useful because:

1.  It tells the user what sort of values that the function expects.  The user
    *knows*, just from the type, that indexing a `Vec 5 a` requires a `Finite
    5`, or a number between 0 and 4.
2.  It guarantees that whatever `Finite n` index you give to `index` is a
    *valid one*. It's impossible to give `index` an "invalid index", so `index`
    is allowed to use "unsafe indexing" in its implementation, knowing that
    nothing bad can be given.
3.  It lets you develop code in "typed-hole" style: if a function requires a
    `Finite 4`, put an underscore there, and GHC will tell you about all the
    `Finite 4`s you have in scope.  It can help you write your code for you!

In the positive position, `Finite n` and the type-safety have different uses
and advantages:  it tells the user what sort of values the function can return,
and also also the type of values that the user has to be expected to handle.
For example, in `generate`, the fact that the user has to provide a `Finite n
-> a` tells the user that they have to handle every number between 0 and n-1,
and nothing else.

### Between Static and Dynamic

So



