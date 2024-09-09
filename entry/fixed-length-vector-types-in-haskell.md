Fixed-Length Vector Types in Haskell (an Update for 2017)

==========================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on August 25, 2017.
> [Read online!](https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html)

This post is a follow-up to my [fixed-length vectors in haskell in
2015](https://blog.jle.im/entry/fixed-length-vector-types-in-haskell-2015.html)
post! When I was writing the post originally, I was new to the whole type-level
game in Haskell; I didn't know what I was talking about, and that post was a way
for me to push myself to learn more. Immediately after it was posted, of course,
people taught me where I went wrong in the idioms I explained, and better and
more idiomatic ways to do things. And that's great! Learning is awesome!

Unfortunately, however, to my horror, I began noticing people referring to the
post in a canonical/authoritative way...so the post became an immediate source
of guilt to me. I tried correcting things with my [practical dependent types in
haskell](https://blog.jle.im/entries/series/+practical-dependent-types-in-haskell.html)
series the next year. But I still saw my 2015 post being used as a reference
even after that post, so I figured that writing a direct replacement/follow-up
as the only way I would ever fix this!

So here we are in 2017. GHC 8.2 is here, and *base* is in version *4.10*. What's
the "right" way to do fixed-length vectors in Haskell?

This post doesn't attempt to present anything groundbreaking or new, but is
meant to be a sort of *reference/introduction* to fixed-length vectors in
Haskell, as of GHC 8.2 and the 2017 Haskell ecosystem.

We'll be looking at two methods here: The first one we will be looking at is a
*performant* fixed-length vector that you will probably be using for any code
that requires a fixed-length container --- especially for tight numeric code and
situations where performance matters. We'll see how to implement them using the
universal native `KnownNat` mechanisms, and also how we can implement them using
*[singletons](http://hackage.haskell.org/package/singletons)* to help us make
things a bit smoother and more well-integrated. For most people, this is all
they actually need. (I claim the canonical haskell ecosystem source to be the
*[vector-sized](http://hackage.haskell.org/package/vector-sized)* library)

The second method is a *structural* fixed-length inductive vector.
It's...actually more like a fixed-length (lazily linked) *list* than a vector.
The length of the list is enforced by the very structure of the data type. This
type is more useful as a streaming data type, and also in situations where you
want take advantage of the structural characteristics of lengths in the context
of a dependently typed program. (I claim the canonical haskell ecosystem source
to be the
*[type-combinators](http://hackage.haskell.org/package/type-combinators)*
library)

## The Non-Structural Way

(Code for this section (as well as exercise solutions) are [available
here](https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs))

In most situations, if you use vectors, you want some sort of constant-time
indexed data structure. The best way to do this in Haskell is to wrap the
heavily optimized *[vector](http://hackage.haskell.org/package/vector)* library
with a newtype wrapper that contains its length as a phantom type parameter.

``` haskell
import qualified Data.Vector as V
import           GHC.TypeNats

-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs#L19-L20

data Vec (n :: Nat) a = UnsafeMkVec { getVector :: V.Vector a }
    deriving Show
```

A `Vec n a` will represent an `n`-element vector of `a`s. So, a `Vec 5 Int` will
be a vector of five `Int`s, a `Vec 10 String` is a vector of 10 `String`s, etc.

For our numeric types, we're using the fancy "type literals" that GHC offers us
with the `DataKinds` extension. Basically, alongside the normal kinds `*`,
`* -> *`, etc., we also have the `Nat` kind; type literals `1`, `5`, `100`, etc.
are all *types* with the *kind* `Nat`, from the
*[GHC.TypeNats](http://hackage.haskell.org/package/base/docs/GHC-TypeNats.html)*
module[^1].

``` haskell
ghci> :k 5
Nat
ghci> :k Vec
Vec :: Nat -> * -> *
```

You can "reflect" the type-level numeral as a value using the `KnownNat`
typeclass, provided by GHC, which lets you gain back the number as a run-time
value using `natVal`: (This process is called "reflection")

``` haskell
natVal :: KnownNat n => p n -> Natural
```

(Where `Natural`, from
*[Numeric.Natural](http://hackage.haskell.org/package/base/docs/Numeric-Natural.html)*,
is a non-negative `Integer` type.)

``` haskell
ghci> natVal (Proxy @10)   -- or, natVal (Proxy :: Proxy 10)
10
ghci> natVal (Proxy @7)
7
```

Super low-level utility functions for the `Nat` kind (like `natVal`) are found
in the
*[GHC.TypeNats](http://hackage.haskell.org/package/base/docs/GHC-TypeNats.html)*
module (and also in
*[GHC.TypeLits](http://hackage.haskell.org/package/base/docs/GHC-TypeLits.html)*
for a slightly different API)

### The Smart Constructor

We can use `natVal` with the `KnownNat` typeclass to write a "smart constructor"
for our type -- make a `Vec` from a `Vector`, but only if the length is the
correct type:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs#L22-L26

mkVec :: forall n a. KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVec v | V.length v == l = Just (UnsafeMkVec v)
        | otherwise       = Nothing
  where
    l = fromIntegral (natVal (Proxy @n))
```

Here, we use `ScopedTypeVariables` so we can refer to the `n` in the type
signature in the function body (for `natVal (Proxy @n)`), and we need to use an
explicit forall, then, to bring the `n` into scope.

We also use `TypeApplications` syntax (`Proxy @n`) so we can say "We want a
`Proxy :: Proxy n`" and tell `natVal` that we want the `Natural` for `n`.

### Utilizing type-level guarantees

Another operation we might want to do with vectors is do things with them that
might change their length in a predetermined way. We might want the type of our
vectors to describe the nature of the operations they are undergoing. For
example, if you saw a function:

``` haskell
someFunc :: (a -> b) -> Vec n a -> Vec n b
```

You can see that it takes a function and a vector of length `n`, and returns
another vector of length `n`. Clearly, this function might be a "map" function,
which applies the function to all of the values in the `Vec`! We know that it
must have the same length, so it can't drop or add items. (However, it could
still be shuffling or duplicating or permuting the items, as long as the
resulting length is the same)

In this situation, we can write such a mapping function in an "unsafe" way, and
then give it our type signature as a form of documentation:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs#L28-L29

mapVec :: (a -> b) -> Vec n a -> Vec n b
mapVec f v = UnsafeMkVec $ V.map f (getVector v)

-- just for fun
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs#L31-L32

instance Functor (Vec n) where
    fmap = mapVec
```

The compiler didn't help us write this function, and we have to be pretty
careful that the guarantees we specify in our types are reflected in the actual
unsafe operations. This is because our types don't *structurally* enforce their
type-level lengths.

So, why bother? For us, here, our fixed-length vector types basically act as
"active documentation", in a way. Compare:

``` haskell
-- | Maps the function over the items in the vector, returning a vector of the
-- same length
V.map :: (a -> b) -> V.Vector a -> V.Vector b
```

Which is the type signature we'd have to write for a map of an "unsized" vector
(like from the *Data.Vector* module)

We have to rely on the documentation to *tell* us what the length of the final
resulting vector is, even though it can be known statically if you know the
length of the input vectors. The vectors have a *static relationship* in their
length, but this isn't specified in a way that the compiler can take advantage
of.

By having our `mapVec :: (a -> b) -> Vec n a -> Vec n b`, the relationship
between the input lengths and output length is right there in the types, when
you *use* `mapVec`, GHC is aware of the relationships and can give you help in
the form of typed hole suggestions and informative type errors. You can even
catch errors in logic at compile-time instead of runtime!

``` haskell
-- the resulting vector's length is the sum of the input vectors' lengths
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs#L34-L35

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
UnsafeMkVec xs ++ UnsafeMkVec ys = UnsafeMkVec (xs V.++ ys)

-- you must zip two vectors of the same length
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs#L37-L38

zipVec :: Vec n a -> Vec n b -> Vec n (a, b)
zipVec (UnsafeMkVec xs) (UnsafeMkVec ys) = UnsafeMkVec (V.zip xs ys)

-- type-level arithmetic to let us 'take'
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs#L40-L43

takeVec :: forall n m a. KnownNat n => Vec (n + m) a -> Vec n a
takeVec (UnsafeMkVec xs) = UnsafeMkVec (V.take l xs)
  where
    l = fromIntegral (natVal (Proxy @n))

-- splitAt, as well
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs#L45-L49

splitVec :: forall n m a. KnownNat n => Vec (n + m) a -> (Vec n a, Vec m a)
splitVec (UnsafeMkVec xs) = (UnsafeMkVec ys, UnsafeMkVec zs)
  where
    l = fromIntegral (natVal (Proxy @n))
    (ys, zs) = V.splitAt l xs
```

Here, `(+)` comes from
*[GHC.TypeNats](http://hackage.haskell.org/package/base/docs/GHC-TypeNats.html)*,
which provides it as a type family (type-level function) we can use, with proper
meaning and semantics.

Note the usage of `(+)` for `takeVec` and `splitVec` to let the function ensure
that the input vector has "enough" (at least `n`) elements to do the taking.

#### Notes on the typechecker

GHC's typechecker works very well with concrete, monomorphic `Nat`s. For
example, `5 + 3` will always typecheck as `8`, so you don't have to worry at all
about `takeVec`, `(++)`, and `splitVec`'s usage of `(+)` if you work with
monomorphic, specific `Nat`s.

However, GHC treats `(+)` "opaquely" when using using it with polymorphic type
variables. That means that `n + (m + o)` is seen as a completely different type
to GHC than `(n + m) + o` -- GHC doesn't reduce `+`, and to it, they both just
look like different trees. Remember that one is `(+) n ((+) m o)`, and the other
is `(+) ((+) n m) o`. Completely different structure!

This comes up as an issue when you start doing non-trivial things, so it
sometimes helps to augment GHC's typechecker.

The
*[ghc-typelits-natnormalise](https://hackage.haskell.org/package/ghc-typelits-natnormalise)*
package provides such a plugin. If we pass it as a flag to GHC (as
`-fplugin GHC.TypeLits.NatNormalise`) or as a pragma:

``` haskell
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
```

Then GHC will be able to recognize the fact that `n + (m + o)` and `(n + m) + o`
are the same, and will unify them during typechecking. It also provides
normalization/unification for many other situations that we "expect" to work
when using `*` and `+` on type variables.

### Indexing

We need an appropriate type for indexing our vectors, but we'd like a type where
indexing is "safe" -- that is, that you can't compile a program that will result
in an index error.

For this, we can use the
*[finite-typelits](http://hackage.haskell.org/package/finite-typelits)* package,
which provides the `Finite n` type.

A `Finite n` type is a type with exactly `n` distinct inhabitants/values. For
example, `Finite 4` contains four "anonymous" inhabitants. For convenience,
sometimes we like to name them 0, 1, 2, and 3. In general, we sometimes refer to
the values of type `Finite n` as 0 ... (n - 1).

So, we can imagine that `Finite 6` has inhabitants corresponding to 0, 1, 2, 3,
4, and 5. We can convert back and forth between a `Finite n` and its `Integer`
representation using `packFinite` and `getFinite`:

``` haskell
packFinite :: KnownNat n => Integer  -> Maybe (Finite n)
getFinite  ::               Finite n -> Integer
```

``` haskell
ghci> map packFinite [0..3] :: [Maybe (Finite 3)]
[Just (finite 0), Just (finite 1), Just (finite 2), Nothing]
ghci> getFinite (finite 2 :: Finite 5)
2
```

We can use a `Finite n` to "index" a `Vector n a`. A `Vector n a` has exactly
`n` slots, and a `Finite n` has `n` possible values. Clearly, `Finite n` only
contains valid indices into our vector!

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs#L51-L52

index :: Vec n a -> Finite n -> a
index v i = getVector v V.! fromIntegral (getFinite i)
```

`index` will never fail at runtime due to a bad index --- do you see why? Valid
indices of a `Vector 5 a` are the integers 0 to 4, and that is precisely the
exact things that `Finite 5` can store!

### Generating

We can directly generate these vectors in interesting ways. Using return-type
polymorphism, we can have the user *directly* request a vector length, *just* by
using type inference or a type annotation. (kind of like with `read`)

For example, we can write a version of `replicate`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs#L54-L57

replicate :: forall n a. KnownNat n => a -> Vec n a
replicate x = UnsafeMkVec $ V.replicate l x
  where
    l = fromIntegral (natVal (Proxy @n))
```

``` haskell
ghci> replicate 'a' :: Vec 5 Char
UnsafeMkVec (V.fromList ['a','a','a','a','a'])
```

Note that normally, `replicate` takes an `Int` argument so that the user can
give how long the resulting vector needs to be. However, with our new
`replicate`, we don't need that `Int` argument --- the size of the vector we
want can more often than not be inferred auto-magically using type inference!

With this new cleaner type signature, we can actually see that `replicate`'s
type is something very familiar. Look at it carefully:

``` haskell
replicate :: KnownNat n => a -> Vec n a
```

You might recognize it as very similar to haskellism `pure`:

``` haskell
pure :: Applicative f => a -> f a
```

`replicate` is actually `pure` for the Applicative instance of `Vec n`! As an
extra challenge, what would `<*>` be? See [the
solution](https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs#L59-L61)
if you want to check your answer!

#### Generating with indices

We can be a little more fancy with `replicate`, to get what we normally call
`generate`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs#L63-L66

generate :: forall n a. KnownNat n => (Finite n -> a) -> Vec n a
generate f = UnsafeMkVec $ V.generate l (f . fromIntegral)
  where
    l = fromIntegral (natVal (Proxy @n))
```

#### Type-Safety and positives and negatives

I think it's an interesting point that we're using `Finite n` in a different
sense here than in `index`, and for different reasons. In `index`, `Finite` is
in the "negative" position --- it's something that the function "takes". In
`generate`, `Finite` is in the "positive" position --- it's something that the
function "gives" (to the `f` in `generate f`).

In the negative position, `Finite n` and type-safety is useful because:

1.  It tells the user what sort of values that the function expects. The user
    *knows*, just from the type, that indexing a `Vec 5 a` requires a
    `Finite     5`, or a number between 0 and 4.
2.  It guarantees that whatever `Finite n` index you give to `index` is a *valid
    one*. It's impossible to give `index` an "invalid index", so `index` is
    allowed to use "unsafe indexing" in its implementation, knowing that nothing
    bad can be given.
3.  It lets you develop code in "typed-hole" style: if a function requires a
    `Finite 4`, put an underscore there, and GHC will tell you about all the
    `Finite 4`s you have in scope. It can help you write your code for you!

In the positive position, `Finite n` and the type-safety have different uses and
advantages: it tells the user what sort of values the function can return, and
also also the type of values that the user has to be expected to handle. For
example, in `generate`, the fact that the user has to provide a `Finite n -> a`
tells the user that they have to handle every number between 0 and n-1, and
nothing else.

### Moving between Sized and Unsized

One key part of our API is missing: how to convert between "sized" and "unsized"
vectors.

Converting from sized to unsized is easy, and we already have it:

``` haskell
getVector :: Vec n a -> V.Vector a
```

Converting from unsized to sized is harder. We already saw a "shoe-horning"
method, if we know the size we want at compile-time:

``` haskell
mkVec :: forall n. KnownNat n => V.Vector a -> Maybe (Vec n a)
```

But what if we don't know what size `n` we want? What if we want `n` to be
whatever the actual size of the input vector is?

In general we can't predict the size of our input vector at compile-time, so we
can't just directly put in a size we want. What we want is a method to return a
`Vec n`, where `n` is the length of the input vector, determined at runtime.

I'm going to try to convince you that a plausible API is:

``` haskell
withVec
    :: V.Vector a
    -> (forall n. KnownNat n => Vec n a -> r)
    -> r
```

(Note: this does require `RankNTypes`)

People familiar with dependent types might recognize that `withVec` is a
function that takes an unsized vector and returns an *existentially quantified*
sized vector, in CPS-style. Basically, give the function a vector, and a way to
"handle" a `Vec n` of *any possible size*. The function will then give your
handler a `Vec n` of the proper type/size. The *function* gets to chose the `n`
that you must handle.

Within your continuation/handler, you can take advantage of the size type, and
do take advantage of all of the type-level guarantees and benefits of a
length-indexed vector. In a way, it is its own "world" where your vector has a
fixed size. However, the caveat is that you have to treat the size *universally*
--- you have to be able to handle any possible size given to you, in a
parametrically polymorphic way.

For example:

``` haskell
ghci> myVector = V.fromList [10,5,8] :: V.Vector Int
ghci> withVec myVector $ \(v :: Vec n Int) ->
          -- in this function body, `v :: Vec 3 Int`, and `n ~ 3`
          -- whatever I return here will be the return value of the entire line
          case packFinite 1 :: Maybe (Finite n) of      -- Finite 3
            Nothing -> 0
            Just i  -> v `index` i
5
```

We could write, say, a function to always safely get the third item:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs#L68-L69

getThird :: V.Vector a -> Maybe a
getThird v = withVec v $ \v' -> fmap (v' `index`) (packFinite 2)
```

And we can run it:

``` haskell
ghci> getThird $ V.fromList [1,2,3]
Just 3
ghci> getThird $ V.fromList [1,2]
Nothing
```

We can even do something silly like convert an unsized vector to a sized vector
and then back again:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs#L71-L72

vectorToVector :: V.Vector a -> V.Vector a
vectorToVector v = withVec v getVector
```

Now that I've (hopefully) convinced you that this function really does convert
an unsized vector into a sized vector that you can use, let's see how we can
implement it!

To do this, we can take advantage of the `someNatVal` function (from
*[GHC.TypeNats](http://hackage.haskell.org/package/base/docs/GHC-TypeNats.html)*):

``` haskell
data SomeNat = forall n. KnownNat n => SomeNat (Proxy n)

someNatVal :: Natural -> SomeNat
```

`SomeNat` contains what we call an existentially quantified type, `n`.
Basically, a value of `SomeNat` contains a `Proxy n` with *some specific `n`*,
that is hidden "inside" the constructor. The only way to figure it out is to
pattern match on the constructor and use it in a generic and parametrically
polymorphic way. Sound familiar?

`someNatVal` converts `Natural` (a non-negative Integer type) into a `SomeNat`
--- it "picks" the right `n` (the one that corresponds to that `Natural`) and
stuffs/hides it into `SomeNat`. We can leverage this to write our `withVec`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs#L74-L76

withVec :: V.Vector a -> (forall n. KnownNat n => Vec n a -> r) -> r
withVec v f = case someNatVal (fromIntegral (V.length v)) of
    SomeNat (Proxy :: Proxy m) -> f (UnsafeMkVec @m v)
```

(The `TypeApplications` syntax `@m` is used with `UnsafeMkVec` to specify that
we want a `Vec m a`.)

This process is actually called "reification" -- we take a value-level runtime
property (the length) and "reify" it, bringing it up to the type-level.

And now, we have both of our conversion functions! We can convert from sized to
unsized using `getVector`, and from unsized to sized using `withVec`.

### Verifying Properties

The final useful API aspect we will be looking at is how to verify properties of
our vector lengths at the type level, and let us use those properties.

One common thing we might want to do is ensure that two vectors have the same
length. This might happen when we use `withVec` from two different vectors, and
we get a `Vec n a` and `Vec m a` of two (potentially) different lengths.

We can do this using `sameNat` from
*[GHC.TypeNats](http://hackage.haskell.org/package/base/docs/GHC-TypeNats.html)*:

``` haskell
-- `Type` is just a synonym for * from Data.Kind
-- from the module Data.Type.Equality
data (:~:) :: k -> k -> Type where
    Refl :: x :~: x

sameNat
    :: (KnownNat n, KnownNat m)
    => Proxy n
    -> Proxy m
    -> Maybe (n :~: m)
```

The only way we can have a non-bottom value of type `n :~: m` is with the `Refl`
constructor, which can only be used in the case that `n` and `m` are equal.
`sameNat` gives us that `Refl`, if possible --- that is, if `n` and `m` are
equal. If not, it gives us `Nothing`.

Now, we can write:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs#L78-L81

exactLength :: forall n m a. (KnownNat n, KnownNat m) => Vec n a -> Maybe (Vec m a)
exactLength v = case sameNat (Proxy @n) (Proxy @m) of
    Just Refl -> Just v     -- here, n ~ m, so a `Vec n a` is a `Vec m a`, too
    Nothing   -> Nothing
```

(We could also write this by using `getVector` and `mkVec`, which wraps and
unwraps, but let's pretend it is expensive to construct and re-construct).

Now we can do:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs#L37-L37

zipVec :: Vec n a -> Vec n b -> Vec n (a, b)

-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrapped.hs#L83-L89

zipSame :: forall a b. V.Vector a -> V.Vector b -> Maybe (V.Vector (a, b))
zipSame v1 v2 = withVec v1 $ \(v1' :: Vec n a) ->
                withVec v2 $ \(v2' :: Vec m b) ->
      case exactLength v1' of
        Just v1Same -> Just $ getVector
                          (zipVec v1Same v2')     -- v1' has the same length as v2'
        Nothing     -> Nothing
```

Which will zip two unsized vectors, but only if their lengths are the same.

Now, "checking that the length is a certain length" is literally the least
interesting property we can test about our vectors. There are definitely more
interesting properties we can test, like whether or not our lengths are even or
odd, if they are greater than a certain number, etc.; for these, the process is
essentially the same: find some way, *at runtime*, to get some sort of witness
for the property you want. In our case, our witness was `n :~: m`, which
witnessed the fact that `n ~ m`. Different libraries might provide different
witnesses that might be useful. But the general process is

1.  Find a way to get your witness, using some runtime function (that will
    probably return `Maybe`)
2.  Pattern match on your witness, and see that property realized and usable by
    GHC/the type checker!

### Help from singletons

You have probably heard that `TypeNats` provides a very bare-bones and primitive
interface. This is true. Its interface also sometimes doesn't play well with
other type-level mechanisms you might want to try. To prepare you for the real
world, let's re-implement these things using the
*[singletons](http://hackage.haskell.org/package/singletons)* library, which
provides a unified interface for type-level programming in general.

Instead of `KnownNat`, `Proxy`, `natVal`, `SomeNat`, and `someNatVal`, we can
use the singletons equivalents, `Sing`, `fromSing`, `SomeSing`, and
`toSing`:[^2]

``` haskell
-- TypeNats style
natVal :: KnownNat n => p n -> Natural

-- Singletons style
sing     :: KnownNat n => Sing n
fromSing :: Sing n -> Natural       -- (for n :: Nat)

-- TypeNats style
data SomeNat = forall n. KnownNat n => SomeNat (Proxy n)
someNatVal :: Natural -> SomeNat

-- Singletons style
data SomeSing Nat = forall n. SomeSing (Sing n)
toSing :: Natural -> SomeSing Nat

withSomeSing :: Natural -> (forall n. Sing n -> r) -> r

-- TypeNats style
sameNat :: (KnownNat n, KnownNat m) => Proxy n -> Proxy m -> Maybe (n :~: m)

-- Singletons style
-- from Data.Singletons.Decide
-- for our purposes, Decision is basically a fancy Maybe
data Decision a = Proved a | Disproved (a -> Void)
(%~) :: Sing n -> Sing m -> Decision (n :~: m)
```

Hopefully the above should give you a nice "key" for translating between the two
styles. But here are some practical translations:

``` haskell
-- "explicit Sing" style
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrappedSingletons.hs#L20-L24

mkVec_ :: Sing n -> V.Vector a -> Maybe (Vec n a)
mkVec_ s v | V.length v == l = Just (UnsafeMkVec v)
           | otherwise       = Nothing
  where
    l = fromIntegral (fromSing s)

-- "implicit" style
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrappedSingletons.hs#L26-L30

mkVec :: forall n a. KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVec v | V.length v == l = Just (UnsafeMkVec v)
        | otherwise       = Nothing
  where
    l = fromIntegral (fromSing (sing :: Sing n))

-- alternatively, re-using `mkVec_`
mkVec :: KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVec = mkVec_ sing
```

As you can see, in singletons, we have the luxury of defining our functions in
"explicit" style (where the user passes in a `Sing` token which reveals what
length they want) or "implicit" style (where the length is inferred from the
return type, requiring a `KnownNat n =>` constraint), like we have been writing
up to this point. `Sing n ->` and `KnownNat n =>` really have the same power.
You can think of `Sing n` as a token that carries around `KnownNat n =>`, in a
way.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrappedSingletons.hs#L32-L42

replicate_ :: Sing n -> a -> Vec n a
replicate_ s x = UnsafeMkVec $ V.replicate l x
  where
    l = fromIntegral (fromSing s)

replicate :: KnownNat n => a -> Vec n a
replicate = replicate_ sing

withVec :: V.Vector a -> (forall n. Sing n -> Vec n a -> r) -> r
withVec v f = case toSing (fromIntegral (V.length v)) of
    SomeSing s -> f s (UnsafeMkVec v)

-- alternatively, skipping `SomeSing` altogether:
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrappedSingletons.hs#L44-L54

withVec' :: V.Vector a -> (forall n. Sing n -> Vec n a -> r) -> r
withVec' v0 f = withSomeSing (fromIntegral (V.length v0)) $ \s ->
    f s (UnsafeMkVec v0)

exactLength_ :: Sing m -> Sing n -> Vec n a -> Maybe (Vec m a)
exactLength_ sM sN v = case sM %~ sN of
    Proved Refl -> Just v
    Disproved _  -> Nothing

exactLength :: (KnownNat m, KnownNat n) => Vec n a -> Maybe (Vec m a)
exactLength = exactLength_ sing sing
```

Note that you *aren't* required to implement both a `replicate_` and `replicate`
--- I'm just including them here to show that both API's (implicit and explicit)
are possible. (You can always just directly use `sing` right away before getting
started to get the `Sing n` that those functions use, and so skip `replicate_`
and other explicit variants)

One slight bit of friction comes when using libraries that work with `KnownNat`,
like *finite-typelits* and the `Finite` type. But we can convert between the two
using `SNat` or `withKnownNat`

``` haskell
-- SNat can be used to construct a `Sing` if we have a `KnownNat` constraint
-- It can also be pattern matched on to reveal a `KnownNat constraint`
SNat :: KnownNat n => Sing n

-- we can give a `Sing n` and be able to execute something in the context where
-- that `n` has a `KnownNat` constraint
withKnownNat :: Sing n -> (KnownNat n => r) -> r
```

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrappedSingletons.hs#L56-L60

generate_ :: Sing n -> (Finite n -> a) -> Vec n a
generate_ s f = withKnownNat s $
    UnsafeMkVec $ V.generate l (f . fromIntegral)
  where
    l = fromIntegral (fromSing s)

-- alternatively, via pattern matching:
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrappedSingletons.hs#L63-L66

generate'_ :: Sing n -> (Finite n -> a) -> Vec n a
generate'_ s@SNat f = UnsafeMkVec $ V.generate l (f . fromIntegral)
  where
    l = fromIntegral (fromSing s)

-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrappedSingletons.hs#L68-L69

generate :: KnownNat n => (Finite n -> a) -> Vec n a
generate = generate_ sing
```

You can see most of our original code (with pure `KnownNat`) rewritten to work
with singletons in [this
file](https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecWrappedSingletons.hs).

#### Why Singletons?

As you can see, singletons-style programming completely subsumes programming
with `TypeNats` and `KnownNat`. What we don't see here is that singletons style
integrates very well with the rest of the singletons ecosystem...so you might
just have to take my word for it :)

What we have just witnessed is the bridge between the singletons ecosystem and
the rest of the Haskell ecosystem's usage of `GHC.TypeNats`. `KnownNat`, because
it is provided by GHC itself, is universal. However, I recommend any new
projects or libraries you write that do *anything* more than the most trivial of
usages of `KnownNat` should take a look at doing things singletons-style.

Working with just `GHC.TypeNats` and `KnownNat`, you run into limitations very
quickly unless you stick to very basic things. And, if you ever work with any
other type-level stuff, *singletons* integrates very well and very smoothly with
everything else type-level you do. If you plan on doing other type-level things
besides just the most basic, you will not regret starting singletons-style from
the beginning.

### Real-World Examples

This exact pattern is used in many real-world libraries. The canonical
fixed-length vector library implemented in this style is
*[vector-sized](http://hackage.haskell.org/package/vector-sized)*, which more or
less re-exports the entire *[vector](http://hackage.haskell.org/package/vector)*
library, but with a statically-sized interface. This is the library I use for
all my my modern sized-vector needs.

It's also used to great benefit by the
*[hmatrix](http://hackage.haskell.org/package/hmatrix)* library, which I take
advantage of in my [dependently typed neural
networks](https://blog.jle.im/entries/series/+practical-dependent-types-in-haskell.html)
tutorial series.

It's also provided in the
*[linear](http://hackage.haskell.org/package/linear-1.20.7/docs/Linear-V.html)*
library, which was one of the first major libraries to adopt this style.
However, it offers an incomplete API, and requires lens --- its main purpose is
for integration with the rest of the
*[linear](http://hackage.haskell.org/package/linear-1.20.7/docs/Linear-V.html)*
library, which it does very well.

Anyway, if all you really wanted was performant fixed-size containers, feel free
to stop reading now (or jump to the conclusion). But if you want to explore a
bit deeper into the world of inductive dependent types ... continue on :)

## The Structural Way

So, the (a?) problem with `TypeNats` from GHC is that it has no internal
structure. It's basically the same as the `Integer` or `Natural` type --- every
single value (constructor) is completely structurally unrelated to the next.

Just like we can imagine

``` haskell
data Int = .. -2 | -1 | 0 | 1 | 2 ...
```

We can also think of `Nat` as just `0 | 1 | 2 | 3 | 4 ...`. Each constructor is
completely distinct.

This is useful for most practical applications. However, when we want to use our
fixed-length types in a more subtle and nuanced way, it might help to work with
a length type that is more...structurally aware.

We've also noticed that the structure of our `Vec` and the structure of our
`Nat` have nothing in common, so we can't take advantage of any shared structure
to help us with type-safety in our implementation...everything we wrote was
pretty much implemented using "unsafe" functions.

So, enough of this non-structural blasphemy. We are proper dependent type
programmers, dangit! We want structural verification! Compiler verification from
the very bottom!

For this, we'll dig into *inductive* type-level nats.

``` haskell
data Nat = Z | S Nat
  deriving Eq
```

We're using the `DataKinds` extension, so not only does that define the *type*
`Nat` with the *values* `Z` and `S :: Nat -> Nat`, it also defines the *kind*
`Nat` with the *types* `'Z` and `'S :: Nat -> Nat`! (note the backticks)

``` haskell
ghci> :t S Z
Nat
ghci> :k 'S 'Z
Nat
```

So `'Z` represents 0, and `'S` represents the "successor" function: one plus
whatever number it contains. `'S 'Z` represents 1, `'S ('S 'Z)` represents 2,
etc.

And now we can define a fixed-length *list*, which is basically a normal haskell
list "zipped" with `S`s.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L26-L30

data Vec :: Nat -> Type -> Type where
    VNil :: Vec 'Z a
    (:+) :: a -> Vec n a -> Vec ('S n) a

infixr 5 :+
```

Here, we're using `GADT` syntax to define our type using its constructors: the
`VNil` constructor (which creates a `Vec 'Z a`, or the empty vector, like `[]`)
and the `(:+)` constructor (like cons, or `(:)`), which conses an item to a
`Vec n a` to get a `Vec ('S n) a`, or a vector with one more element.

Basically, all usage of nil and cons (`VNil` and `:+`) keeps track of the
current "length" of the vectors in its type. Observe that the only way to
construct a `Vec ('S ('S 'Z)) a` is by using two `:+`s and a `VNil`!

``` haskell
ghci> :t VNil
Vec 'Z a
ghci> :t True :+ VNil
Vec ('S 'Z) Bool
ghci> :t False :+ True :+ VNil
Vec ('S ('S 'Z)) Bool
```

### Type-level Guarantees are Structurally Free

One nice thing about this is that there is no "unsafe" way to construct a `Vec`.
Any `Vec` is *inherently of the correct size*. The very act of constructing it
enforces its length.

Remember our "unsafe" `mapVec`? We had to implement it unsafely, and trust that
our implementation is correct. Even worse --- our *users* have to trust that our
implementation is correct!

But writing such a `mapVec` function using `Vec` is guaranteed to preserve the
lengths:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L32-L35

mapVec :: (a -> b) -> Vec n a -> Vec n b
mapVec f = \case
    VNil    -> VNil
    x :+ xs -> f x :+ mapVec f xs

-- compare to
map :: (a -> b) -> [a] -> [b]
map f = \case
    [] -> []
    x:xs -> f x : map f xs
```

Our implementation is guaranteed to have the correct length. Neat! We get all of
the documentation benefits described in our previous discussion of `mapVec`,
plus more.

We can write `zip` too:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L37-L42

zipVec :: Vec n a -> Vec n b -> Vec n (a, b)
zipVec = \case
    VNil -> \case
      VNil -> VNil
    x :+ xs -> \case
      y :+ ys -> (x,y) :+ zipVec xs ys
```

Isn't it neat how the code reads exactly like the code for map/zip for *lists*?
Because their structure is identical, their only real difference is the
type-level tag. All of the functions we write are the same.

#### Type-Level Arithmentic

GHC provided our `+` before, so we have to write it ourselves if we want to be
able to use it for our `Nat`s. We can write it as a type family:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L54-L61

type family (n :: Nat) + (m :: Nat) :: Nat where
    'Z   + m = m
    'S n + m = 'S (n + m)

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
(++) = \case
    VNil    -> \ys -> ys
    x :+ xs -> \ys -> x :+ (xs ++ ys)
```

This works! However, we have to be careful that GHC can verify that the final
vector *really does* have the length `n + m`. GHC can do this automatically only
in very simple situations. In our situation, it is possible because `+` and `++`
have the *exact same structure*.

Take a moment to stare at the definition of `+` and `++` very closely, and then
squint really hard. You can see that `+` and `++` really describe the "same
function", using the exact same structure. First, if the first item is a Z-y
thing, return the second item as-is. If the first item is a consy thing, return
the second item consed with the rest of the first item. Roughly speaking, of
course.

This is a part of what we mean when we say that we can take advantage of the
*structure* of the length type. Here, the structure of `Nat` aligns so well with
the structure of `Vec` what we can prove structural properties about `Nat` and
the `Vec` together by exploiting their shared inductive structure.

Unfortunately, for examples where the function we write doesn't exactly match
the structure as the type family we write, this won't work. And sometimes, the
structural properties might get in the way of what we are trying to
prove/produce. An example here would be a `snoc` function (cons to the end of a
list). If you try writing it, you'll see that the structure of `Nat` and `Vec`
fight back against you pretty hard. So, exploiting structure isn't universally
useful, but it definitely helps in many situations! Handling tricky cases like
this is a subject for a whole other blog post.

### Indexing

To index our previous type, we used some abstract `Finite` type, where
`Finite n` conveniently represented the type of all possible indices to a
`Vec n a`. We can do something similar, inductively, as well.

Let's think about this inductively. How would we construct a valid index into a
vector of size `n`? Well, there are two ways:

1.  We can always make a "zeroth" index for a vector of size `'S n`, to get the
    first item.
2.  If we have an index into the ith item of a vector of size `n`, then we have
    an index into the i+1th item of a vector of size `'S n`.

We can write this out as a GADT:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L63-L68

data Fin :: Nat -> Type where
    FZ :: Fin ('S n)  -- ^ we can create a 0th index if Vec n is non-empty
    FS :: Fin n       -- ^ if we have an ith index into a vector of size n
       -> Fin ('S n)  -- ... then we have an i+1th index into a vector of size ('S n)

deriving instance Show (Fin n)
```

If you play around it enough, you might be able to convince yourself that there
are exactly `n` inhabitants of `Fin n`.

For example, for `Fin ('S 'Z)` (indices for a one-item vector), there should be
only one inhabitant. And there is! It's `FZ`. `FS FZ` is not a valid inhabitant,
because it has type `Fin ('S ('S m))` for some `m`, so cannot possibly have the
type `Fin ('S 'Z)`.

Let's see the inhabitants of `Fin ('S ('S ('S 'Z)))` (indices for three-item
vectors):

``` haskell
ghci> FZ              :: Fin ('S ('S ('S 'Z)))
FZ
ghci> FS FZ           :: Fin ('S ('S ('S 'Z)))
FS FZ
ghci> FS (FS FZ)      :: Fin ('S ('S ('S 'Z)))
FS (FS FZ)
ghci> FS (FS (FS FZ)) :: Fin ('S ('S ('S 'Z)))
TYPE ERROR!  TYPE ERROR!  TYPE ERROR!
```

As GHC informs us, `FS (FS (FS FZ))` is not an inhabitant of
`Fin ('S ('S ('S 'Z)))`, which is exactly the behavior we wanted. This is
because `FS (FS (FS FZ))` has type `Fin ('S ('S ('S ('S m))))` for some `m`, and
this can't fit `Fin ('S ('S ('S 'Z)))`.

Also, note that there are no inhabitants of `Fin 'Z`. There is no constructor or
combination of constructors that can yield a value of that type.

Armed with this handy `Fin` type, we can do structural type-safe indexing:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L70-L75

index :: Fin n -> Vec n a -> a
index = \case
    FZ -> \case
      x :+ _ -> x
    FS i -> \case
      _ :+ xs -> index i xs
```

Note that our `Fin` type structurally precludes us from being able to index into
a `Vec 'Z a` (an empty vector), because to do that, we would have to pass in a
`Fin 'Z`...but there is no such value with that type!

### Generating

Now, generating these requires some more thought. Naively writing a
`replicate :: a -> Vec n a` is not possible; ideally, we'd want to "pattern
match" on our length `n`, and use `VNil` if it's `'Z`, etc.

However, we can't pattern match on types in Haskell, because types are *erased*
at runtime. They're just used by the compiler to verify your code, but they
don't exist at runtime. So, you can't just say "do this if `n` is `'Z`,
otherwise do this".

Recall that, in our previous vector type, we needed to use a `KnownNat n`
constraint to be able to *reflect* a `n` type down to the value level. We can do
something similar using the *singletons* machinery!

First, we need to get singletons for our `Nat`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L21-L24

$(singletons [d|
  data Nat = Z | S Nat
    deriving Eq
  |])

-- this creates:
data instance Sing :: Nat -> Type where
    SZ :: Sing 'Z
    SS :: Sing n -> Sing ('S n)
```

`Sing n` is a singleton for our `Nat`, in that there is only one `Sing n` for
every `n`. So, if we receive a value of type `Sing n`, we can pattern match on
it to figure out what `n` is. Essentially, we can *pattern match* on `n`.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L77-L84

singSize :: Sing (n :: Nat) -> String
singSize = \case
    -- here, n is 'Z
    SZ        -> "Size of zero!"
    -- here, n is ('S 'Z)
    SS SZ     -> "Size of one!"
    -- here, n is ('S ('S n))
    SS (SS _) -> "Wow, so big!"
```

We can now branch depending on what `n` is!

Basically, *we can use a singleton* if we ever want to "pattern match" or branch
our program's output based on the type. This is a general rule you will observe
as we continue on this article.

Note that because of the inductive nature of our original `Nat` type, the
singletons are also inductive, as well. This is handy, because then our whole
ecosystem remains inductive.

Now, to write `replicate`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L91-L94

replicate_ :: Sing n -> a -> Vec n a
replicate_ = \case
    SZ   -> \_ -> VNil
    SS l -> \x -> x :+ replicate_ l x
```

And we can recover our original "implicit" style, with type-inference-driven
lengths, using `SingI` and `sing :: SingI n => Sing n`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L96-L97

replicate :: SingI n => a -> Vec n a
replicate = replicate_ sing
```

You can think of `SingI` as the "generic singletons" equivalent of `KnownNat`.
`KnownNat` lets us reflect out a `GHC.TypeNats.Nat` to a `Sing`...`SingI` lets
us reflect any type that has singletons defined to its corresponding `Sing`.
Since our new `Nat` type has singletons, we basically get a free "`KnownNat`
equivalent"!

See how useful the whole singletons ecosystem is? :)

#### Generating with indices

Writing `generate` using the inductive `Fin` and `Nat` is an interesting
challenge. It's actually a fairly standard pattern that comes up when working
with inductive types like these. I'm going to leave it as an exercise to the
reader -- click the link at the top corner of the text box to see the solution,
and see how it compares to your own :)

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L99-L105

generate_ :: Sing n -> (Fin n -> a) -> Vec n a

generate :: SingI n => (Fin n -> a) -> Vec n a
generate = generate_ sing
```

The one thing I will point out is that it is very useful that GHC verifies our
code for us, and that we have typed holes to help us develop our code. If we
ever don't know something, we can just use a typed hole `_`, and GHC will tell
us what type it expects, and what values in scope have that type. It is
infinitely useful for situations like this, especially when you are new to this
sort of dependently typed inductive programming!

If you ever get stuck, try throwing in a `_` and seeing what types GHC
expects...these clues will help you get your bearings!

### Between Sized and Unsized

Converting from sized to unsized vectors (to lists) is something that is pretty
straightforward, and can be done by just pattern matching on the vector and
recursing on the tail. I've [left it as an
exercise](https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L86-L89)
to write `Vec n a -> [a]`.

More interesting is the other way around; our the API of converting unsized to
sized vectors will be the same:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L107-L107

withVec :: [a] -> (forall n. Sing n -> Vec n a -> r) -> r
```

But implementing it inductively is also an interesting challenge. See my tip
above about typed holes (`_`). I recommend taking a break here to try to solve
it yourself.

Ready?

Welcome back! Hope you had a fun time :) Here's the solution!

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L107-L111

withVec :: [a] -> (forall n. Sing n -> Vec n a -> r) -> r
withVec = \case
    []   -> \f -> f SZ VNil
    x:xs -> \f -> withVec xs $ \l ys ->
        f (SS l) (x :+ ys)
```

To handle the empty list, we just return immediately, giving `f` the proper
singleton and vector (`SZ` and `VNil`). For the non-empty list, first we convert
the tail `xs` into a vector (`ys`) and its corresponding length-singleton (`l`),
and then we give `f` the "correct" length singleton of our complete vector
(`SS l`) and the correct complete vector (`x :+ ys`)

One nice property where (in contrast with our previous non-structural `withVec`)
is that GHC ensures that the length of the vector we give to `f` is actually
what we claim it is.

### Verifying properties

We can create some corresponding example of `exactLength` using the exact same
process we did before

First, it'd be nice to get a witness for the length of a given vector just from
the vector itself:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L113-L116

vecLength :: Vec n a -> Sing n
vecLength = \case
    VNil    -> SZ
    _ :+ xs -> SS (vecLength xs)
```

The type of `vecLength :: Vec n a -> Sing n` says that it is possible, from the
structure of the vector given alone, to get a witness to its length. And,
because the structure of the vector and the structure of the length type are so
similar, this is possible! (Note that this is not possible for our
non-structural "wrapped" `Vec`, without some unsafe operations)

Now, our code will be identical to the code for our wrapped/non-structural
vectors, using `%~` and `Decision` and `Refl`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L118-L124

exactLength_ :: Sing m -> Vec n a -> Maybe (Vec m a)
exactLength_ sM v = case sM %~ vecLength v of
    Proved Refl -> Just v
    Disproved _ -> Nothing

exactLength :: SingI m => Vec n a -> Maybe (Vec m a)
exactLength = exactLength_ sing
```

It's nice that this is exactly the same as before, and that's a testament to how
useful the singletons library is at unifying all of these distinct type-level
stuffs.

We could also write `exactLength` in a cute way by inducting on the length we
want and the vector, so it might be fun to look at this version instead --

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L126-L136

exactLengthInductive_ :: Sing m -> Vec n a -> Maybe (Vec m a)
exactLengthInductive_ = \case
    SZ -> \case
      VNil   -> Just VNil
      _ :+ _ -> Nothing
    SS l -> \case
      VNil    -> Nothing
      x :+ xs -> (x :+) <$> exactLengthInductive_ l xs

exactLengthInductive :: SingI m => Vec n a -> Maybe (Vec m a)
exactLengthInductive = exactLengthInductive_ sing
```

This is another way you can take advantage of the *structure* of the length
type. Here, we explicitly take advantage of the inductive structure of the `Nat`
type and how it matches with the structure of the `Vec` type, and do bold things
with it![^3]

But I digress. Like in the last section, checking for a given length is
literally the least interesting property you can check for. But, again, the same
process is usable here: find a way to get your witness, and then pattern match
on that witness.

For example, we can make a witness that `n` is less than or equal to `m`, as
well as a way to construct such a witness:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L138-L150

data LTE :: Nat -> Nat -> Type where
    LEZ :: LTE 'Z n
    LES :: LTE n m -> LTE ('S n) ('S m)

isLTE :: Sing n -> Sing m -> Decision (LTE n m)
isLTE = \case
    SZ   -> \_ -> Proved LEZ
    SS n -> \case
      SZ -> Disproved $ \case       -- EmptyCase
      SS m -> case isLTE n m of
        Proved l    -> Proved $ LES l
        Disproved p -> Disproved $ \case
          LES l -> p l
```

So, it is impossible to construct an `LTE n m` if `n` is *not* less than or
equal to `m`. I dare you to try!

We can write code to check for this property in our vectors:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L152-L158

atLeast_ :: Sing n -> Vec m a -> Maybe (LTE n m, Vec m a)
atLeast_ sN v = case isLTE sN (vecLength v) of
    Proved l    -> Just (l, v)
    Disproved _ -> Nothing

atLeast :: SingI n => Vec m a -> Maybe (LTE n m, Vec m a)
atLeast = atLeast_ sing
```

`atLeast_ sN` will only return our vector if its length is *at least* the length
of the length indicated by `sN`. Basically, we check if our vector is "at least"
a certain length.

We can write a function that can "take" an arbitrary amount from a vector, given
(via proof) that the vector has at least that many elements:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L160-L164

takeVec :: LTE n m -> Vec m a -> Vec n a
takeVec = \case
    LEZ   -> \_ -> VNil
    LES l -> \case
      x :+ xs -> x :+ takeVec l xs
```

And, we can combine that with our `atLeast` function, to be able to take
(maybe)[^4] from any vector:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/fixvec-2/VecInductive.hs#L166-L170

takeVecMaybe_ :: Sing n -> Vec m a -> Maybe (Vec n a)
takeVecMaybe_ sN v = uncurry takeVec <$> atLeast_ sN v

takeVecMaybe :: SingI n => Vec m a -> Maybe (Vec n a)
takeVecMaybe v = uncurry takeVec <$> atLeast v
```

### In the Real World

This type is more like a list than a vector, so it's in a bit of an awkward
position, utility-wise. You usually chose a list over a vector in Haskell when
you want some sort of lazy streaming, but the cases where you want to lazily
stream something *and* you know exactly how many items you want to stream are
admittedly a bit rare. GHC can't handle infinite `Vec`s, so there's that, too.
For "containers", *vector* is great, so the non-structural `Vec` is seen a lot
more.

However, if you are working with a lot of other inductive types, `Vec` works
very naturally alongside them. It makes sense, then, that a "canonical" package
offering `Vec` is
*[type-combinators](http://hackage.haskell.org/package/type-combinators)*, an
actively maintained library with loads of useful inductive types for type-level
programming, exporting its own `Nat` and `Sing`-equivalents. If I am doing the
sort of type-level programming that `Vec` is useful for, chances are I already
have *type-combinators* imported. This is the library that I personally suggest
if you want to use this `Vec` in the real world.

## Wrapping up

There's obviously more to look at, and much more we can do with fixed-length
vectors and inductive types. And, there will definitely be more issues that come
up when you start working with these in the real world, with real applications.

If you plan on moving into learning about dependent types, I hope that guide
would be a good launching point. But if all you wanted to do was learn how to
use fixed-length vectors effectively in Haskell...hopefully after reading this,
you have confidence to work with these things directly, and to know what to
google if anything else comes up :)

Feel free as always to leave a comment or a [tweet](https://twitter.com/mstk),
or find me the freenode `#haskell` channel, as *jle\`*. I always welcome
feedback, suggestions, or questions!

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

[^1]: Users who are used to GHC 8.0 and below might remember `Nat` coming from
    *[GHC.TypeLits](http://hackage.haskell.org/package/base/docs/GHC-TypeLits.html)*.
    Well, GHC 8.2 is here, `TypeLits` is out, `TypeNats` is in. The difference
    is that, in `TypeLits`, the `Nat` kind reifies/reflects with `Integer`. In
    `TypeNats`, the `Nat` kind reifies/reflects with `Natural` from
    *[Numeric.Natural](http://hackage.haskell.org/package/base/docs/Numeric-Natural.html)*.

[^2]: For singletons \> 2.3 `fromSing` and `toSing` give and take `Natural` when
    going to `Nat`. However, for 2.3.1 and below, they give/take `Integer`
    instead.

[^3]: Note, however, that if you unroll the definition of `%~` for `Nat`, you
    pretty much get the exact same thing.

[^4]: Remember the whole point of this exercise --- that the `Maybe` is required
    only in the completely polymorphic case, where we get our lengths at runtime
    and don't know them at compile-time. If we *knew* `n` and `m` at
    compile-time, and knew that `n` was less than or equal to `m`, we could
    construct an `LTE n m` and call `takeVec` directly, and not return a
    `Maybe`.

