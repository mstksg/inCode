The Const Applicative and Monoids

==================================

> Originally posted by [Justin Le](https://blog.jle.im/) on May 8, 2018.
> [Read online!](https://blog.jle.im/entry/const-applicative-and-monoids.html)

The Applicative typeclass has a somewhat infamous reputation for having opaque
laws. There are a lot of great
[alternative](https://wiki.haskell.org/Typeclassopedia#Alternative_formulation)
[rephrasing](https://www.reddit.com/r/haskell/comments/2lompe/where_do_the_applicative_laws_come_from/clws90h/)
of these laws, from many different approaches. For this post, however, I want to
talk about Applicative in terms of one of my favorites: `Const`.

## Const

The `Const` data type from the standard libraries is relatively simple as far as
newtypes go:

``` haskell
newtype Const w a = Const { getConst :: w }
```

However, let's look at a less polymorphic version, `IntConst`, which is
essentially `Const Int`:[^1]

``` haskell
newtype IntConst a = IntConst { getIntConst :: Int }
```

For a `IntConst a`, the `a` is a *phantom* type parameter. This means that there
are not necessarily any values of type `a` in a value of type `IntConst a`. In
modern GHC with PolyKinds, this means that `a` might not even be a type that can
have values --- you might have, say, a value of type `IntConst Maybe`, or a
value of type `IntConst Monad`, and GHC would be perfectly happy.

`IntConst` admits a straightforward `Functor` instance that is a lot like the
`Functor` instance for `Proxy` and `Either e`:

``` haskell
instance Functor IntConst where
    fmap _ (IntConst w) = IntConst w
```

In fact, sometimes I like to refer to `Const w a` as "an `Either w a` with only
`Left`, no `Right`". The `Functor` instance reflects this pretty well:

``` haskell
instance Functor (Either e) where
    fmap _ (Left e)  = Left e        -- just like 'Const'
    fmap f (Right x) = Right (f x)   -- who cares
```

However, the `Applicative` instance of `IntConst` is one of my favorite things
about it. Let's try to imagine how we'd write it.

First of all, let's look at the types of the functions we need:

``` haskell
pure  :: a -> IntConst a
(<*>) :: IntConst (a -> b) -> IntConst a -> IntConst b
```

Now, remember that `IntConst`'s type parameter is phantom, so we don't have any
actual values of type `a -> b`, `a`, or `b` involved. An `IntConst a`, for any
`a`, is really just an `Int`. Essentially, once we strip out the newtype wrapper
shenanigans (replacing `IntConst a` with its contents, `Int`), we just get:

``` haskell
pure  :: a -> Int
(<*>) :: Int -> Int -> Int
```

We now have a few options on how to implement these. Let's try one and see if it
works:

``` haskell
instance Applicative IntConst where
    pure _                    = IntConst 42
    IntConst x <*> IntConst _ = IntConst x
```

A perfectly reasonable implementation, right? Our Applicative instance
type-checks. And if it type-checks, ship it! Time to call it a day and go home,
right?

Not quite.

## Applicative

Let's take a detour through the essense of the `Applicative` typeclass. Or, at
least, one particular interpretation of it that makes sense for instances that
represent some sort of effectful action.

The essence of Applicative, to me, is a way to combine values representing some
sort of "effect" in a sane way. In Haskell, we often talk about data types as
representing/describing some sort of effects. Applicative lets us combine
("sequence") them in a way that allows us to write powerful generic combinators.

One way to look at it is as a generalization of `fmap` to taking two parameters:

``` haskell
fmap1 :: (a -> b     ) -> F a -> F b
fmap2 :: (a -> b -> c) -> F a -> F b -> F c
```

`fmap` alone lets you take a single `F a` and transform it into an `F b`.
`fmap2` (or, `liftA2` in the standard libraries) is a way of taking *two*
`F`-values and squishing them into one fat `F` value.

It does this by letting us talk about combining the *effects* of an `F a`,
independent of the `a` (the result). For example, `sequenceA_`:

``` haskell
sequenceA_ :: Applicative f => [f a] -> f ()
```

Basically will take a list of `f a`s, and return a new `f ()` that has *all* of
the effects of the `f a`s in the list.

To do this sensibly, we need also to talk about a "no-op" value:

``` haskell
pure :: a -> F a
```

`pure x` is intended to be a no-op with "no effects".

With this, we can say something about the behavior of `<*>` or `fmap2` or
`liftA2`. Namely:

1.  The effects of `f <*> x` must have the effects of `f` once *and* the effects
    of `x` once -- no more, and no less.

2.  `pure`'s results must have no "effects", and so if used with `<*>`,
    introduces no extra effects:

    ``` haskell
    pure f <*> x = fmap f x
    f <*> pure x = fmap ($ x) f
    ```

    (Remember that `fmap` is not allowed to affect "effects" in any way)

3.  Combining effects must be associative.

With this guarantee, we can write `sequenceA_` in a polymorphic way:

``` haskell
sequenceA_ :: Applicative f => [f a] -> f ()
sequenceA_ []     = pure ()
sequenceA_ (x:xs) = x *> sequenceA_ xs
```

And we can say with certainty that:

1.  Each "effect" of each value in the `[f a]` will be executed *exactly* once:
    no more, and no less.
2.  `sequenceA_` of an empty list has no effects.

This makes `sequenceA_` a *useful* combinator. The fact that we can talk about
how `sequenceA_` behaves for all Applicative instances makes it something that
is *worth* defining. If you use `sequenceA_` for your type, you can do it
knowing that it will behave in a well-defined way: it *must* execute every
action once (no more, and no less), and sequencing an empty list *must* have no
effects.

If it weren't for those Applicative laws and expectations, `sequenceA_` would be
a pretty useless function. It might completely ignore all effects, or it might
perform some of the effects multiple times...who knows! The fact that we have
Applicative laws and expectations means we can look at the implementation of
`sequenceA_` and know with certainty (and make bold claims about) how
`sequenceA_` combines effects.

For example, we can always make the program substitution:

``` haskell
sequenceA_ xs *> sequenceA_ ys
-- can be substituted with
sequenceA_ (xs ++ ys)
```

If `sequenceA_` combines all the effects in `xs` once, and `sequenceA_` combines
all the effects in `ys` once, and `*>` combines the effects of either side once,
then this is a *legal substitution* that doesn't change what your program does.

However, if the `Applicative` instance doesn't have any rules, we can't do this.
For example, the original form uses `pure ()` *twice* (once for each list's `[]`
end), and the second form uses `pure ()` *once* (since there's only one list).
If `pure ()` was allowed to have effects...then the first version would have
more effects than the second.

## Back to Const

With this new information in mind, let's revisit our instance for `IntConst`.

In order for `IntConst`'s Applicative instance to behave meaningfully, and in
order to be able to match with user expectations of `Applicative` instance, you
need to make sure it follows the basic principles I mentioned earlier (the
effects of `f <*> x` has the effects of `f` and `x` exactly once, and the
properties about `pure`).

We haven't defined what the "effects" of `IntConst` are yet, but let's at least
look at if our `pure` behaves sensibly with `<*>`. Namely, let's check
`pure f <*> x = fmap f x`.

Note that this is a meaningful starting point because `fmap`'s definition is
*fixed*. For any type, there is *at most one* possible `fmap` that is legal and
lawful --- and in Haskell, we only have to check that `fmap id` leaves all
inputs unchanged.[^2]

With that out of the way, let's check our `pure f <*> x = fmap f x` law with a
simple example for `x`...say, `IntConst 5`. On the left hand side, we have:

``` haskell
-- pure _                     = IntConst 42
-- IntConst x <*> InstConst _ = IntConst x
pure f <*> IntConst 5 = IntConst 42 <*> IntConst 5
                      = IntConst 42
```

On the right hand side, we have:

``` haskell
-- fmap f (IntConst x) = IntConst x
fmap f (IntConst 5) = IntConst 5
```

It is clear that this definition does not work, since `IntConst 42` is not the
same as `IntConst 5`.

What if we defined:

``` haskell
instance Applicative IntConst where
    pure _                    = IntConst 42
    IntConst _ <*> IntConst y = IntConst y
```

Is that any better? Well, `pure f <*> IntConst 5` is now equal to `IntConst 5`,
so that works out. But what about `f <*> pure x = fmap ($ x) f`? Let's use
`IntConst 3` as our `f`. On the left hand side:

``` haskell
-- pure _                    = IntConst 42
-- IntConst _ <*> IntConst y = IntConst y
IntConst 3 <*> pure x = IntConst 3 <*> IntConst 42
                      = IntConst 42
```

And on the right hand side:

``` haskell
-- fmap f (IntConst x) = IntConst x
fmap ($ x) (IntConst 3) = IntConst 3
```

Ah, that's wrong too, then.

At this point it might seem like I am facetiously moving very slowly to an
answer that has to use *both* inputs. After all, my earlier statement claimed
that `f <*> x` has to use both the effects of `f` and the effects of `x`, each
exactly once. Because we didn't really know what the "effects" of `IntConst`
are, we don't know exactly "how" to combine them...but we can probably guess it
has to use both `Int`s. So, with that in mind, let's try another definition:

``` haskell
instance Applicative IntConst where
    pure _                    = IntConst 42
    IntConst x <*> IntConst y = IntConst (x + y)
```

Alright, now we use both `x` and `y` in the result. Let's see again if this
follows our expectations about `pure` -- if `pure f <*> x` is the same as
`fmap f x`. Using `IntConst 5` again as `x`:

``` haskell
-- pure _                     = IntConst 42
-- IntConst x <*> InstConst y = IntConst (x + y)
pure f <*> IntConst 5 = IntConst 42 <*> IntConst 5
                      = IntConst 47
```

On the right hand side, we have:

``` haskell
-- fmap f (IntConst x) = IntConst x
fmap f (IntConst 5) = IntConst 5
```

Another dead end. It looks like it isn't just enough that we *use* both
`Int`s...we have to use them in a way such that the `Int` we use as the result
of `pure f` as to be an *identity* to our operation. Whatever `Int` is returned
by `pure f` has to leave any other `Int` unchanged when used with `<*>`.

Thinking back, we remember that if our operation is `+`, we can use `0`, since
`0 + x = x` and `x + 0 = x`, for all `x`. Luckily, our operation `x + y` is one
that even *has* an identity. If we had chosen another operation (like
`x + 2 * y`), we wouldn't be so lucky. Finally:

``` haskell
instance Applicative IntConst where
    pure _                    = IntConst 0
    IntConst x <*> IntConst y = IntConst (x + y)
```

At last this feels like something that should make sense. And, does it? Testing
out, again, `pure f <*> x = fmap f x`:

``` haskell
-- pure _                     = IntConst 0
-- IntConst x <*> InstConst y = IntConst (x + y)
pure f <*> IntConst 5 = IntConst 0 <*> IntConst 5
                      = IntConst 5
```

``` haskell
-- fmap f (IntConst x) = IntConst x
fmap f (IntConst 5) = IntConst 5
```

Perfect! And, checking now `f <*> pure x = fmap ($ x) f`:

``` haskell
-- pure _                    = IntConst 0
-- IntConst x <*> IntConst y = IntConst (x + y)
IntConst 3 <*> pure x = IntConst 3 <*> IntConst 0
                      = IntConst 3
```

``` haskell
-- fmap f (IntConst x) = IntConst x
fmap ($ x) (IntConst 3) = IntConst 3
```

This definition works for both[^3]!

### The Effect of Const

With our definition picked out, what do we think `sequenceA_` does for
`IntConst`?

``` haskell
sequenceA_ :: [IntConst a] -> IntConst ()
```

Well, if each application of `<*>` adds together the `Int` in the `IntConst a`,
and `sequenceA_` uses `<*>` once per every `IntConst a`...we can guess that
`sequenceA_` for `IntConst` is just `sum`!

This might be more clear if we strip away the newtype wrappers (replacing
`IntConst a` with its contents, `Int`):

``` haskell
sequenceA_ :: [Int] -> Int
```

From this definition of `<*>`, we can form an idea of what the effects of the
`IntConst` Applicative are: they *add* to some accumulator environment! And
`pure _ = IntConst 0` means that `pure _` adds zero to our accumulator -- it
leaves our accumulator *unchanged*, and so effectively has no effect.

That's why `sequenceA_` is `sum` -- it sequences every effect of the `IntConst`,
which means that it sequences all of those "add to the accumulator" effects one
after the other.

### Alternative Pictures

Note that the requirements we gave for the `Applicative` instance doesn't
necessarily imply that the one we have is the only instance. For example, the
following instance is also valid:

``` haskell
instance Applicative IntConst where
    pure _                    = IntConst 1
    IntConst x <*> IntConst y = IntConst (x * y)
```

If our "combining" action is `*`, then `pure` has to be an identity. So,
`pure _ = IntConst 1` works fine as an identity here, since `1 * x = x` and
`x * 1 = x`, for all `x`.

``` haskell
-- pure _                     = IntConst 1
-- IntConst x <*> InstConst y = IntConst (x * y)
pure f <*> IntConst 5 = IntConst 1 <*> IntConst 5
                      = IntConst 5
```

``` haskell
-- fmap f (IntConst x) = IntConst x
fmap f (IntConst 5) = IntConst 5
```

### A General Alternative

It looks like the Applicative for `IntConst` has to follow some pattern:

-   `<*>` has to combine the `Int`s inside somehow using some operation `f`.
    (`f` also has to be associative, which is a point we didn't touch on
    specifically)
-   The `Int` that `pure` returns has to be an *identity* to `f`.

Sound familiar?

This is all satisfied if and only if `f` and the result of `pure` form a
**[monoid](https://www.schoolofhaskell.com/user/mgsloan/monoids-tour)** on the
integers!

There is a very fundamental link here: the `Applicative` laws for `IntConst` are
satisfied if and only if `<*>` acts monoidally on the contents, with `pure`'s
result as the identity of that monoid.

(For those unfamiliar with monoids, a `Monoid` in Haskell is a type `w` that has
an associative operation `(<>) :: w -> w -> w` along with an identity
`mempty :: w` that leaves values unchanged when used with `<>`.)

So, *any* `f` and `pure` works, as long as they form a *monoid*. And any monoid
in the Integers is a valid `Applicative` instance for `IntConst`!

## General Const

Let's revisit our original `Const` type:

``` haskell
newtype Const w a = Const { getConst :: w }
```

The `Functor` instance is unique, so there isn't any leeway we have (`fmap` is
always fixed for every type):

``` haskell
instance Functor (Const w) where
    fmap _ (Const w) = Const w
```

This is the only definition that preserves `fmap id = id`.

Now we can actually write an `Applicative` instance for `Const w`...as long as
we provide a `Monoid` to use with `w`[^4]!

``` haskell
instance Monoid w => Applicative (Const w) where
    pure _              = Const mempty
    Const x <*> Const y = Const (x <> y)
```

Like how we said, as long as the "combining" function for `x` and `y` have the
identity that is given by the result of `pure`, this is a valid Applicative.

The "effects" of this Applicative instance are "accumulate to some accumulator".
If this sounds familiar, this is because this is exactly the effect of the
`Writer w` Applicative instance. `Const w` and `Writer w` have the same effects
("accumulate to some accumulator"), and this can be seen clearly by comparing
the two types:

``` haskell
data Const  w a = Const  w
data Writer w a = Writer w a
```

(`Const` is just `Writer` without the `a` value)

## What is Applicative Really?

If you think about this, this seems like a bit of a crazy coincidence.
Applicatives are an interesting concept, and Monoids are a different one.

But it looks like in order to make an `Applicative` instance for `Const w`, the
behavior of `<*>` and `pure` have to follow some certain properties in order to
fit the Applicative laws...and those properties are *exactly* monoidal
properties and the Monoid laws.

To illustrate this link, we can see the type of `pure` and `<*>` for `Const w`,
without the newtype wrappers (and ignored arguments)

``` haskell
instance Monoid w => Applicative (Const w) where
    pure  :: w
    (<*>) :: w -> w -> w
```

And let's look at the `Monoid` typeclass:

``` haskell
instance Monoid w where
    mempty :: w
    (<>)   :: w -> w -> w
```

It seems like `Const` is nothing more than a (type-level) function on a Monoid.
As an `* -> (k -> *)`, it takes a `*`-kinded Monoid and turns it into a
`k -> *`-kinded Monoid:

``` haskell
instance Monoid (w :: *) => Applicative (Const w :: k -> *)
```

"Give me a `Monoid` and I'll give you something `k -> *` that is also a monoid!"

`Const` is a *[monoid
homomorphism](https://bartoszmilewski.com/2015/07/21/free-monoids/)*: it takes a
monoid `w` with `mempty` and `(<>)`, and turns it into a monoid `Const w` with
`pure _` and `<*>`:

``` haskell
Const (x <> y) = Const x <*> Const y
Const mempty   = pure ()
```

Meaning "`<>` then `Const`" is the same as "`Const` then `<*>`", and "`Const`
the `mempty`" is the same as `pure ()`. Both things essentially convey the exact
same monoid -- one with `<>` and `mempty`, and the other with `<*>` and
`pure ()`. In fact, it's a bit more than a monoid homomorphism -- it's a
**monoid isomorphism**:

``` haskell
getConst x <> getConst y = getConst (x <*> y)
mempty                   = getConst (pure ())
```

Which means "`getConst` then `<>`" is the same as "`<*>` then `getConst`", and
`mempty` is the same as `getConst (pure ())`. `getConst` takes you from one
monoid (with `<*>` and `pure ()`) to another (with `<>` and `mempty`).

One incidental observation -- `sequenceA_` for `Const w` might look familiar:

``` haskell
sequenceA_ :: Monoid w => [Const w a] -> Const w ()

-- strip out newtype wrappers
sequenceA_ :: Monoid w => [w] -> w
```

It's just `mconcat`!

As an exercise, see if you can understand this definition of `mconcat` in terms
of `Const` and `traverse`:

``` haskell
mconcat :: Monoid w => [w] -> w
mconcat = getConst . traverse_ Const
```

`traverse_`, if you aren't familiar with it, an "effectful" function (in our
case, `Const :: w -> Const w w`) over all values in a container, and sequences
all of their effects.

### Monoid is the Key

All of this actually witnesses the core of Applicative. A lot of people describe
Applicative as a "lax monoidal functor".

In this post, I was really handwavey with how I talked about "effects"
("`f <*> x` must use the effects of `f` and `x` each once and only once", I
claimed, without defining what an effect was). The notion of what an "effect" is
really comes from each individual Applicative, and each type really has its own
conceptual picture of what counts as an effect. The rigorous test of what is a
meaningful way to have an effect that can be combined comes from those laws
(`pure f <*> x = fmap f x`, etc.) and the overall sentiment that the combination
of effects is *monoidal*.

At its heart, Applicative enforces that `liftA2` and `<*>` are supposed to be
"monoidal" in some way. This fact is hidden by the normal form of the
Applicative laws, but I feel like seeing this play out in the `Applicative`
instance for `Const` --- how `Monoid` is exactly the constraint necessary to
implement the instance, and how `Const` forms a monoid isomorphism --- really
helps hammer in the monoidal nature of *all* Applicative instances.

Applicative instances must be monoidal in how they sequence their effects.
Because `Const`'s effects are so simple ("accumulate a value"), this makes it an
especially obvious demonstration of this.

Hopefully this helps you gain some sense of appreciation between the link
between `Applicative` and `Monoid`, and also why `Const`'s Applicative instance
is defined the way it is!

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

[^1]: Note that if you want to play along in ghci, you should give this a `Show`
    instance by typing `deriving (Show)` after the data declaration

[^2]: There are other laws, but because of parametric polymorphism in Haskell,
    we know they must be true if and only if `fmap id = id`.

[^3]: Note that in the real world we also have to verify that our definition
    combines effects in an *associative* way, but we won't go too deeply into
    this for this article.

[^4]: Note that the Applicative laws are loose enough to allow a different
    definition, with the same `pure`, but with
    `Const x <*> Const y = Const (y <> x`). But, this is just a different
    `Monoid` (`Const (Dual w)`).

