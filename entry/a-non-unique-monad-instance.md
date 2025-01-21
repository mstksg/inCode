A Non-Unique Monad Instance

============================

> Originally posted by [Justin Le](https://blog.jle.im/) on January 28, 2015.
> [Read online!](https://blog.jle.im/entry/a-non-unique-monad-instance.html)

Just stopping in for a short post before continuing with a long-overdue series
or two :) This post is a bit of a short fun one that describes a quest I had,
and hopefully some useful extra ideas I found along the way.

Soon after I discovered Haskell, one question has plagued my mind. Day and
night, I wondered...

> Are there any Haskell types with more than one unique `Monad` instance?

This was a question that was pretty simple...so simple that I was sure many
people had already asked and answered this. But I couldn't really find any
answers and nobody I asked at the time could really give me one either, so this
soon embedded itself as a pretty deep mystery to my psyche.

The background?

## Functor and Applicative

All `Functor` instances, if they exist, are *unique* for the type. The type
uniquely determines the instance. There is only one possible `Functor` instance
for `[]`, one possible `Functor` instance for `Maybe`, `Either`, etc.

This fact is taken advantage of by GHC to allow you to derive, for some types, a
`Functor` instance automatically.

``` haskell
ghci> :set -XDeriveFunctor
ghci> data Foo a = Bar [a] (Maybe (Foo a)) | Baz (Either String a) (Foo a)
ghci> let x = Bar [1, 3] (Just (Baz (Right 4) (Bar [10] Nothing)))
ghci> fmap (*2) x
Bar [2, 6] (Just (Baz (Right 8) (Bar [20] Nothing)))
```

There is no other possible `Functor` instance for that data type. Go ahead, try
:D

``` haskell
data Foo a = Bar [a] (Maybe (Foo a)) | Baz (Either String a) (Foo a)

instance Functor Foo where
    fmap f (Bar xs y) = Bar (fmap f xs) (fmap f y)
    fmap f (Baz x fy) = Baz (fmap f x) (fmap f fy)
```

However, this is not the case for `Applicative`. Everyone knows of course about
the normal (cartesian product) Applicative instance and the zippy Applicative
instance for list:

``` haskell
instance Applicative [] where
    pure x    = [x]
    fs <*> xs = [ f x | f <- fs, x <- xs ]

instance Applicative [] where
    pure      = repeat
    fs <*> xs = zipWith ($) fs xs
```

What is also fairly established is that every *noncommutative* `Applicative`
instance also has a "flipped" version:

``` haskell
-- a flipped IO Applicative
data FlipIO a = FlipIO { runFlipIO :: IO a }

instance Applicative FlipIO where
    pure x    = FlipIO (pure x)
    fi <*> xi = FlipIO $ do
                  x <- runFlipIO xi
                  f <- runFlipIO fi     -- note the backwards effects
                  return (f x)
```

``` haskell
data State s a = State { runState :: s -> (a, s) }

-- the normal instance
instance Applicative (State s) where
    pure x    = State $ \s0 -> (x, s0)
    fs <*> xs = State $ \s0 -> let (f, s1) = runState fs s0
                                   (x, s2) = runState xs s1
                               in  (f x, s2)

-- the flipped instance
instance Applicative (State s) where
    pure x    = State $ \s0 -> (x, s0)
    fs <*> xs = State $ \s0 -> let (x, s1) = runState xs s0
                                   (f, s2) = runState fs s1
                               in  (f x, s2)
```

``` haskell
ghci> liftA2 (,) getLine getLine
> hello         -- asking for the first field
> world         -- asking for the second field
("hello", "world")
ghci> runFlipIO $ liftA2 (,) (FlipIO getLine) (FlipIO getLine)
> hello         -- asking for the second field
> world         -- asking for the first field
("world", "hello")
```

Every non-commutative `Applicative` admits an alternative instance where
"flipping" the order of the "effects" is also a valid `Applicative` instance.
So, not `Maybe` or `Either`, but `State`, `[]`, and `IO`.

``` haskell
-- free "flipped" Applicative instance
data Flipped f a = Flipped { runFlipped :: f a }

-- instance where (<*>) is the same, but the order of effects is switched
instance Applicative f => Applicative (Flipped f) where
    pure = Flipped . pure
    Flipped f <*> Flipped x = Flipped $ liftA2 (flip ($)) x f
```

Cool. Types that have `Functor` instances only have one. Types that have
`Applicative` instances very often have more than one.

So, the obvious next question is...what about `Monad`s? Is a `Monad` instance
uniquely determined by its type?

## Monad

The answer wasn't that simple, for me. Yes, *most* `Applicative`s in the wild
are non-unique, and there was a generating rule. But not so for `Monad`s. You
can't have a `Monad` where the effects are switched, because for `(>>=)`, you
need the effects of the first action in order to even decide what the effects of
the next action are.

I vaguely remember from my past two data types that are very similar yet have
very different `Monad` and `Applicative` instances: (finite) lists, (infinite)
[streams](http://blog.jle.im/entry/intro-to-machines-arrows-part-1-stream-and).
From the outset, the two have almost identical structure. A `Stream` is just a
list with no `[]`/nil:

``` haskell
data Stream a = a :~ Stream a
```

The `Functor` instance is identical:

``` haskell
instance Functor Stream where
    fmap f (x :~ xs) = f x :~ fmap f xs
```

And the (only??) `Applicative` instance is the `ZipList` instance for lists:

``` haskell
instance Applicative Stream where
    pure x = x :~ pure x
    (f :~ fs) <*> (x :~ xs) = f x :~ (fs <*> xs)
```

The `Monad` instance is however very different from that of lists:

``` haskell
instance Monad Stream where
    return x = x :~ return x
    xs >>= f = join' (fmap f xs)
      where
        join' :: Stream (Stream a) -> Stream a
        join' ((x :~ _) :~ yss) = x :~ join' (fmap tail' yss)
        tail' (_ :~ xs) = xs
```

The `Monad` instance itself is actually interesting enough to write about. It
all revolves around `join`, where `join` takes a stream of streams and creates a
stream *of the diagonals*. So it takes the first element of the first stream,
the second element of the second stream, the third element of the third stream,
etc.

This is actually a special case of the `Monad` instance for all fixed-sized
ordered containers. A length 5 vector, for example, will have the same
`Applicative` and `Monad` instance as described here: `(<*>)` with "zipping",
and `join` with grabbing the diagonal of the 5-vector of 5-vectors.

This was a promising lead, but, it doesn't take *too* much thought to see that
neither lists nor `Stream` are appropriate for *both* instances.

::: note
**Aside**

In case you were wondering, here is an elaboration :D

-   Fixed length vectors can't have the normal list Applicative instance at all,
    unless they are of size 0 or 1. That's because the result after `(<*>)`, the
    resulting list's length is the product of the original lists. So you can
    forget the `Monad` instance, too.

-   Streams give you no luck, either. The easiest way to see is by considering
    the analogous `Monad` instance, where `join` is the straight-up
    concatenation. `m >>= return == m` is clearly violated. If `m` is an
    infinite list, `fmap return` gives you an infinite list of infinite lists,
    "joining"/concatenating them back will just give you an infinite list of the
    first item in `m`.

    To put succinctly, for `Stream`, `concat == head`.

-   Lists can have the `Applicative` instance fine, but not the `Monad`
    instance. Here we assume that zipping and "getting the diagonal" go only as
    "far as possible", and stop when one of the lists is too short.

    This one is a little trickier, but the weakness is when you have lists of
    lists of lists of different lengths.

    ``` haskell
    ghci> let counterexample = [[[1]], [[], [2,3]]]
    ghci> join counterexample
    [[1], [2,3]]
    ghci> join. join $ counterexample
    [1,3]
    ghci> fmap join counterexample
    [[1], []]
    ghci> join . fmap join $ counterexample
    [1]
    ```

    For a monad, joining the inner layer and then joining it all should be the
    same as joining it all and joining it all. The order of the joining
    shouldn't count. We can see this in the more haskelly monad laws by noting:

    ``` haskell
    ghci> id <=< (id <=< id) $ counterexample
    [1,2]
    ghci> (id <=< id) <=< id $ counterexample
    [1]
    ```

    So, dead end here.
:::

So I didn't really have any leads at that point; I tried a couple of other paths
but nothing really panned out. So I shelved it for a while.

## Revelation

Several centuries later[^1], the final revelation came as many revelations do in
Haskell --- from a hint by Edward Kmett. He pointed out something interesting
regarding a `Monad` instance that I had yet to notice:

``` haskell
instance Monoid w => Monad ((,) w) where
```

This is the classic "Writer" monad instance, which is literally about as old as
monads in functional programming is.

The key is that the `Monad` instance of `(w,)` depends on the `Monoid` instance
of `w`. This is the "log", so to speak. You need a `Monoid` instance in order to
make the `Monad` instance...and the behavior of the `Monad` instance is directly
determined by the behavior of the `Monoid` instance of `w`.

And...`Monoid` instances in Haskell are rarely ever unique! A different `Monoid`
instance would create a very different `Monad` instance for the same type!

So, by factoring out the dependency on an external `Monoid` instance, you get...

``` haskell
data Two a = One a | Two a

instance Functor Two where
    fmap f (One a) = One (f a)
    fmap f (Two a) = Two (f a)
```

and...voila! There it is!

This type is basically equivalent to `(Bool, a)`. And `Bool` has multiple
`Monoid`s on it. Instead of requiring an outside `Monoid` instance, we can
encode the instance directly into the behavior of `(>>=)`. And here we go!

Our instances are basically the `Writer` instance for `(Bool, a)`, with
different `Monoid` instances for `Bool`.

The first instance:

``` haskell
instance Applicative Two where
    pure = One
    One f <*> One x = One (f x)
    One f <*> Two x = Two (f x)
    Two f <*> One x = Two (f x)
    Two f <*> Two x = Two (f x)

instance Monad Two where
    return = One
    One x >>= f = f x
    Two x >>= f = case f x of
                    One y -> Two y
                    Two y -> Two y
```

Which represents the monoids formed by `(&&)` with `True` or by `(||)` with
`False` (depending on which one you pick as `True` and which one you pick as
`False`; the two instances are isomorphic)

The second:

``` haskell
instance Applicative Two where
    pure = One
    One f <*> One x = One (f x)
    One f <*> Two x = Two (f x)
    Two f <*> One x = Two (f x)
    Two f <*> Two x = One (f x)

instance Monad Two where
    return = One
    One x >>= f = f x
    Two x >>= f = case f x of
                    One y -> Two y
                    Two y -> One y
```

Which represents the monoid formed by `(/=)` (or "XOR") with `False`.

And there you go. One type, two possible unique, non-isomorphic `Monad`
instances.

::: note
**Aside**

One interesting thing to note is that the Monad instance for `(->) a` requires
no monoid constraint, and the Monad instance for `(,) a` *does*.

Interestingly enough, if we look at *comonads*, the Comonad instance for
`(->) a` *does* require a monoid constraint on `a` (so for example there are
many unique Comonad instances for things isomorphic to `(->) a` where `a` has
more than one Monoid instance) and and the Comonad instance for `(,) a` *does
not* require a monoid constraint on `a`.

Is there some duality at play here?

The answer is, apparently, yes! But according to Edward Kmett, it is one that is
pretty hard to arrive at and a big headache and overall not worth the time to
dig into. So you're going to have to take my second-hand word for it.
:::

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

[^1]: More accurately, "about a year"

