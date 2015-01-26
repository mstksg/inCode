mtl is Not a Transformer Library
================================

Categories
:   Haskell
:   Ramblings
Tags
:   functional programming
:   haskell
CreateTime
:   2014/10/19 03:50:57
PostDate
:   Never
Identifier
:   mtl

*mtl* is not a transformer library --- contrary to popular conception.  I
believe that this commonly spread myth is due in part to some rather peculiar
branding choices (the name of the library) and in part to some historical
accidents (*mtl* was, in the distant and pre-historic past, indeed a monad
transformer library).

What is *mtl*?  It is a library of *interfaces* you can provide to your own
types, in the form of typeclasses.  It abstracts over *different design
patterns* for different types, in the form of typeclasses.  Just like Functor
abstracts over "things that can be `fmap`ped".  *mtl* provides typeclasses
abstracting over many useful patterns that many types satisfy.

The Patterns
------------

### MonadIO

The simplest pattern abstracted over by *mtl* is probably the *MonadIO*
pattern.

There are several types you can make that can construct that can "represent"
or "encode" an IO action: (something of an `IO` type)

~~~haskell
newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

instance Functor MaybeIO where
    fmap f (MaybeIO mx) = MaybeIO (fmap f mx)

instance Applicative MaybeIO where
    pure x = return (Just x)
    MaybeIO mf <*> MaybeIO mx = MaybeIO $ liftA2 (<*>) f x

instance Monad MaybeIO where
    return = pure
    mx >>= f = MaybeIO $ do
                   x <- runMaybeIO mx
                   case x of
                       Just y  -> f y
                       Nothing -> return Nothing
~~~

`MaybeIO` is an IO action, describing an action a computer/CPU can take to
possibly produce an `a`, or fail in the process (indicated by `Just` and
`Nothing`).

We encoded in the `Monad` instance that this type has "short circuit" binding;
if at any point along a chained sequence of computations, if any of the
chained values fails, the entire thing fails.  The rest of the sequence is
skipped.

It is clear that we can "encode" any value `a` into this type, by producing a
`MaybeIO` that describes a no-op that always succeeds with that value.  We
call that `pure`, or `return`.

This would be pretty useless if we can't use any of our existing `IO a`
actions from other libraries.  Luckily, it is clear that we can *use* any `IO
a` action *as if it were* a `MaybeIO`.  In other words, we can use `MaybeIO`
to represent any `IO a` that you might want.  A `MaybeIO` --- an IO action
that represents a computer/IO computation that might "fail" --- can also be
used to represent an `IO` --- an IO action that represents a computer/IO
computation that does not "fail"[^nofail].

[^nofail]: That is, "fail" in the same way that `MaybeIO` "fails".  You know
what I mean :)

~~~haskell
representIO :: IO a -> MaybeIO a
representIO x = MaybeIO (fmap Just x)
~~~

There are a lot of types that actually fit this pattern.  In fact, many DSL's
and things that manage IO and cusom monads for interpreted environments can do
this.  Being able to represent arbitrary IO actions in your type opens up a
lot of doors.[^doors]

[^doors]: Some good, some bad :)

This design pattern is encapsulated in *mtl* as a typeclass, `MonadIO`:

~~~haskell
instance MonadIO MaybeIO where
    liftIO x = MaybeIO (fmap Just x)
~~~

<div class="note">
**Aside**

In order for `liftIO` to behave meaningfully, it has to follow some laws:

1.  Lifting the no-op will also be a no-op.

    ~~~haskell
    liftIO (return x) = return x
    ~~~

    That is, lifting the no-op will also be a no-op.

2.  That is, `liftIO` "distributes" over bind.  `liftIO`-ing a bunch of
    chained `IO` actions is the same as `liftIO`-ing them each individually
    and chaining those.

    ~~~haskell
    liftIO $ do x <- m
                f x
    ~~~

    is equal to

    ~~~haskell
    do x <- liftIO m
       liftIO (f x)
    ~~~


Basically, we say that `liftIO` is a [monad morphism][mmorph].

[mmorph]: http://hackage.haskell.org/package/mmorph-1.0.4/docs/Control-Monad-Morph.html















