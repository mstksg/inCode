mtl is Not a Monad Transformer Library

=======================================

> Originally posted by [Justin Le](https://blog.jle.im/) on May 18, 2015.
> [Read online!](https://blog.jle.im/entry/mtl-is-not-a-monad-transformer-library.html)

*mtl* is not a monad transformer library --- contrary to popular conception. I
believe that this commonly spread myth is due in part to some rather peculiar
branding choices (the name of the library) and in part to some historical
accidents (*mtl* was, in the distant and pre-historic past, indeed a monad
transformer library).

What is *mtl*? It is a library of *interfaces* you can provide to your own
types, in the form of typeclasses. It abstracts over *different design patterns*
for different types, in the form of typeclasses. Just like Functor abstracts
over "things that can be fmapped". *mtl* provides typeclasses abstracting over
many useful patterns that many types satisfy --- patterns involving different
sorts of "effects".

## The Patterns

### MonadError

`MonadError` is a generic interface over things where you can throw "errors" of
a specific type `e`, and "catch" them. It offers two methods:
`throwError :: e -> m a`, and `catchError :: m a -> (e -> m a) -> m a`, which
does what you'd expect from an error monad.

Now, we have a generic interface to work on *all specific type error-throwing
Monads*. The `Either` type comes to mind as an obvious candidate:

``` haskell
instance MonadError e (Either e) where
    throwError = Left
    catchError s f = case s of
                       Right _ -> s
                       Left e  -> f e
```

But there are definitely other instances possible. How about for `IO` and
`IOException`s, in specific?

``` haskell
instance MonadError IOException IO where
    throwError  = ioError
    catchErrror = catch     -- will not catch non-IOExceptions
```

This is great, because we can now write code *generic* over *all* specific-type
error things!

#### Error behavior...for free!

If we're clever enough, we can actually imbue any arbitrary Monad `m` with
rudimentary, basic, "dumb" error handling by using the `ExceptT` type. An
`ExceptT e m` behaves *just* like our original Monad `m` in every way...except
now, we have access to rudimentary implementations of side-channels of
`throwError` and `catchError`.

This is pretty useful...to be able to add short-circuiting error behavior to any
Monad we wanted. But remember, `ExceptT` is not the "point" of `MonadError`.
It's just one way to generate instances for free given a Monad. The real power
of `MonadError` is in the ability to write generically over many Monads with
some sort of "error" behavior, like `Either` or `IO`.

### MonadState

A `MonadState s m` is a Monad `m` where, during in the context of `m`, you have
access to a global state of type `s` that you can modify.

You can "get" it with `get :: m s`. You can modify it with
`modify :: (s -> s) -> m ()`. You can replace it with `put :: s -> m ()`.

There are a lot of types that can offer this type of interface. You might have,
for example, a type where "getting" the state comes from reading an `IORef`, and
"putting" it comes from writing to the `IORef`. Or maybe the state can come from
a a query to a database...where `get` queries a database in IO, and `put` writes
to the database.

`MonadState`, as a typeclass, gives you the ability to *write generically over
all Monads with state*. You can now write generically over those database state
things...or those IORef state things...or those web query things...or anything
that cares to implement the interface!

`MonadState` says, "the functions and actions I write can work for *all* Monads
offering state I can modify!" An action of type
`MonadState String m => m Double` can create a `Double` from *any* monad
offering some sort of `String` state.

#### State...for free!

Again, we can actually imbue any Monad `m` with some very rudimentary, "dumb"
stateful interface, using a type called `StateT`. A `StateT s m` behaves just
like our monad `m` (be it `IO`, `Reader`, `ST`, `STM`...), except now we have
access to a rudimentary state getting-and-putting mechanism on a state of type
`s`, using a form of function composition. The implementation of the `StateT`
handles it under the hood.

Obviously, being able to add a rudimentary stateful interface on top of any
Monad is pretty useful. Very useful, in fact!

But remember, this isn't the *point* of `MonadState`. `MonadState` doesn't exist
for `StateT`. `StateT` is just a way to generate a free instance of `MonadState`
if you just want to add rudimentary statefulness to an existing Monad. But there
are many instances of `MonadState`...really, `MonadState` has nothing to do with
`StateT` fundamentally, any more than `Monad` has to do with `Maybe`
fundamentally. And `MonadState` and `StateT` don't even come from the same
library!

*mtl* offers a generic interface for working with all monads offering a statey
API.

### MonadReader

`MonadReader` is more or less the same thing...it offers a generic interface to
work on monads that have access to some sort of global, unchanging
"environment". An example might be a Monad where you could work with command
line arguments, or environment variables, assuming they are read once and fixed
when things start up. You could access the command line arguments with `ask`,
and use them in your program.

### MonadIO

This one is actually from *transformers*, but it gives a nice picture. Any
`MonadIO m` is a `Monad` that allows you to embed and sequence in any arbitrary
IO action. This is pretty useful! In the
*[persistent](http://hackage.haskell.org/package/persistent)* database library,
for example --- the main "database access type monad" can sequence actions that
access databases *and* arbitrary IO actions, as well. A lot of resource managers
and DSL's offer the ability to sequence IO in the middle of all the other
actions.

That's what `MonadIO` is for --- it allows you to write functions and say, "hey,
my function is generic over *all* things that can embed IO...anything that can
embed IO can sequence my function/type". The generic "embedding" action is
`liftIO :: MonadIO m => IO a -> m a`.

::: note
**Aside**

You know...ideally, all of these typeclasses would have laws, so we could make
conclusions and apply equational reasoning to generically written functions.

Some of the laws are simple...`liftIO` from `MonadIO` should be a [monad
morphism](http://hackage.haskell.org/package/mmorph-1.0.4/docs/Control-Monad-Morph.html).
But the rest of them don't really have any well-established laws. This is a bit
of a shame, because we'd really like to be able to apply reasoning to generic
functions.

People have suggested `MonadState` have laws similar to how view/set/over
interact in the *lens* laws. But as of now, most of we have in terms of our
capability of analyzing generic programs is rough heuristins/feelings about what
"should" be right.

A bit un-ideal, but...in practice, this ends up working not-so-badly :)
:::

## Not a Monad Transformer Library

So, let's work together to dispel the myth that *mtl* is a monad transformer
library. It really has nothing to do with monad transformers at all...any more
than `Control.Monad` is an "IO module", or `Control.Monoid` is a "list module".
Transformers don't even come from the *mtl* library!

Together, we can overcome this myth. We can show people that we can live in a
world where we can combine effects, work generically in Monads with *multiple
types of effects* by writing functions generic over many different *mtl*
typeclasses at once! (`MonadState` + `MonadIO`, maybe?)

We don't *have to* reach for Monad transformers to work with combined effects.
We can write our own combined effects monads and just write the instances...or
we can write generically and not even care about what Monad we actually use in
the end. We don't have to teach people to be afraid of monad transformers as if
they were the only way to get things done, and *mtl* is tied to them like a ball
and chain.

*mtl* is not a Monad transformer library. How liberating!

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

