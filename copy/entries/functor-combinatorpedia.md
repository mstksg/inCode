---
title: The Functor Combinatorpedia
categories: Haskell
tags: functional programming, free, haskell, interpreters
create-time: 2019/06/17 21:43:36
identifier: functor-combinatorpedia
slug: functor-combinatorpedia
---

Recently I've been very productive what I have been calling the
"Functor Combinator" design pattern.  It is heavily influenced by ideas like
[Data types a la Carte][dtalc] and [unified free monoidal functors][ufmf], but
the end goal is slightly different in spirit.

[dtalc]: http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf
[ufmf]:  http://oleg.fi/gists/posts/2018-02-21-single-free.html

The goal is to represent schemas, DSL's, and computations (things like parsers,
things to execute, things to consume or produce data) by assembling
"self-evident" basic primitives and subjecting them to many *different*
successive transformations and combiners.  The process of doing so:

1.  Forces you to make explicit decisions about the structure of your
    computation type as an ADT.
2.  Allows you to retain isolation of fundamental parts of your domain as
    separate types
3.  Lets you manipulate the structure of your final computation type through
    *normal Haskell techniques* like pattern matching.  The structure is
    available throughout the entire process, so you can replace individual
    components and values within your structure.
4.  Allows you to fully *reflect* the structure of your final computation
    through pattern matching and folds, so you can inspect the structure and
    produce useful summaries.

Like "data types a la carte" and free monad/applicative/alternative designs,
these techniques allow you to separate the assembly and inspection of your
programs from the "running" of them.  However, the main difference is that here
we focus not just on products and sums, but many different varied and
multi-purpose combinators --- a bona fide "zoo" of combinators.  So, this
focuses less on "fixed-points" like `Free`, `Ap`, `Alt`, and more on
non-recursive simple building blocks...where recursive combinators like `Free`
might be used along the way.  The *functor itself* is the goal, *not* its fixed
point.

This post also serves as an overview of the *[functor-combinators][]* library,
which mostly pulls together all of the functor combinators scattered across the
Haskell ecosystem and provides a unified interface for working with them, which
will help us talk about all of them with the same vocabulary.

[functor-combinators]: https://hackage.haskell.org/package/functor-combinators

Prologue: What is a functor combinator?
---------------------------------------

A functor combinator takes "functors" (or other indexed types) and returns a new
functor, enhances or mixes them together in some way.

That is, they take things of kind `k -> Type` and themselves return a `k ->
Type`.

This lets us build complex functors out of simpler "primitive" ones.

For example, `ReaderT r` is a famous one that takes a functor `f` and enhances
it with "access to an `r` environment" functionality.

Another famous one is `Free`, which takes a functor `f` and enhances it with
"sequential binding" capabilities: it turns `f` into a `Monad`.

Sometimes, we have binary functor combinators, like `:+:`, which takes two
functors `f` and `g` and returns a functor that is "either" `f` or `g`.  Binary
functor combinators "mix together" the functionality of different functors in
different ways.

### Examples

If your final DSL/program/schema is some functor, then functor combinators
allow you to construct your final functor by combining simpler "primitive"
functors, and take advantage of common functionality.

For example, if you were making a data type/EDSL to describe a command line
parser, you might have two primitives: `data Arg a`, for positional arguments
parsing `a`, and `data Option a`, for `--flag` non-positional options parsing
`a`.  From there, you can *choose* what structure of command line arguments you
want to be able to express.

For instance, a structure that can support multiple arguments and optionally a
single `Option` would be:

```haskell
type CommandArgs = Ap Arg :*: Lift Option
```

And a structure that supports *multiple commands* on top of that would be:

```haskell
type CommandArgs = MapF String (Ap Arg :*: Lift Option)
```

You can mix or match combinators to decide exactly what sort of structures you
allow in your DSL.

### Common Functionality

Most of these functor combinators allow us to "swap out" the underlying
functor, retaining all of the "enhanced" structure.  We abstract over all of
these using `hmap` for single-argument functor combinators ("enhancers") and
`hbimap` for two-argument functor combinators ("mixers").

```haskell
class HFunctor t where
    -- | Swap out underlying functor for a single-argument functor combinator
    hmap
        :: t f a -> t g a

class HBifunctor t where
    -- | Swap out underlying functors for a two-argument functor combinator
    hbimap
        :: (forall x. f x -> h x)
        -> (forall x. g x -> j x)
        -> t f g a
        -> t g j a
```

However, for this post, the concept of a "natural transformation" between `f`
and `g` --- a function of type `forall x. f x -> g x`, is given a type synonym:

```haskell
type f ~> g = forall x. f x -> g x
```

Then the type signatures of `hmap` and `hbimap` become:


```haskell
class HFunctor t where
    hmap
        :: t f ~> t g

class HBifunctor t where
    hbimap
        :: f ~> h
        -> g ~> j
        -> t f g ~> t h j
```

What does it mean exactly when we say that `hmap` and `hbimap` "preserve the
enhanced structure"?  Well, for example, `ListF f a` is essentially a list of
`f a`s.  `hmap` will swap out and replace each `f a`, but it must *preserve the
relative order* between each of the original `f a`s.  And it must preserve the
length of the list.  It's a complete "in-place swap".  This is formalizing by
requiring `hmap id == id` and `hbimap id id == id`.

You can also always "lift" a functor value into its transformed type.  We
abstract over this by using `inject` (for single-argument functors) and `inL`
and `inR` (for two-argument functors):

```haskell
class Inject t where
    -- | Lift `f` into `t f`
    inject :: f ~> t f

inL :: (Monoidal t, CM t g)     -- more on the `CM t` later
    => f ~> t f g

inR :: (Monoidal t, CM t f)     -- more on the `CM t` later
    => g ~> t f g
```

Finally, in order to *use* any functor combinators, you have to *interpret*
them into some target context.  The choice of combinator imposes some
constraints on the target context.  We abstract over this using `interpret` and
`binterpret`:

```haskell
class Interpret t where
    type C t :: (Type -> Type) -> Constraint

    -- | Interpret unary functor combinator
    interpret
        :: C t g
        => f ~> g             -- ^ interpreting function
        -> t f ~> g

instance Semigroupoidal t where
    -- | Interpret binary functor combinator
    binterpret
        :: (Semigroupoidal t, CS t h)
        => f ~> h             -- ^ interpreting function on f
        => g ~> h             -- ^ interpreting function on g
        -> t f g ~> h
```

Each functor combinator defines a constraint (`C` for unary functor
combinators, and `CS` and `CM` for binary functor combinators) that allows you
to "exit", or "run" the functor combinator.

For some concrete examples:

```haskell
type C (ReaderT r) = MonadReader r

interpret @(MonadReader r)
    :: MonadReader r g
    => (f ~> g)
    -> ReaderT r f a
    -> g a

type C Free = Monad

interpret @Free
    :: Monad g
    => (f ~> g)
    -> Free f a
    -> g a

type CM (:+:) = Unconstrained   -- no constraints on exiting

binterpret @(:+:)
    :: (f ~> h)
    -> (g ~> h)
    -> (f :+: g) a
    -> h a
```

We see that `interpret` lets you "run" a `ReaderT r f` into any `MonadReader r
g` and "run" a `Free` in any monad `g`, and `binterpret` lets you "run" a
function over both branches of an `f :+: g` to produce an `h`.

From these, we can build a lot of useful utility functions (like `retract`,
`biretract`, `getI`, `biget`, etc.) for convenience in actually working on
them.  These are provided in *[functor-combinators][]*.

Without further ado, let's dive into the zoo of functor combinators!
