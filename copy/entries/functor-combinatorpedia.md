---
title: The Functor Combinatorpedia
categories: Haskell
tags: functional programming, free, haskell, interpreters
create-time: 2019/06/17 21:43:36
date: 2019/06/19 10:41:46
identifier: functor-combinatorpedia
slug: functor-combinatorpedia
---

**functor-combinators**: [hackage][functor-combinators] / [github][]

[functor-combinators]: https://hackage.haskell.org/package/functor-combinators
[github]: https://github.com/mstksg/functor-combinators

Recently I've been very productive what I have been calling the "Functor
Combinator" design pattern.  It is heavily influenced by ideas like [Data types
a la Carte][dtalc] and [unified free monoidal functors][ufmf], but the end goal
is slightly different in spirit.  The goal is to represent schemas, DSL's, and
computations (things like parsers, things to execute, things to consume or
produce data) by assembling "self-evident" basic primitives and subjecting them
to many *different* successive transformations and combiners (through
combinators, free structures, tensors, and other options).  The process of
doing so:


[dtalc]: http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf
[ufmf]:  http://oleg.fi/gists/posts/2018-02-21-single-free.html

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
programs from the "running" of them.[^comparisons]  However, the main
difference is that here we focus not just on products and sums, but many
different varied and multi-purpose combinators --- a "zoo" of combinators.  The
fixed point is *not* the end goal.

[^comparisons]: On the surface, this functor combinator design pattern might
    look like it fills a similar space to effects systems and libraries like
    *[mtl][]*, *[polysemy][]*, *[freer-simple][]*, or *[fused-effects][]*.
    However, this design pattern actually exists on a different level.

    Functor combinator design patterns can be used to help build the
    *structure* of the *data types* and schemas that define your program/DSL.
    Once you build these nice structures, you then *interpret* them into some
    target context. This "target context" is the realm that libraries like
    *mtl* and *polysemy* can fill; functor combinators serve to help you define
    a structure for your program *before* you interpret it into whatever
    Applicative or Monad or effects system you end up using.

[mtl]: https://hackage.haskell.org/package/mtl
[polysemy]: https://hackage.haskell.org/package/polysemy
[freer-simple]: https://hackage.haskell.org/package/freer-simple
[fused-effects]: https://hackage.haskell.org/package/fused-effects

This post is a run-down on the wide variety of such "functor combinators"
across the Haskell ecosystem --- a functor combinatorpedia.  To speak about
them all with the same language and vocabulary, this post also serves as an
overview of the *[functor-combinators][]* library, which doesn't really define
these functor combinators, but rather pulls them all together and provides a
unified interface for working with them.  Most of these types and typeclasses
are exported by *[Data.Functor.Combinator]*.

Right now I already have some posts about this general design pattern,
["Interpreters a la Carte" in Advent of Code 2017 Duet][ialc] and [Applicative
Regular Expressions using the Free Alternatuve][are], but I do have some posts
planned in the future going through projects using this unified interface.  In
a way, this post also serves as the "introduction to free structures" that I
always wanted to write :)

[ialc]: https://blog.jle.im/entry/interpreters-a-la-carte-duet.html
[are]: https://blog.jle.im/entry/free-alternative-regexp.html


Please refer to the [table of contents][toc] if you are using this as a
reference!

[toc]: https://blog.jle.im/entry/functor-combinatorpedia.html#title

Preface: What is a functor combinator?
--------------------------------------

A functor combinator takes "functors" (or any other indexed type, `k -> Type`)
and returns a new functor, enhances or mixes them together in some way.  That
is, they take things of kind `k -> Type` and themselves return a `j -> Type`.
This lets us build complex functors/indexed types out of simpler "primitive"
ones.  This includes many some monad transformers, free structures, and
tensors.

For example, `ReaderT r` is a famous one that takes a functor `f` and enhances
it with "access to an `r` environment" functionality.  Another famous one is
`Free`, which takes a functor `f` and enhances it with "sequential binding"
capabilities: it turns `f` into a `Monad`.

The main thing that distinguishes these functor combinators from things like
monad transformers is that they are "natural on `f`": they work on *all* `f`s,
not just monads, and assume no structure (not even `Functor`).

Sometimes, we have binary functor combinators, like `:+:`, which takes two
functors `f` and `g` and returns a functor that is "either" `f` or `g`.  Binary
functor combinators "mix together" the functionality of different functors in
different ways.

### Examples

If your final DSL/program/schema is some functor, then functor combinators
allow you to construct your final functor by combining simpler "primitive"
functors, and take advantage of common functionality.

For example, if you were making a data type/EDSL to describe a command line
argument parser, you might have two primitives: `data Arg a`, for positional
arguments parsing `a`, and `data Option a`, for `--flag` non-positional options
parsing `a`.  From there, you can *choose* what structure of command line
arguments you want to be able to express.

For instance, a structure that can support multiple arguments and optionally a
single `Option` would be:

```haskell
type CommandArgs = Ap Arg :*: Lift Option
```

And a structure that supports *multiple named commands* on top of that would
be:

```haskell
type CommandArgs = MapF String (Ap Arg :*: Lift Option)
```

You can mix or match combinators to decide exactly what sort of structures you
allow in your DSL.

Now, instead of writing one "giant" `runParser :: MapF String (Ap Arg :*: Lift
Option) a -> IO a` function, you can instead just write parsers for your
simple primitives `Arg a -> IO a` and `Option a -> IO a`, and then use functor
combinator tools to "promote" them to being runnable on a full `MapF String (Ap
Arg :*: Lift Option)` without any extra work.

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
enhanced structure"?  Well, for example, the type `newtype ListF f a = ListF [f
a]` is essentially a list of `f a`s.  `hmap` will swap out and replace each `f
a`, but it must *preserve the relative order* between each of the original `f
a`s.  And it must preserve the length of the list.  It's a complete "in-place
swap".  This is formalizing by requiring `hmap id == id` and `hbimap id id ==
id`.

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

class Semigroupoidal t where
    type CS t :: (Type -> Type) -> Constraint

    -- | Interpret binary functor combinator
    binterpret
        :: CS t h
        => f ~> h             -- ^ interpreting function on f
        => g ~> h             -- ^ interpreting function on g
        -> t f g ~> h
```

Each functor combinator defines a constraint (`C` for unary functor
combinators, and `CS` and `CM` for binary functor combinators) that allows you
to "exit", or "run" the functor combinator.

One nice consequence of this approach is that for many such schemas/functors
you build, there might be many *useful* target functors.  For example, if you
build a command line argument parser schema, you might want to run it in `Const
String` to build up a "help message", or you might want to run it in `Parser`
to parse the actual arguments or run pure tests, or you might want to run it in
`IO` to do interactive parsing.

For some concrete examples of these functor combinators and their constraints:

```haskell
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

We see that `interpret` lets you "run" a `Free` in any monad `g`, and
`binterpret` lets you "run" a function over both *branches* of an `f :+: g` to
produce an `h`.

From these, we can also build a lot of useful utility functions (like
`retract`, `biretract`, `getI`, `biget`, etc.) for convenience in actually
working on them.  These are provided in *[functor-combinators][]*.

Without further ado, let's dive into the zoo of functor combinators!

Two-Argument
------------

Binary functor combinators "mix together" two functors/indexed types in
different ways.

We can finally *interpret* (or "run") these into some target context (like
`Parser`, or `IO`), provided the target satisfies some constraints.

It can be useful to separate the functionality associated with each binary
functor combinator into two typeclasses: `Semigroupoidal` and `Monoidal`.
`Semigroupoidal` deals with the "interpreting" and "collapsing" of the mixed
functors, and `Monoidal` deals with the "introduction" and "creation" of them.
A more detailed run-down is available in the docs for
*[Data.Functor.Combinator][]*.

[Data.Functor.Combinator]: https://hackage.haskell.org/package/functor-combinators/docs/Data-Functor-Combinator.html

From each, we can identify two associated constraints, `CS t` (for
`Semigroupoidal`) and `CM t` (for `Monoidal`):[^cscm]

*   `CS t` is what we will call the constraint on where you can *interpret* or
    *run* values of the enhanced type:

    ```haskell
    binterpret
        :: (Semigroupoidal t, CS t h)
        => (f ~> h)
        => (g ~> h)
        -> (t f g ~> h)
    ```

    For example, for `Comp` (functor composition):

    ```haskell
    data Comp f g a = Comp (f (g a))
    ```

    we have `type CS Comp = Bind` ("`Monad` without pure").

*   `CM t` is the constraint on where you can *create* values of the enhanced
    type (`pureT`, `inL`, `inR`)

    ```haskell
    pureT
        :: (Monoidal t, CM t f)
        => I t ~> f

    inL :: (Monoidal t, CM t g)
        => f ~> t f g

    inR :: (Monoidal t, CM t f)
        => g ~> t f g
    ```

    For example, for `Comp`, `type CS Free = Monad`.

    It should always be a subclass of `CS`.

These constraints all depend on the extra structure that the specific functor
combinator imbues.

[^cscm]: As it turns out, `CS` and `CM` seem to generalize the "has an
    identity" property of many typeclasses --- the example used illustrates
    that `Comp` gives us `type CS Comp = Bind` and `type CM Comp = Monad`,
    which is "monad without pure" and "monad".  We will see other examples
    later.

Most of these (the ones that are "tensors") also have an identity functor, `I
t`, where applying `t f (I t)` leaves `f` unchanged (`t f (I t)` is isomorphic
to `f`) and `t (I t) f` is also just `f`.  This is represented by the
associated type `I t`.  For example, `type I Comp = Identity`, because `Comp f
Identity` (composing any functor with `Identity`, `f (Identity a)`) is just the
same as `f a` (the original functor); also, `Comp Identity f` (or `Identity (f
a)`) is the same as `f a`.

One interesting property of these is that for tensors, if we have a binary
functor combinator `*`, we can represent a type `f | f * f | f * f * f | f * f
* f * f | ...` ("repeatedly apply to something multiple times"), which
essentially forms a linked list along that functor combinator.  We call this
the "induced monoidal functor combinator", given by `MF t`.  We can also make a
"non-empty variant", `SF t`, which contains "at least one `f`".

For example, the type that is either `a`, `f a`, `f (f a)`, `f (f (f a))`, etc.
is `Free f`, so that `type MF Comp = Free`.  The type that is either `f a`, `f
(f a)`, `f (f (f a))`, etc. (at least one layer of `f`) is `Free1`, so `type SF
Comp = Free1`.

*functor-combinators* provides functions like `toMF :: t f f ~> MF f` to
abstract over "converting" back and forth between `t f f a` and the induced
monoidal functor combinator `MF t f a` (for example, between `Comp f f a` and
`Free f a`).[^mflink]

[^mflink]: In fact, the link between the binary functor combinator and its induced
    monoidal/semigroupoidal functor combinators is very deep.  In
    *functor-combinators*, it's actually used in the definition of `CS` and `CM`:

    ```haskell
    type CS t = C (SF t)
    type CM t = C (MF t)
    ```

### :+: / Sum

*   **Origin**: *[GHC.Generics][]* (for `:+:`) / *[Data.Functor.Sum][]* (for
    `Sum`)

*   **Mixing Strategy**: "Either-or": provide either case, and user has to
    handle both possibilities.  Basically higher-order `Either`.

    ```haskell
    data (f :+: g) a
        = L1 (f a)
        | R1 (g a)

    data Sum f g a
        = InL (f a)
        | InR (g a)
    ```

    It can be useful for situations where you can validly use one or the other
    in your schema or functor.  For example, if you are describing an HTTP request,
    we could have `data GET a` describing a GET request and `data POST a`
    describing a POST request; `(GET :+: POST) a` would be a functor that
    describes either a GET or POST request.

    The person who creates the `f :+: g` decides which one to give, and the
    person who consumes/interprets/runs the `f :+: g` must provide a way of
    handling *both*

    ```haskell
    binterpret @(:+:)
        :: (f ~> h)
        -> (g ~> h)
        -> (f :+: g) a
        -> h a
    ```

    `binterpret` becomes analogous to `either` from *[Data.Either][]*

*   **Identity**

    ```haskell
    type I (:+:) = Void1

    -- | Data type with no inhabitants
    data Void1 a
    ```

    `f :+: Void1` is equivalent to just `f`, because you can never have a value
    of the right branch.

*   **Constraints**

    ```haskell
    type CS (:+:) = Unconstrained
    type CM (:+:) = Unconstrained

    binterpret @(:+:)
        :: f ~> h
        -> g ~> h
        -> (f :+: g) ~> h

    inL   @(:+:) :: f     ~> f :+: g
    inR   @(:+:) :: g     ~> f :+: g
    pureT @(:+:) :: Void1 ~> h
    ```

    You don't need any constraint in order to use `binterpret`, `inL`, `inR`,
    etc.

    However, note that `pureT` is effectively impossible to call, because no
    values of type `Void1 a` exist.

*   **Induced Monoid**

    ```haskell
    type SF (:+:) = Step
    type MF (:+:) = Step
    ```

    `Step` is the result of an infinite application of `:+:` to the same value:

    ```haskell
    type Step f = f :+: f :+: f :+: f :+: f :+: f :+: ... etc.
    ```

    The correspondence is:

    ```haskell
    L1 x           <=> Step 0 x
    R1 (L1 y)      <=> Step 1 y
    R1 (R1 (L1 z)) <=> Step 2 z
    -- etc.
    ```

    It's not a particularly useful type, but it can be useful if you want to
    provide an `f a` alongside "which position" it is on the infinite list.

[Data.Either]: https://hackage.haskell.org/package/base/docs/Data-Either.html
[Data.Functor.Sum]: https://hackage.haskell.org/package/base/docs/Data-Functor-Sum.html
[GHC.Generics]: https://hackage.haskell.org/package/base/docs/GHC-Generics.html

### :\*: / Product

*   **Origin**: *[GHC.Generics][]* (for `:*:`) / *[Data.Functor.Product][]* (for
    `Product`)

*   **Mixing Strategy**: "Both, separately": provide values from *both*
    functors, and the user can choose which one they want to use.  Basically a
    higher-order tuple.

    ```haskell
    data (f :*: g) a = f a :*: g a

    data Product f g a = Pair (f a) (g a)
    ```

    It can be useful for situations where your schema/functor must be
    *specified* using *both* functors, but the *interpreter* can choose to use
    only one or the other (or both).

    ```haskell
    prodOutL :: (f :*: g) ~> f
    prodOutL (x :*: _) = x

    prodOutR :: (f :*: g) ~> g
    prodOutR (_ :*: y) = y
    ```

*   **Identity**

    ```haskell
    type I (:*:) = Proxy

    -- | Data type with only a single constructor and no information
    data Proxy a = Proxy
    ```

    `f :+: Proxy` is equivalent to just `f`, because the left hand side doesn't
    add anything extra to the pair.

*   **Constraints**

    ```haskell
    type CS (:*:) = Alt
    type CM (:*:) = Plus

    binterpret @(:*:)
        :: Alt h
        => f ~> h
        -> g ~> h
        -> (f :*: g) ~> h

    inL   @(:*:) :: Plus g => f     ~> f :*: g
    inR   @(:*:) :: Plus f => g     ~> f :*: g
    pureT @(:*:) :: Plus h => Proxy ~> h
    ```

    `Alt`, from *[Data.Functor.Alt][]* in *semigroupoids*, can be thought of a
    "higher-kinded semigroup": it's like `Alternative`, but with no
    `Applicative` constraint and no identity:

    ```haskell
    class Alt f where
        (<!>) :: f a -> f a -> f a
    ```

    It is used to combine the results in both branches of the `:*:`.

    To introduce an "empty" branch, we need `Plus` (in
    *[Data.Functor.Plus][]*), which is like a higher-kinded `Monoid`, or
    `Alternative` with no `Applicative`:

    ```haskell
    class Alt f => Plus f where
        zero :: f a
    ```

*   **Induced Monoid**

    ```haskell
    type SF (:*:) = NonEmptyF
    type MF (:*:) = ListF
    ```

    `ListF f a` is a "list of `f a`s".  It represents the posibility of having
    `Proxy` (zero items), `x :: f a` (one item), `x :*: y` (two items), `x :*:
    y :*: z` (three items), etc.

    It's basically an ordered collection of `f a`s `:*:`d with each other.

    ```haskell
    Proxy         <=> ListF []
    x             <=> ListF [x]
    x :*: y       <=> ListF [x,y]
    x :*: y :*: z <=> ListF [x,y,z]
    -- etc.
    ```

    It is useful if you want to define a schema where you can offer *multiple*
    options for the `f a`, and the interpreter/consumer can freely pick any one
    that they want to use.

    `NonEmptyF` is the version of `ListF` that has "at least one `f a`".

    See the information later on `ListF` alone (in the single-argument functor
    combinator section) for more information on usage and utility.

[GHC.Generics]: https://hackage.haskell.org/package/base/docs/GHC-Generics.html
[Data.Functor.Product]: https://hackage.haskell.org/package/base/docs/Data-Functor-Product.html
[Data.Functor.Alt]: https://hackage.haskell.org/package/semigroupoids/docs/Data-Functor-Alt.html
[Data.Functor.Plus]: https://hackage.haskell.org/package/semigroupoids/docs/Data-Functor-Plus.html

### Day

*   **Origin**: *[Data.Functor.Day][]*

*   **Mixing Strategy**: "Both, together forever": provide values from *both*
    functors, and the user *must* also *use* both.

    It can be useful for situations where your schema/functor must be
    *specified* using *both* functors, and the user must also *use* both.

    ```haskell
    binterpret @Day
        :: Apply h          -- superclass of Applicative
        => (f ~> h)
        -> (g ~> h)
        -> Day f g ~> h
    ```

    Unlike for `:*:`, you always have to interpret *both* functor values in
    order to interpret a `Day`.  It's a "full mixing".

*   **Identity**

    ```haskell
    type I Day = Identity
    ```

    `Day f Identity` is equivalent to just `f`, because `Identity` adds no
    extra effects or structure.

*   **Constraints**

    ```haskell
    type CS Day = Apply
    type CM Day = Applicative

    binterpret @Day
        :: Apply h
        => f ~> h
        -> g ~> h
        -> Day f g ~> h

    inL   @Day :: Applicative g => f        ~> Day f g
    inR   @Day :: Applicative f => g        ~> Day f g
    pureT @Day :: Applicative h => Identity ~> h
    ```

    `Apply`, from *[Data.Functor.Apply][]* in *semigroupoids*, is
    "`Applicative` without `pure`"; it only has `<*>` (called `<.>`).

    `pureT` is essentially `pure :: Applicative h => a -> h a`.

*   **Induced Monoid**

    ```haskell
    type SF Day = Ap1
    type MF Day = Ap
    ```

    `Ap f a` is a bunch of `f x`s `Day`d with each other.  It is either:

    *   `a` (zero `f`s)
    *   `f a` (one `f`)
    *   `Day f f a` (two `f`s)
    *   `Day f (Day f f) a` (three `f`s)
    *   .. etc.

    Like `ListF` this is very useful if you want your schema to provide a "bag"
    of `f a`s and your interpreter *must use all of them*.

    For example, if we have a schema for a command line argument parser, each
    `f` may represent a command line option.  To interpret it, we must look at
    *all* command line options.

    `Ap1` is a version with "at least one" `f a`.

    See the information later on `Ap` alone (in the single-argument functor
    combinator section) for more information on usage and utility.

[Data.Functor.Day]: https://hackage.haskell.org/package/kan-extensions/docs/Data-Functor-Day.html
[Data.Functor.Apply]: https://hackage.haskell.org/package/semigroupoids/docs/Data-Functor-Apply.html

### Comp

*   **Origin**: *[Control.Monad.Freer.Church][]*.  Note that an equivalent type
    is also found in *[GHC.Generics][]* and *[Data.Functor.Compose][]*, but
    they are incompatible with the `HBifunctor` typeclass because they require
    the second input to have a `Functor` instance.

*   **Mixing Strategy**: "Both, together, sequentially" : provide values from
    *both* functors; the user must *use* both, and *in order*.

    ```haskell
    data Comp f g a = Comp (f (g a))
    ```

    It can be useful for situations where your schema/functor must be specified
    using both functors, and the user must *use* both, but also enforcing that
    they must use both in the *given order*: that is, for a `Comp f g`, they
    interpret `f` *before* they interpret `g`.

    ```haskell
    binterpret @Day
        :: Bind h          -- superclass of Monad
        => (f ~> h)
        -> (g ~> h)
        -> Comp f g ~> h
    ```

    Unlike for `:*:`, you always have to interpret *both* functor values.  And,
    unlike for `Day`, you must interpret both functor values *in that order*.

*   **Identity**

    ```haskell
    type I Comp = Identity
    ```

    `Comp f Identity` is equivalent to just `f`, because `Identity` adds no
    extra effects or structure.

*   **Constraints**

    ```haskell
    type CS Comp = Bind
    type CM Comp = Monad

    binterpret @Comp
        :: Bind h
        => f ~> h
        -> g ~> h
        -> Comp f g ~> h

    inL   @Comp :: Monad g => f        ~> Comp f g
    inR   @Comp :: Monad f => g        ~> Comp f g
    pureT @Comp :: Monad h => Identity ~> h
    ```

    `Bind`, from *semigroupoids*, is "`Monad` without `return`"; it only
    has `>>=` (called `>>-`).

    Somewhat serendipitously, the `CM` constraint associated with `Comp` is the
    infamous `Monad`.  Hopefully this insight also gives you some insight on
    the nature of `Monad` as an abstraction: it's a way to "interpret" in and
    out of `Comp` :)

*   **Induced Monoid**

    ```haskell
    type SF Day = Free1
    type MF Day = Free
    ```

    `Free f a` is a bunch of `f x`s composed with each other.  It is either:

    *   `a` (zero `f`s)
    *   `f a` (one `f`)
    *   `f (f a)` (two `f`s)
    *   `f (f (f a))` (three `f`s)
    *   .. etc.

    `Free` is very useful because it allows you to specify that your schema can
    have many `f`s, sequenced one after the other, in which the *choice* of
    "the next `f`" is allowed to depend on the *result* of "the previous `f`".

    For example, in an interactive "wizard" sort of schema, where `f`
    represents a wizard dialog box, we can represent our wizard using `Free f
    a` --- an ordered sequence of dialog boxes, where the choice of the next
    box can depend on result of the previous box.

    `Free1` is a version with "at least one" `f a`.

    See the information later on `Free` alone (in the single-argument functor
    combinator section) for more information on usage and utility.

[Control.Monad.Freer.Church]: https://hackage.haskell.org/package/functor-combinators/docs/Control-Monad-Freer-Church.html
[Data.Functor.Compose]: https://hackage.haskell.org/package/base/docs/Data-Functor-Compose.html

::::: {.note}
**Aside**

Let us pause for a brief aside to compare and contrast the hierarchy of the
above functor combinators, as there is an interesting progression we can draw
from them.

1.  `:+:`: Provide either, be ready for both.
2.  `:*:`: Provide both, be ready for either.
3.  `Day`: Provide both, be ready for both.
4.  `Comp`: Provide both (in order), be ready for both (in order).

:::::

### These1

*   **Origin**: *[Data.Functor.These][]*.

*   **Mixing Strategy**: "Either-or, or both": provide either (or both) cases,
    and user has to handle both possibilities.  An "inclusive either"

    ```haskell
    data These1 f g a
        = This1  (f a)
        | That1        (g a)
        | These1 (f a) (g a)
    ```

    This can be useful for situations where your schema/functor can be
    specified using one functor or another, or even both.  See description on
    `:+:` for examples.

    The person who creates the `These1 f g` decides which one to give, and the
    person who consumes/interprets/runs the `f :+: g` must provide a way of
    handling *both* situations.

    ```haskell
    binterpret @These
        :: Alt h
        => (f ~> h)
        -> (g ~> h)
        -> These f g a
        -> h a
    ```

    You can also pattern match on the `These1` directly to be more explicit
    with how you handle each of the tree cases.

*   **Identity**

    ```haskell
    type I These1 = Void
    ```

    `These1 f Void` is equivalent to just `f`, because it means the `That1` and
    `These1` branches will be impossible to construct, and you are left with
    only the `This1` branch.

*   **Constraints**

    ```haskell
    type CS These1 = Alt
    type CM These1 = Alt

    binterpret @These1
        :: Alt h
        => f ~> h
        -> g ~> h
        -> These1 f g ~> h

    inL   @These1 :: Alt g => f     ~> Comp f g
    inR   @These1 :: Alt f => g     ~> Comp f g
    pureT @These1 :: Alt h => Void1 ~> h
    ```

    You need at least `Alt` to be able to interpret out of a `These1`, because
    you need to be able to handle the case where you have *both* `f` and `g`,
    and need to combine the result.  However, you never need a full `Plus`
    because we always have at least one value to use.

*   **Induced Monoid**

    ```haskell
    type MF These1 = Steps
    ```

    `Steps`, the induced monoidal functor combinator, is the result of an
    infinite application of `These1 to the same value:

    ```haskell
    type Steps f = f `These1` f `These1` f `These1` f `These1` ... etc.
    ```

    It essentially represents an infinite *sparse* array of `f a`s, where an `f a` might
    exist at many different positions, with gaps here and there.  There is
    always at least *one* `f a`.

    Like `Step`, it's not particularly useful, but it can be used in situations
    where you want a giant infinite sparse array of `f a`s, each at a given
    position, with many gaps between them.

    I've skipped over the the induced semigroupoidal functor, which is
    `ComposeT Flagged Steps`; it requires an extra boolean "flag" because of
    some of the quirks of nonemptiness.  I feel it is even less useful than
    `Steps`.

[Data.Functor.These]: https://hackage.haskell.org/package/these/docs/Data-Functor-These.html

### LeftF / RightF

*   **Origin**: *[Data.HBifunctor][]*

*   **Mixing Strategy**: "Ignore the left" / "ignore the right".

    ```haskell
    data LeftF  f g a = LeftF  { runLeftF  :: f a }

    data RightF f g a = RightF { runRightF :: g a }
    ```

    You can think of `LeftF` as "`:+:` without the Right case,
    `R1`", or `RightF` as "`:+:` without the Left case, `L1`".  `RightF` can be
    considered a higher-order version of *Tagged*, which "tags" a value with
    some type-level information.

    This can be useful if you want the second (or first) argument to be
    ignored, and only be used maybe at the type level.

    For example, `RightF IgnoreMe MyFunctor` is equivalent to just `MyFunctor`,
    but you might want to use `IgnoreMe` as a phantom type to help limit what
    values can be used for what functions.

*   **Identity**

    Unlike the previous functor combinators, these three are only
    `Semigroupoidal`, not `Monoidal`: this is because there is no functor `i` such
    that `LeftF i g` is equal to `g`, for all `g`, and no functor `i` such that
    `RightF f i` is equal to `f`, for all `f`.

*   **Constraints**

    ```haskell
    type CS LeftF  = Unconstrained
    type CS RightF = Unconstrained
    ```

    Interpreting out of either of these is unconstrained, and can be done in
    any context.

*   **Induced Semigroup**

    ```haskell
    type SF LeftF = Flagged
    ```

    For `LeftF`, the induced semigroup is `Flagged`, which is the `f a ` tupled
    with a `Bool`.  See the information on `Flagged` for more details.
    This can be useful as a type that marks if an `f` is made with
    `inject`/`pure` and is "pure" (`False`), or "tainted" (`True`).  The
    *provider* of a `Flagged` can specify "pure or tainted", and the
    *interpreter* can make a decision based on that tag.

    ```haskell
    type SF RightF = Step
    ```

    For `RightF`, the induced semigroup is `Step`.  See `Step` and the
    information on `:+:` for more details.  This can be useful for having a
    value of `f a` at "some point", indexed by a `Natural`.

[Data.HBifunctor]: https://hackage.haskell.org/package/functor-combinators/docs/Data-HBifunctor.html

Single-Argument
---------------

Unary functor combinators usually directly "enhance" a functor with extra
capabilities --- usually in the form of a typeclass instance, or extra data
fields/constructors.

All of these can be "lifted into" with any constraint on `f`.

```haskell
class HFunctor t => Inject t where
    inject :: f ~> t f
```

`Inject` seems very similar to `MonadTrans`'s `lift`; the difference is that
`inject` must be *natural* on `f`: it can assume nothing about the structure of
`f`, and must work universally the same.  `MonadTrans`, in contrast, requires
`Monad f`.

Each one has an associated constraint, `C t`, which is the constraint on where
you can *interpret* or *run* values of the enhanced type:

```haskell
class Inject t => Interpret t where
    type C t :: (Type -> Type) -> Constraint

    interpret
        :: C t g
        => f ~> g
        -> t f ~> g
```

The constraint depends on the structure that the specific functor combinator
imbues.

An important law is that:

```haskell
interpret id . inject == id
```

This means that if we inject and immediately interpret out of, we should never
*lose* any information in `f`.  All of the original structure in `f` must stay
intact: functor combinators only ever *add* structure.

### Coyoneda

*   **Origin**: *[Data.Functor.Coyoneda][]*

-   **Enhancement**: The ability to map over the parameter; it's the free
    `Functor`.

    Can be useful if `f` is created using a `GADT` that cannot be given a
    `Functor` instance.

    For example, here is an indexed type that represents
    the type of a "form element", where the type parameter represents the
    output result of the form element.

    ```haskell
    data FormElem :: Type -> Type where
        FInput    :: FormElem String
        FTextbox  :: FormElem Text
        FCheckbox :: FormElem Bool
        FNumber   :: FormElem Int
    ```

    Then `Coyoneda FormElem` has a `Functor` instance.  We can now fmap over
    the result type of the form element; for example, `fmap :: (a -> b) ->
    Coyoneda FormElem a -> Coyoneda FormElem b` takes a form element whose
    result is an `a` and returns a form element whose result is a `b`.

*   **Constraint**

    ```haskell
    type C Coyoneda = Functor

    interpret @Coyoneda
        :: Functor g
        => f ~> g
        -> Coyoneda f ~> g
    ```

    Interpreting out of a `Coyoneda f` requires the target context to itself be
    `Functor`.  Usually, the context is an `Applicative` or `Monad`, so this
    is typically always satisfied.

    For example, if we want to "run" a `Coyoneda FormElem` into `IO`, this
    would be `interpret :: (forall x. FormElem x -> IO x) -> Coyoneda FormElem
    a -> IO a`.

[Data.Functor.Coyoneda]: https://hackage.haskell.org/package/kan-extensions/docs/Data-Functor-Coyoneda.html

### ListF / NonEmptyF

*   **Origin**: *[Control.Applicative.ListF][]*

*   **Enhancement**: The ability to offer multiple options for the interpreter
    to pick from; `ListF` is the free `Plus`, and `NonEmptyF` is the free
    `Alt`.

    ```haskell
    data ListF     f a = ListF     { runListF     :: [f a]          }
    data NonEmptyF f a = NonEmptyF { runNonEmptyF :: NonEmpty (f a) }
    ```

    Can be useful if you want to provide the ability when you *define* your
    schema to provide multiple `f a`s that the *interpreter*/consumer can
    freely pick from.

    For example, for a schema specifying a form, you might have multiple ways
    to enter a name.  If you had a `Name` schema `data Name a`, then you can
    represent "many different potential name inputs" schema as `ListF Name a`.

    Because this has a `Plus` instance, you can use `(<!>) :: ListF f a ->
    ListF f a -> ListF f a` to combine multiple option sets, and `zero :: ListF
    f a` to provide the "choice that always fails/is unusuable".

    `NonEmptyF` is a variety of `ListF` where you always have "at least one `f
    a`".  Can be useful if you want to ensure, for your interpreter's sake,
    that you always have at least one `f a` option to pick from.  For example,
    `NonEmptyF Name a` will always have at least *one* name schema.

    This is essentially `f` `:*:`d with itself multiple times; `ListF` is the
    monoidal functor combinator induced by `:*:`, and `NonEmptyF` is the
    semigroupoidal functor combinator induced by `:*:`.

    ```haskell
    x             <=> ListF [x]     <=> NonEmptyF (x :| [])
    x :*: y       <=> ListF [x,y]   <=> NonEmptyF (x :| [y])
    x :*: y :*: z <=> ListF [x,y,z] <=> NonEmptyF (x :| [y,z])
    ```

*   **Constraint**

    ```haskell
    type C ListF     = Plus
    type C NonEmptyF = Alt

    interpret @ListF
        :: Plus g
        => f ~> g
        -> ListF f ~> g

    interpret @NonEmptyF
        :: Alt g
        => f ~> g
        -> NonEmptyF f ~> g
    ```

    Interpreting out of a `ListF f` requires the target context to be
    `Plus`, and interpreting out of a `NonEmptyF f` requires `Alt` (because you
    will never have the empty case).  However, you can directly pattern match
    on the list and pick an item you want directly, which requires no
    constraint.

[Control.Applicative.ListF]: https://hackage.haskell.org/package/functor-combinators/docs/Control-Applicative-ListF.html

### Ap / Ap1

*   **Origin**: *[Control.Applicative.Free][]* / *[Data.Functor.Apply.Free][]*

*   **Enhancement**: The ability to provide multiple `f`s that the interpreter
    *must* consume *all* of; `Ap` is the free `Applicative`, and `Ap1` is the
    free `Apply`.

    While `ListF` may be considered "multiple options *offered*", `Ap` can be
    considered "multiple actions all *required*".  The interpreter must
    consume/interpret *all* of the multiple `f`s in order to interpret an `Ap`.

    For example, for a form schema, you might want to have multiple form
    elements.  If a single form element is `data FormElem a`, then you can
    make a multi-form schema with `Ap FormElem a`.  The consumer of the form
    schema must handle *every* `FormElem` provided.

    Note that ordering is not enforced: while the consumer must handle each `f`
    eventually, they are free to handle it in whatever order they desire.  In
    fact, they could even all be handled in parallel.  See `Free` for a version
    where ordering is enforced.

    Because this has an `Applicative` instance, you can use `(<*>) :: Ap f (a
    -> b) -> Ap f a -> Ap f b` to sequence multiple `Ap f`s together, and `pure
    :: a -> Ap f a` to produce a "no-op" `Ap` without any `f`s.

    `Ap` has some utility over `Free` in that you can pattern match on the
    constructors directly and look at each individual sequenced `f a`, for
    static analysis, before anything is ever run or interpreted.

    `Ap1` is a variety of `Ap` where you always have to have "at least one
    `f`".  Can be useful if you want to ensure, for example, that your form has
    at least one element.

    Note that this is essentially `f` `Day`d with itself multiple times; `Ap`
    is the monoidal functor combinator induced by `Day` and `Ap1` is the
    semigroupoidal functor combinator induced by `Day`.

*   **Constraint**

    ```haskell
    type C Ap  = Applicative
    type C Ap1 = Apply

    interpret @Ap
        :: Applicative g
        => f ~> g
        -> Ap f ~> g

    interpret @Ap1
        :: Apply g
        => f ~> g
        -> Ap1 f ~> g
    ```

    Interpreting out of an `Ap f` requires the target context to be
    `Applicative`, and interpreting out of a `Ap1 f` requires `Apply` (because
    you will never need the pure case).

[Control.Applicative.Free]: https://hackage.haskell.org/package/free/docs/Control-Applicative-Free.html
[Data.Functor.Apply.Free]: https://hackage.haskell.org/package/functor-combinators/docs/Data-Functor-Apply-Free.html

### Alt

*   **Origin**: *[Control.Alternative.Free][]*

*   **Enhancement**: A combination of both `ListF` and `Ap`: provide a choice
    (`ListF`-style) of sequences (`Ap`-style) of choices of sequences of
    choices ....; it's the free `Alternative`.

    ```haskell
    Alt f ~ ListF (Ap (ListF (Ap (ListF (Ap (...))))
          ~ ListF (Ap (Alt f))
    ```

    This type imbues `f` with both sequential "must use both" operations (via
    `<*>`) and choice-like "can use either" operations (via `<|>`).

    It can be useful for implementing parser schemas, which often involve both
    sequential and choice-like combinations.  If `f` is a primitive parsing
    unit, then `Alt f` represents a non-deterministic parser of a bunch of
    `f`s one after the other, with multiple possible results.  I wrote [an
    entire article][are] on the usage of this combinator alone to implement a
    version of regular expressions.

*   **Constraint**

    ```haskell
    type C Alt = Alternative

    interpret @Alt
        :: Alternative g
        => f ~> g
        -> Alt f ~> g
    ```

    Interpreting out of an `Alt f` requires the target context to be
    `Alternative` --- it uses `<*>` for sequencing, and `<|>` for choice.

[Control.Alternative.Free]: https://hackage.haskell.org/package/free/docs/Control-Alternative-Free.html

### Free / Free1

*   **Origin**: *[Control.Monad.Freer.Church][]*, which is a variant of
    *[Control.Monad.Free][]* that is compatible with `HFunctor`.

*   **Enhancement**: The ability to provide multiple `f`s that the interpreter
    must consume *in order*, sequentially --- the free `Monad`.

    Contrast with `Ap`, which also sequences multiple `f`s together, but
    without any enforced order.  It does this by *hiding* the "next `f a`" until
    the previous `f a` has already been interpreted.

    Perhaps more importantly, you can sequence `f`s in a way where the *choice
    of the next `f`* is allowed to depend on the *result of the previous `f`*.

    For example, in an interactive "wizard" sort of schema, where `f`
    represents a wizard dialog box, we can represent our wizard using `Free f
    a` --- an ordered sequence of dialog boxes, where the choice of the next
    box can depend on result of the previous box.  Contrast to `Ap`, where the
    choice of all dialog boxes must be made in advanced, up-front, before
    reading any input from the user.

    In having this, however, we loose the ability to be able to inspect each `f
    a` before interpreting anything.

    Because this has a `Monad` instance, you can use `(<*>) :: Free f (a
    -> b) -> Free f a -> Free f b` and `(>>=) :: Free f a -> (a -> Free f b) ->
    Free f b)` to sequence multiple `Free f`s together,
    and `pure :: a -> Free f a` to produce a "no-op" `Free` without any `f`s.

    `Free1` is a variety of `Free1` where you always have to have "at least one
    `f`".  Can be useful if you want to ensure, for example, that your wizard
    has at least one dialog box.

    Note that this is essentially `f` `Comp`d with itself multiple times;
    `Free` is the monoidal functor combinator induced by `Comp` and
    `Free1` is the semigroupoidal functor combinator induced by `Comp`.

*   **Constraint**

    ```haskell
    type C Free  = Monad
    type C Free1 = Bind

    interpret @Free
        :: Monad g
        => f ~> g
        -> Free f ~> g

    interpret @Free1
        :: Bind g
        => f ~> g
        -> Free1 f ~> g
    ```

    Interpreting out of a `Free f` requires the target context to be `Monad`,
    and interpreting out of a `Free1 f` requires `Bind` (because you will never
    need the pure case).

[Control.Monad.Free]: https://hackage.haskell.org/package/free/docs/Control-Monad-Free.html

### Lift / MaybeApply

*   **Origin**: *[Control.Applicative.Lift][]* / *[Data.Functor.Apply][]* (the
    same type)

*   **Enhancement**: Make `f` "optional" in the schema in a way that the
    interpreter can still work with as if the `f` was still there; it's the
    free `Pointed`.

    ```haskell
    data Lift f a = Pure  a
                  | Other (f a)

    newtype MaybeApply f a = MaybeApply { runMaybeApply :: Either a (f a) }
        -- ^ same type, from semigroupoids
    ```

    Can be useful so that an `f a` is *optional* for the schema definition, but
    in a way where the consumer can still continue from it as if they *had* the
    `f`.

    It can be used, for example, to turn an required parameter `Param a` into
    an optional paramter `Lift Param a`.

    Contrast this to `MaybeF`: this allows the interpreter to still "continue
    on" as normal even if the `f` is not there.  However, `MaybeF` forces the
    interpreter to abort if the `f` is not there.

    This can be thought of as `Identity :+: f`.

*   **Constraint**

    ```haskell
    type C Lift = Pointed

    interpret @Lift
        :: Pointed g
        => f ~> g
        -> Lift f ~> g
    ```

    Interpreting out of a `Lift f` requires the target context to be `Pointed`,
    from *[Data.Pointed][]* --- it uses `point :: Pointed f => a -> f a` to
    handle the case where the `f` is not there.

[Data.Pointed]: https://hackage.haskell.org/package/pointed/docs/Data-Pointed.html
[Control.Applicative.Lift]: https://hackage.haskell.org/package/transformers/docs/Control-Applicative-Lift.html
[Data.Functor.Apply]: https://hackage.haskell.org/package/semigroupoids/docs/Data-Functor-Apply.html

### MaybeF

*   **Origin**: *[Control.Applicative.ListF][]*

*   **Enhancement**: Make `f` "optional" in the schema in a way that the
    interpreter *must fail* if the `f` is not present.

    ```haskell
    newtype MaybeF f a = MaybeF { runMaybeF :: Maybe (f a) }
    ```

    Can be useful so that an `f a` is *optional* for the schema definition; if
    the `f` is not present, the consumer must abort the current branch, or find
    some other external way to continue onwards.

    Contrast this to `Lift`, which is an "optional" `f` that the consumer may
    continue on from.

*   **Constraint**

    ```haskell
    type C MaybeF = Plus

    interpret @MaybeF
        :: Plus g
        => f ~> g
        -> MaybeF f ~> g
    ```

    Interpreting out of a `Lift f` requires the target context to be
    `Plus` --- it uses `zero :: f a` to handle the case where the `f`
    is not there.  Note that this is actually "over-constrained": we really
    only need `zero`, and not all of `Plus` (which includes `<!>`).  However,
    there is no common typeclass in Haskell that provides this, so this is the
    most pragmatic choice.

### EnvT

*   **Origin**: *[Control.Comonad.Trans.Env][]*

*   **Enhancement**: Provide extra (monoidal) data alongside `f a` that the
    interpreter can access.  Basically tuples extra `e` alongside the `f a`.

    ```haskell
    newtype EnvT e f a = EnvT e (f a)
    ```

    You can use this to basically tuple some extra data alongside an `f a`. It
    can be useful if you want to provide extra information that isn't inside
    the `f` for the interpreter use for interpretation.

    When using `inject :: Monoid e => f a -> EnvT e f a`, it uses `mempty` as
    the initial `e` value.

    This can be thought of as `Const e :*: f`.

    This type exists specialized a few times here, as well:

    *   `Step` is `EnvT (Sum Natural)`
    *   `Flagged` is `EnvT Any`

*   **Constraint**

    ```haskell
    type C (EnvT e) = Unconstrained

    interpret @(EnvT e)
        :: f ~> g
        -> EnvT e f ~> g
    ```

    Interpreting out of `EnvT e` requires no constraints.

[Control.Comonad.Trans.Env]: https://hackage.haskell.org/package/comonad/docs/Control-Comonad-Trans-Env.html

### MapF / NEMapF

*   **Origin**: *[Control.Applicative.ListF][]*

*   **Enhancement**: Contain multiple `f a`s, each indexed at a specific *key*.

    ```haskell
    newtype MapF   k f a = MapF   { runMapF :: Map   k (f a) }
    newtype NEMapF k f a = NEMapF { runMapF :: NEMap k (f a) }
    ```

    This is very similar in functionality to `ListF` and `NonEmptyF`, except
    instead of "positional" location, each `f a` exists at a given index.
    `NEMapF k` is the "non-empty" variant.  You can think of this as a `ListF`
    plus `EnvT`: it's a "container" of multiple `f a`s, but each one exists
    with a given "tag" index `k`.

    In usage, like for `ListF`, the *definer* provides multiple "labeled" `f
    a`s, and the *interpreter* can choose to interpret some or all of them,
    with accews to each labeled.

    `inject` creates a singleton `Map` at key `mempty`.

    This is very useful in schemas that have sub-schemas indexed at specific
    keys.  For example, in a command line argument parser, if we have a functor
    that represents a single command:

    ```haskell
    data Command a
    ```

    We can immediately promote it to be a functor representing *multiple
    possible* named commands, each at a given string:

    ```haskell
    type Commands = MapF String Command
    ```

    So we can implement "git push" and "git pull" using:

    ```haskell
    push :: Command Action
    pull :: Command Action

    gitCommands :: Commands Action
    gitCOmmands = MapF . M.fromList $
        [ ("push", push)
        , ("pull", pull)
        ]
    ```

    This is also useful for specifying things like routes in a server.

    This type exists specialized as `Steps`, which is `NEMapF (Sum
    Natural)`.

*   **Constraint**

    ```haskell
    type C (MapF k  ) = Plus
    type C (NEMapF k) = Alt

    interpret @(MapF k)
        :: Plus g
        => f ~> g
        -> MapF f ~> g

    interpret @(NEMapF k)
        :: Alt g
        => f ~> g
        -> NEMapF f ~> g
    ```

    Interpreting out of a `MapF f` requires the target context to be `Plus`,
    and interpreting out of a `NEMapF f` requires `Alt` (because you will never
    have the empty case).  However, you can directly *look up* into the `Map`
    and pick an item you want directly, which requires no constraint.

### ReaderT

*   **Origin**: *[Control.Monad.Trans.Reader][]*

*   **Enhancement**: Provide each `f a` with access to some "environment" `r`.

    ```haskell
    newtype ReaderT r f a = ReaderT { runReaderT :: r -> f a }
    ```

    `ReaderT r` is often used to model some form of [dependency injection][]:
    it allows you to work "assuming" you had an `r`; later, when you *run* it,
    you provide the `r`.  It delays the evaluation of your final result until
    you provide the missing `r`.

    Another way of looking at it is that it makes your entire functor have
    values that are *parameterized* with an `r`.

    For example, if you have a form data type:

    ```haskell
    data FormElem a
    ```

    you can now make a form data type that is parameterized by the current
    server hostname:

    ```haskell
    type FormElemWithHost = ReaderT HostName FormElem
    ```

    The actual structure of your `FormElem` is deferred until you provide the
    `HostName`.

    Note that, unlike `ReaderT`, most monad transformers from *transformers*
    are actually valid functor combinators under our perspective here, because
    most of them are not *natural* on `f`: they require `Functor f`, at least,
    to implement `inject` or `hmap`.

*   **Constraint**

    ```haskell
    type C (ReaderT r) = MonadReader r

    interpret @(ReaderT r)
        :: MonadReader r g
        => f ~> g
        -> ReaderT r f ~> g
    ```

    Interpreting out of a `ReaderT r` requires requires the target context to
    be `MonadReader r`, which means it must have access to `ask :: MonadReader
    r f => f r`.

[dependency injection]: https://en.wikipedia.org/wiki/Dependency_injection
[Control.Monad.Trans.Reader]: https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Reader.html

### Step

*   **Origin**: *[Control.Applicative.Step][]*

*   **Enhancement**: Tuples the `f a` with an extra natural number index.

    ```haskell
    data Step f a = Step { stepPos :: Natural, stepVal :: f a }
    ```

    This is essentially a specialized `EnvT`: it's `EnvT (Sum Natural)`.

    This is a useful type because it can be seen as equivalent to `f :+: f :+:
    f :+: f :+: f ...` forever: it's an `f`, but at some index.  In
    *[Control.Applicative.Step][]*, we have specialized functions `stepUp` and
    `stepDown`, which allows you to "match" on the "first" `f` in that infinite
    chain; it will increment and decrement the index relatively to make this
    work properly.

*   **Constraint**

    ```haskell
    type C Step = Unconstrained

    interpret @Step
        :: f ~> g
        -> Step f ~> g
    ```

    Interpreting out of `Step` requires no constraints; we just drop the
    `Natural` data.

[Control.Applicative.Step]: https://hackage.haskell.org/package/functor-combinators/docs/Control-Applicative-Step.html

### Steps

*   **Origin**: *[Control.Applicative.Step][]*

*   **Enhancement**: The ability to offer multiple *indexed*
    options for the interpreter to pick from.  Like `NonEmptyF`, except with
    each `f a` existing at an indexed position that the consumer/interpreter
    can look up or access.

    ```haskell
    newtype Steps f a = Steps { getSteps :: NEMap Natural (f a) }
    ```

    This is like a mix between `NonEmptyF` and `Step`: multiple `f a` options
    (at least one) for the consumer/interpreter to pick from.  Unlike
    `NonEmptyF`, each `f a` exists at an "index" --- there might be one at 0,
    one at 5, one at 100, etc.

    Another way of looking at this is like an infinite *sparse array* of `f
    a`s: it's an inifinitely large collection where each spot may potentially
    have an `f a`.

    Useful for "provide options that the consumer can pick from, index, or
    access", like `ListF`/`NonEmptyF`.

    This type can be seen as an infinite ``f `These1` f `These1` f `These1` f
    ...``, and along these lines, `stepsDown` and `stepsUp` exist inside
    *[Control.Applicative.Step]* analogous to `stepUp` and `stepDown` to treat
    a `Steps` in this manner.

*   **Constraint**

    ```haskell
    type C Steps = Alt

    interpret @Steps
        :: Alt g
        => f ~> g
        -> Steps f ~> g
    ```

    Interpreting out of `Steps` requires an `Alt` to combine different
    possibilities.  It does not require a full `Plus` constraint because we
    never need `zero`: a `Steps f a` always has at least one `f a`.

### Flagged

*   **Origin**: *[Control.Applicative.Step][]*

*   **Enhancement**: The ability to "tag" a functor value with a `True`/`False`
    boolean value.

    ```haskell
    data Flagged f a = Flagged { flaggedFlag :: Bool, flaggedVal :: f a }
    ```

    This is essentially a specialized `EnvT`: it's `EnvT Any`.

    If created with `inject` or `pure`, it adds the flag `False`.  This is
    helpful for helping indicate if the value was created using a "pure" method
    like `inject` or `pure`, or an "impure" method (any other method, including
    direct construction).

*   **Constraint**

    ```haskell
    type C Flagged = Unconstrained

    interpret @Flagged
        :: f ~> g
        -> Flagged f ~> g
    ```

    Interpreting out of `Flagged` requires no constraints; we just drop the
    boolean flag.

### Final

*   **Origin**: *[Data.HFunctor.Final][]*

*   **Enhancement**: `Final c` will lift `f` into a free structure of any
    typeclass `c`; it will give it all of the actions/API of a typeclass for
    "free". `Final c f` is the "free `c`" over `f`.

    ```haskell
    data Final c f a
    ```

    In a way, this is the "ultimate free structure": it can fully replace all
    other free structures of typeclasses of kind `Type -> Type`.  For example:

    ```haskell
    Coyoneda  ~ Final Functor
    ListF     ~ Final Plus
    NonEmptyF ~ Final Alt
    Ap        ~ Final Applicative
    Ap1       ~ Final Apply
    Free      ~ Final Monad
    Free1     ~ Final Bind
    Lift      ~ Final Pointed
    IdentityT ~ Final Unconstrained
    ```

    All of these are connections are witnessed by instances of the typeclass
    `FreeOf` in *[Data.HFunctor.Final][]*.

    In fact, `Final c` is often more performant than the actual concrete free
    structures.

    The main downside is that you cannot directly pattern match on the
    structure of a `Final c` the same way you can pattern match on, say, `Ap`
    or `ListF`.  However, you can get often around this by using `Final Plus`
    for most of your operations, and then `interpret inject`-ing it into
    `ListF` when you want to actually pattern match.

    You can also think of this as the "ultimate `Interpret`", because with
    `inject` you can push `f` into `Final c f`, and with `interpret` you only
    ever need the `c` constraint to "run"/interpret this.

    So, next time you want to give an `f` the ability to `<*>` and `pure`, you
    can throw it into `Final Applicative`: `f` now gets "sequencing" abilities,
    and is equivalent to `Ap f`.

    If you want the API of a given typeclass `c`, you can inject `f` into
    `Final c`, and you get the API of that typeclass for free on `f`.

*   **Constraint**

    ```haskell
    type C (Final c) = c

    interpret @(Final c)
        :: c g
        => f ~> g
        -> Final c f ~> g
    ```

    Interpreting out of a `Final c` requires `c`, since that is the extra
    context that `f` is lifted into.

[Data.HFunctor.Final]: https://hackage.haskell.org/package/functor-combinators/docs/Data-HFunctor-Final.html

### Chain / Chain1

*   **Origin**: *[Data.HFunctor.Chain][]*

*   **Enhancement**: `Chain t` will lift `f` into a linked list of `f`s chained
    by `t`.

    ```haskell
    -- i is intended to be the identity of t
    data Chain t i f a = Done (i a)
                       | More (t f (Chain t i f a))
    ```

    For example, for `:*:`, `Chain (:*:) Proxy f` is equivalent to one of:

    ```haskell
    Proxy   <=> Done Proxy                           <=> ListF []
    x       <=> More (x :*: Done Proxy)              <=> ListF [x]
    x :*: y <=> More (x :*: More (y :*: Done Proxy)) <=> ListF [x,y]
    -- etc.
    ```

    For `:+:`, `Chain (:+:) Void1 f` is equivalent to one of:

    ```haskell
    L1 x           <=> More (L1 x)                         <=> Step 0 x
    R1 (L1 y)      <=> More (R1 (More (L1 y)))             <=> Step 1 y
    R1 (R1 (L1 z)) <=> More (R1 (More (R1 (More (L1 z))))) <=> Step 2 z
    -- etc.
    ```

    This is useful because it provides a nice uniform way to work with all
    "induced Monoidal functors".  That's because the following types are all
    isomorphic:

    ```haskell
    ListF ~ Chain (:*:)  Proxy
    Ap    ~ Chain Day    Identity
    Free  ~ Chain Comp   Identity
    Step  ~ Chain (:+:)  Void
    Steps ~ Chain These1 Void
    ```

    This isomorphism is witnessed by `unrollMF` (turn into the `Chain`) and
    `rerollMF` (convert back from the `Chain`) in *[Data.HFunctor.Chain][]*.

    We also have a "non-empty" version, `Chain1`, for induced semigroupoids:

    ```haskell
    data Chain1 t f a = Done1 (f a)
                      | More1 (t f (Chain1 t f a))
    ```

    ```haskell
    NonEmptyF ~ Chain1 (:*:)
    Ap1       ~ Chain1 Day
    Free1     ~ Chain1 Comp
    Step      ~ Chain1 (:+:)
    Steps     ~ Chain1 These1
    EnvT Any  ~ Chain1 LeftF
    Step      ~ Chain1 RightF
    ```

    Using `ListF`, `Ap`, `Free`, `Step`, `Steps`, etc. can sometimes feel very
    different, but with `Chain` you get a uniform interface to pattern match on
    (and construct) all of them in the same way.

    Using `NonEmptyF`, `Ap1`, `Free1`, `Step`, `Steps`, `EnvT`, etc. can
    sometimes feel very different, but with `Chain1` you get a uniform
    interface to pattern match on (and construct) all of them in the same way.

    The construction of `Chain` is inspired by [Oleg Grenrus's blog
    post][ufmf], and the construction of `Chain1` is inspired by
    implementations of finite automata and iteratees.

*   **Constraint**

    ```haskell
    type C (Chain  t (I t)) = CM t
    type C (Chain1 t      ) = CS t

    interpret @(Chain t (I t))
        :: CM t g
        => f ~> g
        -> Chain t (I t) f ~> g

    interpret @(Chain1 t)
        :: CS t g
        => f ~> g
        -> Chain1 t f ~> g
    ```

    Interpreting out of a `Chain` requires the monoidal constraint on `t`,
    since we have to "squish" all of the layers of `t` together with a
    potential empty case.  Interpreting out of a `Chain1` requires the
    semigroupoidal constraint on `t`, since we have to squish all of the layers
    of `t` together, but we don't have to worry about the empty case.

    For example, we have:

    ```haskell
    type C (Chain  (:*:) Proxy) = Plus
    type C (Chain1 (:*:)      ) = Alt

    interpret @(Chain (:*:) Proxy)
        :: Plus g
        => f ~> g
        -> Chain (:*:) Proxy f ~> g

    interpret @(Chain1 (:*:))
        :: Alt g
        => f ~> g
        -> Chain1 (:*:) f ~> g

    type C (Chain  Day Identity) = Applicative
    type C (Chain1 Day         ) = Apply

    type C (Chain  Comp Identity) = Monad
    type C (Chain1 Comp         ) = Bind
    ```

[Data.HFunctor.Chain]: https://hackage.haskell.org/package/functor-combinators/docs/Data-HFunctor-Chain.html

### IdentityT

*   **Origin**: *[Data.Functor.Identity][]*

*   **Enhancement**: None whatsoever; it adds no extra structure to `f`, and
    `IdentityT f` is the same as `f`; it's the "free `Unconstrained`"

    ```haskell
    data IdentityT f a = IdentityT { runIdentityT :: f a }
    ```

    This isn't too useful on its own, but it can be useful to give to the
    functor combinator combinators as a no-op functor combinator.  It can also
    be used to signify "no structure", or as a placeholder until you figure out
    what sort of structure you want to have.

    In that sense, it can be thought of as a "`ListF` with always one item", a
    "`MaybeF` that's always `Just`"', an "`Ap` with always one sequenced item",
    a "`Free` with always exactly one layer of effects", etc.

*   **Constraint**

    ```haskell
    type C IdentityT = Unconstrained

    interpret @IdentityT
        :: f ~> g
        -> IdentityT f ~> g
    ```

    Interpreting out of `IdentityT` requires no constraints --- it basically
    does nothing.

[Data.Functor.Identity]: https://hackage.haskell.org/package/base/docs/Data-Functor-Identity.html

### ProxyF / ConstF

*   **Origin**: *[Data.HFunctor][]*

*   **Enhancement**: "Black holes" --- they completely forget all the structure
    of `f`, and are impossible to `interpret` out of.
    `Impossible`".

    ```haskell
    data ProxyF f a = ProxyF
    data ConstF e f a = ConstF e
    ```

    `ProxyF` is essentially `ConstF ()`.

    These are both valid functor combinators in that you can inject into them,
    and `interpret id . inject == id` is *technically* true (the best kind of
    true).

    You can use them if you want your schema to be impossible to interpret, as
    a placeholder or to signify that one branch is uninterpretable.  In this
    sense, this is like a "`ListF` that is always empty" or a "`MaybeF` that is
    always `Nothing`".

    Because of this, they aren't too useful on their own --- they're more
    useful in the context of swapping out and combining or manipulating with
    other functor combinators or using with functor combinator combinators.

*   **Constraint**

    ```haskell
    type C ProxyF     = Impossible
    type C (ConstF e) = Impossible

    interpret @ProxyF
        :: Impossible g
        => f ~> g
        -> ProxyF f ~> g

    interpret @(ConstF e)
        :: Impossible g
        => f ~> g
        -> ConstF e f ~> g
    ```

    Interpreting out of these requires an impossible constraint.

[Data.HFunctor]: https://hackage.haskell.org/package/base/docs/Data-HFunctor.html

Combinator Combinators
----------------------

There exist higher-order functor combinator combinators that take functor
combinators and return new ones, too.  We can talk about a uniform interface
for them, but they aren't very common, so it is probably not worth the extra
abstraction.

### ComposeT


*   **Origin**: *[Control.Monad.Trans.Compose][]*

*   **Enhancement**: Compose enhancements from two different functor
    combinators

    ```haskell
    newtype ComposeT s t f a = ComposeT { getComposeT :: s (t f) a }
    ```

    Can be useful if you want to layer or nest functor combinators to get both
    enhancements as a *single* functor combinator*.

    Usually really only useful in the context of other abstractions that expect
    functor combinators, since this is the best way to turn two functor
    combinators into a third one.

*   **Constraint**

    ```haskell
    type C (ComposeT s t) = AndC (C s) (C t)

    interpret @(ComposeT s t)
        :: (C s g, C t g)
        => f ~> g
        -> ComposeT s t f ~> g
    ```

    Interpreting out of these requires the constraints on *both* layers.

[Control.Monad.Trans.Compose]: https://hackage.haskell.org/package/mmorph/docs/Control-Monad-Trans-Compose.html

### HLift

*   **Origin**: *[Data.HFunctor][]*

*   **Enhancement**: `HLift t f` lets `f` exist either unchanged, or with the
    structure of `t`.

    ```haskell
    data HLift t f a
        = HPure  (f a)
        | HOther (t f a)
    ```

    Can be useful if you want to "conditionally enhance" `f`.  Either `f` can
    be enhanced by `t`, or it can exist in its pure "newly-injected" form.

    If `t` is `Identity`, we get `EnvT Any`, or `f :+: f`: the "pure or impure"
    combinator.

*   **Constraint**

    ```haskell
    type C (HLift t) = C t

    interpret @(HLift t)
        :: C t g
        => f ~> g
        -> HLift t f ~> g
    ```

    Interpreting out of these requires the constraint on `t`, to handle the
    `HOther` case.

### HFree

*   **Origin**: *[Data.HFunctor][]*

*   **Enhancement**: `HFree t f` lets `f` exist either unchanged, or with
    multiple nested enhancements by `t`.

    ```haskell
    data HFree t f a
        = HReturn (f a)
        | HJoin   (t (HFree t f) a)
    ```

    It is related to `HLift`, but lets you lift over arbitrary many
    compositions of `t`, enhancing `f` multiple times.  This essentially
    creates a "tree" of `t` branches.

    One particularly useful functor combinator to use is `MapF`.  In our
    earlier examples, if we have

    ```haskell
    data Command a
    ```

    to represent the structure of a single command line argument parser, we can
    use

    ```haskell
    type Commands = MapF String Command
    ```

    to represent *multiple* potential named commands, each under a different
    `String` argument.  With `HFree`, we can also use:

    ```haskell
    type CommandTree = HFree (MapF String) Command
    ```

    to represent *nested* named commands, where each nested sub-command is
    descended on by a `String` key.

    For another example, `HFree IdentityT` is essentially `Step`.

*   **Constraint**

    ```haskell
    type C (HFree t) = C t

    interpret @(HFree t)
        :: C t g
        => f ~> g
        -> HFree t f ~> g
    ```

    Interpreting out of these requires the constraint on `t`, to handle the
    `HJoin` case.

    However, it is probably usually more useful to directly pattern match on
    `HReturn` and `HJoin` and handle the recursion explicitly.

    Alternatively, we can also define a recursive folding function (provided in
    *[Data.HFunctor][]*) to recursively fold down each branch:

    ```haskell
    foldHFree
        :: HFunctor t
        => (f ~> g)
        -> (t g ~> g)
        -> HFree t f ~> g
    ```

    This can be useful because it allows you to distinguish between the
    different branches, and also requires no constraint on `g`.

    Applied to the `CommandTree` example, this becomes:

    ```haskell
    foldHFree @(MapF String) @Command
        :: Command ~> g
        -> MapF String ~> g
        -> CommandTree ~> g
    ```

Closing Comments
----------------

As I discover more interesting or useful functor combinators (or as the
abstractions in *functor-combinators* change), I will continue to update this
post.  And, in the upcoming weeks and months I plan to present specific
programs I have written (and simple examples of usage) that will help show this
design pattern in use within a real program.

For now, I hope you can appreciate this as a reference to help guide your
exploration of unique "a la carte" (yet not fixed-point-centric) approach to
building your programs!  You can jump right into using these tools to build
your program *today* by importing *[Data.Functor.Combinator][]* or wherever
they can be found.

I'd be excited to hear about what programs you are able to write, so please do
let me know!  You can leave a comment, find me on [twitter at @mstk][twitter], or find
me on freenode irc idling on *#haskell* as *jle\`* if you want to share, or
have any questions.

[twitter]: https://twitter.com/mstk "Twitter"

Special Thanks
--------------

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts.  Very special
thanks to my supporter at the "Amazing" level on [patreon][], Josh Vera! :)

[patreon]: https://www.patreon.com/justinle/overview
