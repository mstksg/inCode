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

This post, then, will be a run-down on the wide variety of such "functor
combinators" across the Haskell ecosystem --- a functor combinator "zoo" of
sorts.  To speak about them all with the same language and vocabulary, this
post also serves as an overview of the *[functor-combinators][]* library, which
mostly pulls them all together and provides a unified interface for working
with them.

[functor-combinators]: https://hackage.haskell.org/package/functor-combinators

Prologue: What is a functor combinator?
---------------------------------------

A functor combinator takes "functors" (or other indexed types) and returns a new
functor, enhances or mixes them together in some way.

That is, they take things of kind `k -> Type` and themselves return a `k ->
Type`.  This lets us build complex functors out of simpler "primitive" ones.

For example, `ReaderT r` is a famous one that takes a functor `f` and enhances
it with "access to an `r` environment" functionality.  Another famous one is
`Free`, which takes a functor `f` and enhances it with "sequential binding"
capabilities: it turns `f` into a `Monad`.  Yet another is `EnvT e`, which
takes a functor `f` and enhances it by "tagging an `e` value" along our `f a`s.

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

The Zoo
-------

### Two-Argument

Binary functor combinators "mix together" two functors/indexed types in
different ways.

We can finally *interpret* (or "run") these into some target context (like
`Parser`, or `IO`), provided the target satisfies some constraints.

This constraint depends on the functor combinator in question; in particular,
there are two, which we will call `CS` and `CM`:

*   `CS t` is what we will call the constraint on where you can *interpret* or
    *run* values of the enhanced type:

    ```haskell
    binterpret
        :: (Semigroupoidal t, CS t h)
        => (f ~> h)
        => (g ~> h)
        -> (t f g ~> h)
    ```

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

    It should always be a subclass of `CS`.

As it turns out, `CS` and `CM` generalize the "has an identity" property of
many typeclasses --- for example, for `Comp` (functor composition), `CS` is
`Bind` ("`Monad` without `pure`"), and `CM` is `Monad`.

Most of these also have an identity, `I t`, where applying `t f (I t)` leaves
`f` unchanged (`t f (I t)` is isomorphic to `f`) and `t (I t) f` is also just
`f`.  This is represented by the associated type `I t`.

One interesting property of these is that for a lot of these, if we have a
binary functor combinator `*`, we can represent a type `f | f * f | f * f * f |
f * f * f * f | ...` ("repeatedly apply to something multiple times"), which
essentially forms a linked list along that functor combinator.  We call this
the "induced monoidal functor combinator", given by `MF t`.  We can also make a
"non-empty variant", `SF t`, the "induced semigroupoidal functor combinator",
which contains "at least one `f`".

You can "convert" back and forth by using:

```haskell
toMF   :: t f f ~> MF t
nilMF  :: I t ~> MF t
consMF :: t f (MF t) ~> MF t
```

and other helper functions.

If this is unclear, hopefully the following concrete examples will help
illustrate.

#### :+: / Sum

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
    Repeatedly using `consMF` will push the `f` further and further along the
    list.

[Data.Either]: https://hackage.haskell.org/package/base/docs/Data-Either.html
[Data.Functor.Sum]: https://hackage.haskell.org/package/base/docs/Data-Functor-Sum.html
[GHC.Generics]: https://hackage.haskell.org/package/base/docs/GHC-Generics.html

#### :\*: / Product

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

    Incidentally, `NonEmptyF` is the "free `Alt`" (it gives a functor `Alt`-ness,
    and nothing more), and `ListF` is the "free `Plus`" (it gives a functor
    `Plus`-ness, and nothing more).

[GHC.Generics]: https://hackage.haskell.org/package/base/docs/GHC-Generics.html
[Data.Functor.Product]: https://hackage.haskell.org/package/base/docs/Data-Functor-Product.html
[Data.Functor.Alt]: https://hackage.haskell.org/package/semigroupoids/docs/Data-Functor-Alt.html
[Data.Functor.Plus]: https://hackage.haskell.org/package/semigroupoids/docs/Data-Functor-Plus.html

#### Day

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

    Incidentally, `Ap1` is the "free `Apply`" (it gives a functor `Apply`-ness,
    and nothing more), and `Ap` is the "free `Applicative`" (it gives a functor
    `Applicative`-ness, and nothing more).

[Data.Functor.Day]: https://hackage.haskell.org/package/kan-extensions/docs/Data-Functor-Day.html
[Data.Functor.Apply]: https://hackage.haskell.org/package/semigroupoids/docs/Data-Functor-Apply.html

#### Comp

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

    Incidentally, `Free1` is the "free `Bind`" (it gives a functor `Bind`-ness,
    and nothing more), and `Free` is the "free `Monad`" (it gives a functor
    `Monad`-ness, and nothing more).


[Control.Monad.Freer.Church]: https://hackage.haskell.org/package/functor-combinators/docs/Control-Monad-Freer-Church.html
[Data.Functor.Compose]: https://hackage.haskell.org/package/base/docs/Data-Functor-Compose.html

#### These1

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

#### LeftF / RightF

*   **Origin**: *[Data.HBifunctor][]* (for `LeftF` and `RightF`)

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
    *provider* of an `EnvT Any f` can specify "pure or tainted", and the
    *interpreter* can make a decision based on that tag.

    ```haskell
    type SF RightF = Step
    ```

    For `RightF`, the induced semigroup is `Step`.  See `Step` and the
    information on `:+:` for more details.  This can be useful for having a
    value of `f a` at "some point", indexed by a `Natural`.

[Data.HBifunctor]: https://hackage.haskell.org/package/functor-combinators/docs/Data-HBifunctor.html
