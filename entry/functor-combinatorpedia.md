The Functor Combinatorpedia

============================

> Originally posted by [Justin Le](https://blog.jle.im/) on June 19, 2019.
> [Read online!](https://blog.jle.im/entry/functor-combinatorpedia.html)

**functor-combinators**:
[hackage](https://hackage.haskell.org/package/functor-combinators) /
[github](https://github.com/mstksg/functor-combinators)

(*Note:* This post has been heavily revised to reflect the
functor-combinators-0.2 refactoring, as of November 2019. For reference, [the
original
post](https://github.com/mstksg/inCode/blob/ceee9f33492bb703380d877477728feb4fe60a6a/entry/functor-combinatorpedia.md)
is available on github.)

(*Note 2:* The section on contravariant functor combinators was added following
the release of *functor-combinators-0.3* in August 2020, which added support for
contravariant and invariant functor combinators.)

Recently I've been very productive what I have been calling the "Functor
Combinator" design pattern. It is heavily influenced by ideas like [Data types a
la Carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf)
and [unified free monoidal
functors](http://oleg.fi/gists/posts/2018-02-21-single-free.html), but the end
goal is slightly different in spirit. The goal is to represent schemas, DSL's,
and computations (things like parsers, things to execute, things to consume or
produce data) by assembling "self-evident" basic primitives and subjecting them
to many *different* successive transformations and combiners (through
combinators, free structures, tensors, and other options). The process of doing
so:

1.  Forces you to make explicit decisions about the structure of your
    computation type as an ADT.
2.  Allows you to retain isolation of fundamental parts of your domain as
    separate types
3.  Lets you manipulate the structure of your final computation type through
    *normal Haskell techniques* like pattern matching. The structure is
    available throughout the entire process, so you can replace individual
    components and values within your structure.
4.  Allows you to fully *reflect* the structure of your final computation
    through pattern matching and folds, so you can inspect the structure and
    produce useful summaries.

Like "data types a la carte" and free monad/applicative/alternative designs,
these techniques allow you to separate the assembly and inspection of your
programs from the "running" of them.[^1] However, the main difference is that
here we focus not just on products and sums, but many different varied and
multi-purpose combinators --- a "zoo" of combinators. The fixed point is *not*
the end goal. The actual ADT data types *themselves* are the goal.

This post is a run-down on the wide variety of such "functor combinators" across
the Haskell ecosystem --- a functor combinatorpedia. To speak about them all
with the same language and vocabulary, this post also serves as an overview of
the
*[functor-combinators](https://hackage.haskell.org/package/functor-combinators)*
library, which doesn't really define these functor combinators, but rather pulls
them all together and provides a unified interface for working with them. Most
of these types and typeclasses are exported by
*[Data.Functor.Combinator](https://hackage.haskell.org/package/functor-combinators/docs/Data-Functor-Combinator.html)*.
Of course, the end-goal is to work with these data types *themselves* directly,
so not *everything* is meant to be doable with these typeclasses; they only
serve to unite some common aspects.

Right now I already have some posts about this general design pattern,
["Interpreters a la Carte" in Advent of Code 2017
Duet](https://blog.jle.im/entry/interpreters-a-la-carte-duet.html) and
[Applicative Regular Expressions using the Free
Alternative](https://blog.jle.im/entry/free-alternative-regexp.html), but I do
have some posts planned in the future going through projects using this unified
interface. In a way, this post also serves as the "introduction to free
structures" that I always wanted to write :)

Please refer to the [table of
contents](https://blog.jle.im/entry/functor-combinatorpedia.html#title) if you
are using this as a reference!

## Preface: What is a functor combinator?

A functor combinator takes "functors" (or any other indexed type, `k -> Type`)
and returns a new functor, enhances or mixes them together in some way. That is,
they take things of kind `k -> Type` and themselves return a `j -> Type`. This
lets us build complex functors/indexed types out of simpler "primitive" ones.
This includes many some monad transformers, free structures, and tensors.

For example, `ReaderT r` is a famous one that takes a functor `f` and enhances
it with "access to an `r` environment" functionality. Another famous one is
`Free`, which takes a functor `f` and enhances it with "sequential binding"
capabilities: it turns `f` into a `Monad`.

The main thing that distinguishes these functor combinators from things like
monad transformers is that they are "natural on `f`": they work on *all* `f`s,
not just monads, and assume no structure (not even `Functor`).

Sometimes, we have binary functor combinators, like `:+:`, which takes two
functors `f` and `g` and returns a functor that is "either" `f` or `g`. Binary
functor combinators "mix together" the functionality of different functors in
different ways.

### Examples

If your final DSL/program/schema is some functor, then functor combinators allow
you to construct your final functor by combining simpler "primitive" functors,
and take advantage of common functionality.

For example, if you were making a data type/EDSL to describe a command line
argument parser, you might have two primitives: `data Arg a`, for positional
arguments parsing `a`, and `data Option a`, for `--flag` non-positional options
parsing `a`. From there, you can *choose* what structure of command line
arguments you want to be able to express.

For instance, a structure that can support multiple arguments and optionally a
single `Option` would be:

``` haskell
type CommandArgs = Ap Arg :*: Lift Option
```

And a structure that supports *multiple named commands* on top of that would be:

``` haskell
type CommandArgs = MapF String (Ap Arg :*: Lift Option)
```

You can mix or match combinators to decide exactly what sort of structures you
allow in your DSL.

Now, instead of writing one "giant"
`runParser :: MapF String (Ap Arg :*: Lift Option) a -> IO a` function, you can
instead just write parsers for your simple primitives `Arg a -> IO a` and
`Option a -> IO a`, and then use functor combinator tools to "promote" them to
being runnable on a full `MapF String (Ap Arg :*: Lift Option)` without any
extra work.

### Common Functionality

Most of these functor combinators allow us to "swap out" the underlying functor,
retaining all of the "enhanced" structure. We abstract over all of these using
`hmap` for single-argument functor combinators ("enhancers") and `hbimap` for
two-argument functor combinators ("mixers").

``` haskell
class HFunctor t where
    -- | Swap out underlying functor for a single-argument functor combinator
    hmap
        :: (forall x. f x -> g x)
        -> t f a
        -> t g a

class HBifunctor t where
    -- | Swap out underlying functors for a two-argument functor combinator
    hbimap
        :: (forall x. f x -> h x)
        -> (forall x. g x -> j x)
        -> t f g a
        -> t h j a
```

However, for this post, the concept of a "natural transformation" between `f`
and `g` --- a function of type `forall x. f x -> g x`, is given a type synonym:

``` haskell
type f ~> g = forall x. f x -> g x
```

Then the type signatures of `hmap` and `hbimap` become:

``` haskell
class HFunctor t where
    hmap
        :: f ~> g
        -> t f ~> t g

class HBifunctor t where
    hbimap
        :: f ~> h
        -> g ~> j
        -> t f g ~> t h j
```

What does it mean exactly when we say that `hmap` and `hbimap` "preserve the
enhanced structure"? Well, for example, the type
`newtype ListF f a = ListF [f a]` is essentially a list of `f a`s. `hmap` will
swap out and replace each `f a`, but it must *preserve the relative order*
between each of the original `f a`s. It must also preserve the *length* of the
list. It's a complete "in-place swap". This is formalizing by requiring
`hmap id == id` and `hbimap id id == id`.

You can also always "lift" a functor value into its transformed type. We
abstract over this by using `inject` (for single-argument functors) and `inL`
and `inR` (for two-argument functors):

``` haskell
-- single argument functor combinators
inject :: f ~> t f

-- two-argument functor combinators
inL :: MonoidIn t i f
    => f ~> t f g

inR :: MonoidIn t i g
    => g ~> t f g
```

Finally, in order to *use* any functor combinators, you have to *interpret* them
into some target context. The choice of combinator imposes some constraints on
the target context. We abstract over this using `interpret` and `binterpret`:

``` haskell
class Interpret t f where
    -- | Interpret unary functor combinator
    interpret
        :: g ~> f             -- ^ interpreting function
        -> t g ~> f

class SemigroupIn t i f where
    -- | Interpret binary functor combinator
    binterpret
        :: g ~> f             -- ^ interpreting function on g
        -> h ~> f             -- ^ interpreting function on h
        -> t g h ~> f
```

Having the typeclass `Interpret` (and `SemigroupIn`) take both `t` and `f` means
that there are certain limits on what sort of `f` you can interpret into.

One nice consequence of this approach is that for many such schemas/functors you
build, there might be many *useful* target functors. For example, if you build a
command line argument parser schema, you might want to run it in `Const String`
to build up a "help message", or you might want to run it in `Parser` to parse
the actual arguments or run pure tests, or you might want to run it in `IO` to
do interactive parsing.

For some concrete examples of these functor combinators and their constraints:

``` haskell
instance Monad f => Interpret Free f

interpret @Free
    :: Monad g
    => (g ~> f)
    -> Free g a
    -> f a

instance SemigroupIn (:+:) V1 f

binterpret @(:+:)
    :: (g ~> f)
    -> (h ~> f)
    -> (g :+: h) a
    -> f a
```

We see that `interpret` lets you "run" a `Free` in any monad `f`, and
`binterpret` lets you "run" a function over both *branches* of an `g :+: h` to
produce an `f`.

From these, we can also build a lot of useful utility functions (like `retract`,
`biretract`, `getI`, `biget`, etc.) for convenience in actually working on them.
These are provided in
*[functor-combinators](https://hackage.haskell.org/package/functor-combinators)*.

Without further ado, let's dive into the zoo of functor combinators!

## Two-Argument

Binary functor combinators "mix together" two functors/indexed types in
different ways.

We can finally *interpret* (or "run") these into some target context (like
`Parser`, or `IO`), provided the target satisfies some constraints.

For the most part, binary functor combinators `t` are instances of both
`Associative t` and `Tensor t i`. Every `t` is associated with `i`, which is the
"identity" functor that leaves `f` unchanged: `t f i` is the same as `f`, and
`t i f` is the same as `f` as well.

For example, we have `Comp`, which is functor composition:

``` haskell
newtype Comp f g a = Comp (f (g a))
```

We have an instance `Associative Comp` and `Tensor Comp Identity`, because
`Comp f Identity` (composing any functor with `Identity`, `f (Identity a)`) is
just the same as `f a` (the original functor); also, `Comp Identity f` (or
`Identity (f a)`) is the same as `f a`.

From there, some functors support being "merged" (interpreted, or collapsed)
from a binary functor combinator, or being able to be injected "into" a binary
functor combinator. Those functors `f` have instances `SemigroupIn t f` and
`MonoidIn t i f`. If a functor `f` is `SemigroupIn t f`, we can `interpret` out
of it:

``` haskell
binterpret
    :: SemigroupIn t f
    => (g ~> f)
    -> (h ~> f)
    -> (t g h ~> f)

biretract
    :: SemigroupIn t f
    => t f f ~> f
```

And if a functor `f` is `MonoidIn t i f`, we can "inject" into it:

``` haskell
pureT
    :: MonoidIn t i f
    => i ~> f

inL :: MonoidIn t i g
    => f ~> t f g

inR :: MonoidIn t i f
    => g ~> t f g
```

A more detailed run-down is available in the docs for
*[Data.Functor.Combinator](https://hackage.haskell.org/package/functor-combinators/docs/Data-Functor-Combinator.html)*.

One interesting property of these is that for tensors, if we have a binary
functor combinator `*`, we can represent a type
`f | f * f | f * f * f | f * f * f * f | ...` ("repeatedly apply to something
multiple times"), which essentially forms a linked list along that functor
combinator. This is like a linked list with `t` as the "cons" operation, so we
call this `ListBy t`. We can also make a "non-empty variant", `NonEmptyBy t`,
which contains "at least one `f`".

For example, the type that is either `a`, `f a`, `f (f a)`, `f (f (f a))`, etc.
is `Free f a`, so that `type ListBy Comp = Free`. The type that is either `f a`,
`f (f a)`, `f (f (f a))`, etc. (at least one layer of `f`) is `Free1 f a`, so
`type NonEmptyBy Comp = Free1`.

*functor-combinators* provides functions like `toListBy :: t f f ~> ListBy t f`
to abstract over "converting" back and forth between `t f f a` and linked list
version `ListBy t f a` (for example, between `Comp f f a` and `Free f a`).

### :+: / Sum

-   **Origin**:
    *[GHC.Generics](https://hackage.haskell.org/package/base/docs/GHC-Generics.html)*
    (for `:+:`) /
    *[Data.Functor.Sum](https://hackage.haskell.org/package/base/docs/Data-Functor-Sum.html)*
    (for `Sum`)

-   **Mixing Strategy**: "Either-or": provide either case, and user has to
    handle both possibilities. Basically higher-order `Either`.

    ``` haskell
    data (f :+: g) a
        = L1 (f a)
        | R1 (g a)

    data Sum f g a
        = InL (f a)
        | InR (g a)
    ```

    It can be useful for situations where you can validly use one or the other
    in your schema or functor. For example, if you are describing an HTTP
    request, we could have `data GET a` describing a GET request and
    `data POST a` describing a POST request; `(GET :+: POST) a` would be a
    functor that describes either a GET or POST request.

    The person who creates the `f :+: g` decides which one to give, and the
    person who consumes/interprets/runs the `f :+: g` must provide a way of
    handling *both*

    ``` haskell
    binterpret @(:+:)
        :: (g ~> f)
        -> (h ~> f)
        -> (g :+: h) a
        -> f a
    ```

    `binterpret` becomes analogous to `either` from
    *[Data.Either](https://hackage.haskell.org/package/base/docs/Data-Either.html)*

-   **Identity**

    ``` haskell
    instance Tensor (:+:) V1

    -- | Data type with no inhabitants
    data V1 a
    ```

    `f :+: V1` is equivalent to just `f`, because you can never have a value of
    the right branch.

-   **Monoids**

    ``` haskell
    instance SemigroupIn (:+:) f
    instance MonoidIn    (:+:) V1 f

    binterpret @(:+:)
        :: (g ~> f)
        -> (h ~> f)
        -> (g :+: h) a
        -> f a

    inL   @(:+:) :: f     ~> f :+: g
    inR   @(:+:) :: g     ~> f :+: g
    pureT @(:+:) :: V1 ~> h
    ```

    *All* haskell functors are monoids in `:+:`. You can call `binterpret`,
    `inL`, `inR`, etc. with anything.

    However, note that `pureT` is effectively impossible to call, because no
    values of type `V1 a` exist.

-   **List type**

    ``` haskell
    type NonEmptyBy (:+:) = Step
    type ListBy     (:+:) = Step
    ```

    `Step` is the result of an infinite application of `:+:` to the same value:

    ``` haskell
    type Step f = f :+: f :+: f :+: f :+: f :+: f :+: ... etc.

    -- actual implementation
    data Step f a = Step
      { stepPos :: Natural
      , stepVal :: f a
      }
    ```

    The correspondence is:

    ``` haskell
    L1 x           <=> Step 0 x
    R1 (L1 y)      <=> Step 1 y
    R1 (R1 (L1 z)) <=> Step 2 z
    -- etc.
    ```

    It's not a particularly useful type, but it can be useful if you want to
    provide an `f a` alongside "which position" it is on the infinite list.

### :\*: / Product

-   **Origin**:
    *[GHC.Generics](https://hackage.haskell.org/package/base/docs/GHC-Generics.html)*
    (for `:*:`) /
    *[Data.Functor.Product](https://hackage.haskell.org/package/base/docs/Data-Functor-Product.html)*
    (for `Product`)

-   **Mixing Strategy**: "Both, separately": provide values from *both*
    functors, and the user can choose which one they want to use. Basically a
    higher-order tuple.

    ``` haskell
    data (f :*: g) a = f a :*: g a

    data Product f g a = Pair (f a) (g a)
    ```

    It can be useful for situations where your schema/functor must be
    *specified* using *both* functors, but the *interpreter* can choose to use
    only one or the other (or both).

    ``` haskell
    prodOutL :: (f :*: g) ~> f
    prodOutL (x :*: _) = x

    prodOutR :: (f :*: g) ~> g
    prodOutR (_ :*: y) = y
    ```

-   **Identity**

    ``` haskell
    instance Tensor (:*:) Proxy

    -- | Data type with only a single constructor and no information
    data Proxy a = Proxy
    ```

    `f :*: Proxy` is equivalent to just `f`, because the left hand side doesn't
    add anything extra to the pair.

-   **Monoids**

    ``` haskell
    instance Alt  f => SemigroupIn (:*:) f
    instance Plus f => MonoidIn    (:*:) Proxy f

    binterpret @(:*:)
        :: Alt f
        => g ~> f
        -> h ~> f
        -> (g :*: h) ~> f

    inL   @(:*:) :: Plus g => f     ~> f :*: g
    inR   @(:*:) :: Plus f => g     ~> f :*: g
    pureT @(:*:) :: Plus h => Proxy ~> h
    ```

    `Alt`, from
    *[Data.Functor.Alt](https://hackage.haskell.org/package/semigroupoids/docs/Data-Functor-Alt.html)*
    in *semigroupoids*, can be thought of a "higher-kinded semigroup": it's like
    `Alternative`, but with no `Applicative` constraint and no identity:

    ``` haskell
    class Alt f where
        (<!>) :: f a -> f a -> f a
    ```

    It is used to combine the results in both branches of the `:*:`.

    To introduce an "empty" branch, we need `Plus` (in
    *[Data.Functor.Plus](https://hackage.haskell.org/package/semigroupoids/docs/Data-Functor-Plus.html)*),
    which is like a higher-kinded `Monoid`, or `Alternative` with no
    `Applicative`:

    ``` haskell
    class Alt f => Plus f where
        zero :: f a
    ```

-   **List type**

    ``` haskell
    type NonEmptyBy (:*:) = NonEmptyF
    type ListBy     (:*:) = ListF
    ```

    `ListF f a` is a "list of `f a`s". It represents the possibility of having
    `Proxy` (zero items), `x :: f a` (one item), `x :*: y` (two items),
    `x :*: y :*: z` (three items), etc.

    It's basically an ordered collection of `f a`s `:*:`d with each other.

    ``` haskell
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

### Day

-   **Origin**:
    *[Data.Functor.Day](https://hackage.haskell.org/package/kan-extensions/docs/Data-Functor-Day.html)*

-   **Mixing Strategy**: "Both, together forever": provide values from *both*
    functors, and the user *must* also *use* both.

    It can be useful for situations where your schema/functor must be
    *specified* using *both* functors, and the user must also *use* both.

    ``` haskell
    binterpret @Day
        :: Apply f          -- superclass of Applicative
        => (g ~> f)
        -> (h ~> f)
        -> Day g h ~> f
    ```

    Unlike for `:*:`, you always have to interpret *both* functor values in
    order to interpret a `Day`. It's a "full mixing".

    The mechanism for this is interesting in and of itself. Looking at the
    definition of the data type:

    ``` haskell
    data Day f g a = forall x y. Day (f x) (g y) (x -> y -> a)
    ```

    We see that because `x` and `y` are "hidden" from the external world, we
    can't directly use them without applying the "joining" function
    `x -> y -> a`. Due to how existential types work, we can't get anything out
    of it that "contains" `x` or `y`. Because of this, *using* the joining
    function requires *both* `f x` and `g y`. If we only use `f x`, we can only
    get, at best,`f (y -> a)`; if we only use `g y`, we can only get, at best,
    `g (x -> a)`. In order to fully eliminate *both* existential variables, we
    need to get the `x` and `y` from *both* `f x` and `g y`, as if the two
    values held separate halves of the key.

-   **Identity**

    ``` haskell
    instance Tensor Day Identity
    ```

    `Day f Identity` is equivalent to just `f`, because `Identity` adds no extra
    effects or structure.

-   **Monoids**

    ``` haskell
    instance Apply       f => SemigroupIn Day f
    instance Applicative f => MonoidIn    Day Identity f

    binterpret @Day
        :: Apply f
        => (g ~> f)
        -> (h ~> f)
        -> Day g h ~> f

    inL   @Day :: Applicative g => f        ~> Day f g
    inR   @Day :: Applicative f => g        ~> Day f g
    pureT @Day :: Applicative h => Identity ~> h
    ```

    `Apply`, from
    *[Data.Functor.Apply](https://hackage.haskell.org/package/semigroupoids/docs/Data-Functor-Apply.html)*
    in *semigroupoids*, is "`Applicative` without `pure`"; it only has `<*>`
    (called `<.>`).

    `pureT` is essentially `pure :: Applicative h => a -> h a`.

-   **List type**

    ``` haskell
    type NonEmptyBy Day = Ap1
    type ListBy     Day = Ap
    ```

    `Ap f a` is a bunch of `f x`s `Day`d with each other. It is either:

    -   `a` (zero `f`s)
    -   `f a` (one `f`)
    -   `Day f f a` (two `f`s)
    -   `Day f (Day f f) a` (three `f`s)
    -   .. etc.

    Like `ListF` this is very useful if you want your schema to provide a "bag"
    of `f a`s and your interpreter *must use all of them*.

    For example, if we have a schema for a command line argument parser, each
    `f` may represent a command line option. To interpret it, we must look at
    *all* command line options.

    `Ap1` is a version with "at least one" `f a`.

    See the information later on `Ap` alone (in the single-argument functor
    combinator section) for more information on usage and utility.

### Comp

-   **Origin**:
    *[Control.Monad.Freer.Church](https://hackage.haskell.org/package/functor-combinators/docs/Control-Monad-Freer-Church.html)*.
    Note that an equivalent type is also found in
    *[GHC.Generics](https://hackage.haskell.org/package/base/docs/GHC-Generics.html)*
    and
    *[Data.Functor.Compose](https://hackage.haskell.org/package/base/docs/Data-Functor-Compose.html)*,
    but they are incompatible with the `HBifunctor` typeclass because they
    require the second input to have a `Functor` instance.

-   **Mixing Strategy**: "Both, together, sequentially" : provide values from
    *both* functors; the user must *use* both, and *in order*.

    ``` haskell
    newtype Comp f g a = Comp (f (g a))
    ```

    It can be useful for situations where your schema/functor must be specified
    using both functors, and the user must *use* both, but also enforcing that
    they must use both in the *given order*: that is, for a `Comp f g`, they
    interpret `f` *before* they interpret `g`.

    ``` haskell
    binterpret @Comp
        :: Bind f          -- superclass of Monad
        => (g ~> f)
        -> (h ~> f)
        -> Comp g h ~> f
    ```

    Unlike for `:*:`, you always have to interpret *both* functor values. And,
    unlike for `Day`, you must interpret both functor values *in that order*.

-   **Identity**

    ``` haskell
    instance Tensor Comp Identity
    ```

    `Comp f Identity` is equivalent to just `f`, because `Identity` adds no
    extra effects or structure.

-   **Monoids**

    ``` haskell
    instance Bind  f => SemigroupIn Comp f
    instance Monad f => MonoidIn    Comp Identity f

    binterpret @Comp
        :: Bind f
        => (g ~> f)
        -> (h ~> f)
        -> Comp g h ~> f

    inL   @Comp :: Monad g => f        ~> Comp f g
    inR   @Comp :: Monad f => g        ~> Comp f g
    pureT @Comp :: Monad h => Identity ~> h
    ```

    `Bind`, from *\[Data.Functor.Bind\]\[\]* in *semigroupoids*, is "`Monad`
    without `return`"; it only has `>>=` (called `>>-`).

    Somewhat serendipitously, the constraint associated with monoids in `Comp`
    is none other than the infamous `Monad`.

    This might sound familiar to your ears --- it's the realization of the joke
    that "monads are monoids in the category of (endo)functors". The idea is
    that we can make a tensor like `Comp` over functors, and that "monoids in"
    that tensor correspond exactly to `Monad` instances. A part of the joke that
    we can now also see is that monads aren't the *only* monoids in the category
    of endofunctors: they're just the ones that you get when you tensor over
    `Comp`. But we see now that if you use `Day` as your tensor, then "monoids
    in the category of functors over `Day`" are actually `Applicative`
    instances! And that the monoids over `:*:` are `Alt` instances, etc.

    Theory aside, hopefully this insight also gives you some insight on the
    nature of `Monad` as an abstraction: it's a way to "interpret" in and out of
    `Comp`, which enforces an ordering in interpretation :)

-   **List type**

    ``` haskell
    type NonEmptyBy Day = Free1
    type LIstBy     Day = Free
    ```

    `Free f a` is a bunch of `f x`s composed with each other. It is either:

    -   `a` (zero `f`s)
    -   `f a` (one `f`)
    -   `f (f a)` (two `f`s)
    -   `f (f (f a))` (three `f`s)
    -   .. etc.

    `Free` is very useful because it allows you to specify that your schema can
    have many `f`s, sequenced one after the other, in which the *choice* of "the
    next `f`" is allowed to depend on the *result* of "the previous `f`".

    For example, in an interactive "wizard" sort of schema, we can have a
    functor representing a dialog box with its result type:

    ``` haskell
    data Dialog a
    ```

    We can then represent our wizard using `Free Dialog a` --- an ordered
    sequence of dialog boxes, where the choice of the next box can depend on
    result of the previous box.

    `Free1` is a version with "at least one" `f a`.

    See the information later on `Free` alone (in the single-argument functor
    combinator section) for more information on usage and utility.

::: note
**Aside**

Let us pause for a brief aside to compare and contrast the hierarchy of the
above functor combinators, as there is an interesting progression we can draw
from them.

1.  `:+:`: Provide either, be ready for both.
2.  `:*:`: Provide both, be ready for either.
3.  `Day`: Provide both, be ready for both.
4.  `Comp`: Provide both, be ready for both (in order).
:::

### These1

-   **Origin**:
    *[Data.Functor.These](https://hackage.haskell.org/package/these/docs/Data-Functor-These.html)*.

-   **Mixing Strategy**: "Either-or, or both": provide either (or both) cases,
    and user has to handle both possibilities. An "inclusive either"

    ``` haskell
    data These1 f g a
        = This1  (f a)
        | That1        (g a)
        | These1 (f a) (g a)
    ```

    This can be useful for situations where your schema/functor can be specified
    using one functor or another, or even both. See description on `:+:` for
    examples.

    The person who creates the `These1 f g` decides which one to give, and the
    person who consumes/interprets/runs the `f :+: g` must provide a way of
    handling *both* situations.

    ``` haskell
    binterpret @These
        :: Alt f
        => (g ~> f)
        -> (h ~> f)
        -> These g h a
        -> f a
    ```

    You can also pattern match on the `These1` directly to be more explicit with
    how you handle each of the tree cases.

-   **Identity**

    ``` haskell
    instance Tensor These V1
    ```

    `These1 f V1` is equivalent to just `f`, because it means the `That1` and
    `These1` branches will be impossible to construct, and you are left with
    only the `This1` branch.

-   **Monoids**

    ``` haskell
    instance Alt f => SemigroupIn These1 f
    instance Alt f => MonoidIn    These1 V1 f

    binterpret @These
        :: Alt f
        => (g ~> f)
        -> (h ~> f)
        -> These g h ~> f

    inL   @These1 :: Alt g => f  ~> Comp f g
    inR   @These1 :: Alt f => g  ~> Comp f g
    pureT @These1 :: Alt h => V1 ~> h
    ```

    You need at least `Alt` to be able to interpret out of a `These1`, because
    you need to be able to handle the case where you have *both* `f` and `g`,
    and need to combine the result. However, you never need a full `Plus`
    because we always have at least one value to use.

-   **List type**

    ``` haskell
    type ListBy These1 = Steps
    ```

    `Steps`, the list type, is the result of an infinite application of `These1`
    to the same value:

    ``` haskell
    type Steps f = f `These1` f `These1` f `These1` f `These1` ... etc.

    -- actual implementation
    newtype Steps f a = Steps (NEMap Natural (f a))
                    -- NEMap is a non-empty Map
    ```

    It essentially represents an infinite *sparse* array of `f a`s, where an
    `f a` might exist at many different positions, with gaps here and there.
    There is always at least *one* `f a`.

    Like `Step`, it's not particularly useful, but it can be used in situations
    where you want a giant infinite sparse array of `f a`s, each at a given
    position, with many gaps between them.

    I've skipped over the the "non-empty" version, which is
    `ComposeT Flagged Steps`; it requires an extra boolean "flag" because of
    some of the quirks of nonemptiness. I feel it is even less useful than
    `Steps`.

### LeftF / RightF

-   **Origin**:
    *[Data.HBifunctor](https://hackage.haskell.org/package/functor-combinators/docs/Data-HBifunctor.html)*

-   **Mixing Strategy**: "Ignore the left" / "ignore the right".

    ``` haskell
    data LeftF  f g a = LeftF  { runLeftF  :: f a }

    data RightF f g a = RightF { runRightF :: g a }
    ```

    You can think of `LeftF` as "`:+:` without the Right case, `R1`", or
    `RightF` as "`:+:` without the Left case, `L1`". `RightF` can be considered
    a higher-order version of *Tagged*, which "tags" a value with some
    type-level information.

    This can be useful if you want the second (or first) argument to be ignored,
    and only be used maybe at the type level.

    For example, `RightF IgnoreMe MyFunctor` is equivalent to just `MyFunctor`,
    but you might want to use `IgnoreMe` as a phantom type to help limit what
    values can be used for what functions.

-   **Identity**

    Unlike the previous functor combinators, these three are only `Associative`,
    not `Tensor`: this is because there is no functor `i` such that `LeftF i g`
    is equal to `g`, for all `g`, and no functor `i` such that `RightF f i` is
    equal to `f`, for all `f`.

-   **Constraints**

    ``` haskell
    instance SemigroupIn LeftF  f
    instance SemigroupIn RightF f
    ```

    Interpreting out of either of these is unconstrained, and can be done in any
    context.

-   **List type**

    ``` haskell
    type NonEmptyBy LeftF = Flagged
    ```

    For `LeftF`, the non-empty list type is `Flagged`, which is the `f a` tupled
    with a `Bool`. See the information on `Flagged` for more details. This can
    be useful as a type that marks if an `f` is made with `inject`/`pure` and is
    "pure" (`False`), or "tainted" (`True`). The *provider* of a `Flagged` can
    specify "pure or tainted", and the *interpreter* can make a decision based
    on that tag.

    ``` haskell
    type NonEmptyBy RightF = Step
    ```

    For `RightF`, the non-empty list type is `Step`. See `Step` and the
    information on `:+:` for more details. This can be useful for having a value
    of `f a` at "some point", indexed by a `Natural`.

## Single-Argument

Unary functor combinators usually directly "enhance" a functor with extra
capabilities --- usually in the form of a typeclass instance, or extra data
fields/constructors.

All of these can be "lifted into" with any constraint on `f`.

``` haskell
class HFunctor t => Inject t where
    inject :: f ~> t f
```

`Inject` seems very similar to `MonadTrans`'s `lift`; the difference is that
`inject` must be *natural* on `f`: it can assume nothing about the structure of
`f`, and must work universally the same. `MonadTrans`, in contrast, requires
`Monad f`.

Each one can also be "interpreted to" certain functors `f`:

``` haskell
class Inject t => Interpret t f where
    interpret
        :: g ~> f
        -> t g ~> f
```

An important law is that:

``` haskell
interpret id . inject == id
```

This means that if we inject and immediately interpret out of, we should never
*lose* any information in `f`. All of the original structure in `f` must stay
intact: functor combinators only ever *add* structure.

### Coyoneda

-   **Origin**:
    *[Data.Functor.Coyoneda](https://hackage.haskell.org/package/kan-extensions/docs/Data-Functor-Coyoneda.html)*

-   **Enhancement**: The ability to map over the parameter; it's the free
    `Functor`.

    Can be useful if `f` is created using a `GADT` that cannot be given a
    `Functor` instance.

    For example, here is an indexed type that represents the type of a "form
    element", where the type parameter represents the output result of the form
    element.

    ``` haskell
    data FormElem :: Type -> Type where
        FInput    :: FormElem String
        FTextbox  :: FormElem Text
        FCheckbox :: FormElem Bool
        FNumber   :: FormElem Int
    ```

    Then `Coyoneda FormElem` has a `Functor` instance. We can now fmap over the
    result type of the form element; for example,
    `fmap :: (a -> b) -> Coyoneda FormElem a -> Coyoneda FormElem b` takes a
    form element whose result is an `a` and returns a form element whose result
    is a `b`.

-   **Interpret**

    ``` haskell
    instance Functor f => Interpret Coyoneda f

    interpret @Coyoneda
        :: Functor f
        => g ~> f
        -> Coyoneda g ~> f
    ```

    Interpreting out of a `Coyoneda f` requires the target context to itself be
    `Functor`. Usually, the context is an `Applicative` or `Monad`, so this is
    typically always satisfied.

    For example, if we want to "run" a `Coyoneda FormElem` in `IO` (maybe as an
    interactive CLI form), this would be
    `interpret :: (forall x. FormElem x -> IO x) -> Coyoneda FormElem a -> IO a`.

### ListF / NonEmptyF

-   **Origin**:
    *[Control.Applicative.ListF](https://hackage.haskell.org/package/functor-combinators/docs/Control-Applicative-ListF.html)*

-   **Enhancement**: The ability to offer multiple options for the interpreter
    to pick from; `ListF` is the free `Plus`, and `NonEmptyF` is the free `Alt`.

    ``` haskell
    data ListF     f a = ListF     { runListF     :: [f a]          }
    data NonEmptyF f a = NonEmptyF { runNonEmptyF :: NonEmpty (f a) }
    ```

    Can be useful if you want to provide the ability when you *define* your
    schema to provide multiple `f a`s that the *interpreter*/consumer can freely
    pick from.

    For example, for a schema specifying a form, you might have multiple ways to
    enter a name. If you had a `Name` schema `data Name a`, then you can
    represent "many different potential name inputs" schema as `ListF Name a`.

    Because this has a `Plus` instance, you can use
    `(<!>) :: ListF f a -> ListF f a -> ListF f a` to combine multiple option
    sets, and `zero :: ListF f a` to provide the "choice that always fails/is
    unusuable".

    `NonEmptyF` is a variety of `ListF` where you always have "at least one
    `f a`". Can be useful if you want to ensure, for your interpreter's sake,
    that you always have at least one `f a` option to pick from. For example,
    `NonEmptyF Name a` will always have at least *one* name schema.

    This is essentially `f` `:*:`d with itself multiple times; `ListF` is the
    linked list list made by `:*:`, and `NonEmptyF` is the non-empty linked list
    made by `:*:`.

    ``` haskell
    x             <=> ListF [x]     <=> NonEmptyF (x :| [])
    x :*: y       <=> ListF [x,y]   <=> NonEmptyF (x :| [y])
    x :*: y :*: z <=> ListF [x,y,z] <=> NonEmptyF (x :| [y,z])
    ```

-   **Interpret**

    ``` haskell
    instance Plus f => Interpret ListF     f
    instance Alt  f => Interpret NonEmptyF f

    interpret @ListF
        :: Plus f
        => g ~> f
        -> ListF g ~> f

    interpret @NonEmptyF
        :: Alt f
        => g ~> f
        -> NonEmptyF g ~> f
    ```

    Interpreting out of a `ListF f` requires the target context to be `Plus`,
    and interpreting out of a `NonEmptyF f` requires `Alt` (because you will
    never have the empty case). However, you always have the option to directly
    pattern match on the list and pick an item you want directly, which requires
    no constraint.

### Ap / Ap1

-   **Origin**:
    *[Control.Applicative.Free](https://hackage.haskell.org/package/free/docs/Control-Applicative-Free.html)*
    /
    *[Data.Functor.Apply.Free](https://hackage.haskell.org/package/functor-combinators/docs/Data-Functor-Apply-Free.html)*

-   **Enhancement**: The ability to provide multiple `f`s that the interpreter
    *must* consume *all* of; `Ap` is the free `Applicative`, and `Ap1` is the
    free `Apply`.

    While `ListF` may be considered "multiple options *offered*", `Ap` can be
    considered "multiple actions all *required*". The interpreter must
    consume/interpret *all* of the multiple `f`s in order to interpret an `Ap`.

    For example, for a form schema, you might want to have multiple form
    elements. If a single form element is `data FormElem a`, then you can make a
    multi-form schema with `Ap FormElem a`. The consumer of the form schema must
    handle *every* `FormElem` provided.

    Note that ordering is not enforced: while the consumer must handle each `f`
    eventually, they are free to handle it in whatever order they desire. In
    fact, they could even all be handled in parallel. See `Free` for a version
    where ordering is enforced.

    Because this has an `Applicative` instance, you can use
    `(<*>) :: Ap f (a -> b) -> Ap f a -> Ap f b` to sequence multiple `Ap f`s
    together, and `pure :: a -> Ap f a` to produce a "no-op" `Ap` without any
    `f`s.

    `Ap` has some utility over `Free` in that you can pattern match on the
    constructors directly and look at each individual sequenced `f a`, for
    static analysis, before anything is ever run or interpreted.

    *Structurally*, `Ap` is built like a linked list of `f x`s, which each link
    being existentially bound together:

    ``` haskell
    data Ap :: (Type -> Type) -> Type -> Type where
        Pure :: a   -> Ap f a
        Ap   :: f a -> Ap f (a -> b) -> Ap f b
    ```

    `Pure` is like "nil", and `Ap` is like "cons":

    ``` haskell
    data List :: Type -> Type where
        Nil  :: List a
        Cons :: a -> List a -> List a
    ```

    The existential type in the `Ap` branch plays the same role that it does in
    the definition of `Day` (see the description of `Day` for more information).

    `Ap1` is a variety of `Ap` where you always have to have "at least one `f`".
    Can be useful if you want to ensure, for example, that your form has at
    least one element.

    Note that this is essentially `f` `Day`d with itself multiple times; `Ap` is
    the linked list made by `Day` and `Ap1` is the non-empty linked list made by
    `Day`.

-   **Interpret**

    ``` haskell
    instance Applicative f => Interpret Ap  f
    instance Apply       f => Interpret Ap1 f

    interpret @Ap
        :: Applicative f
        => g ~> f
        -> Ap g ~> f

    interpret @Ap1
        :: Apply f
        => g ~> f
        -> Ap1 g ~> f
    ```

    Interpreting out of an `Ap f` requires the target context to be
    `Applicative`, and interpreting out of a `Ap1 f` requires `Apply` (because
    you will never need the pure case).

### Alt

-   **Origin**:
    *[Control.Alternative.Free](https://hackage.haskell.org/package/free/docs/Control-Alternative-Free.html)*

-   **Enhancement**: A combination of both `ListF` and `Ap`: provide a choice
    (`ListF`-style) of sequences (`Ap`-style) of choices of sequences of choices
    ....; it's the free `Alternative`.

    ``` haskell
    Alt f ~ ListF (Ap (ListF (Ap (ListF (Ap (...))))
          ~ ListF (Ap (Alt f))
    ```

    This type imbues `f` with both sequential "must use both" operations (via
    `<*>`) and choice-like "can use either" operations (via `<|>`).

    It can be useful for implementing parser schemas, which often involve both
    sequential and choice-like combinations. If `f` is a primitive parsing unit,
    then `Alt f` represents a non-deterministic parser of a bunch of `f`s one
    after the other, with multiple possible results. I wrote [an entire
    article](https://blog.jle.im/entry/free-alternative-regexp.html) on the
    usage of this combinator alone to implement a version of regular
    expressions.

-   **Interpret**

    ``` haskell
    instance Alternative f => Interpret Alt f

    interpret @Alt
        :: Alternative f
        => g ~> f
        -> Alt g ~> f
    ```

    Interpreting out of an `Alt f` requires the target context to be
    `Alternative` --- it uses `<*>` for sequencing, and `<|>` for choice.

### Free / Free1

-   **Origin**:
    *[Control.Monad.Freer.Church](https://hackage.haskell.org/package/functor-combinators/docs/Control-Monad-Freer-Church.html)*,
    which is a variant of
    *[Control.Monad.Free](https://hackage.haskell.org/package/free/docs/Control-Monad-Free.html)*
    that is compatible with `HFunctor`.

-   **Enhancement**: The ability to provide multiple `f`s that the interpreter
    must consume *in order*, sequentially --- the free `Monad`.

    Contrast with `Ap`, which also sequences multiple `f`s together, but without
    any enforced order. It does this by *hiding* the "next `f a`" until the
    previous `f a` has already been interpreted.

    Perhaps more importantly, you can sequence `f`s in a way where the *choice
    of the next `f`* is allowed to depend on the *result of the previous `f`*.

    For example, in an interactive "wizard" sort of schema, we can create a
    functor to represent a dialog box with its result type:

    ``` haskell
    data Dialog a
    ```

    We can then construct a type for a wizard:

    ``` haskell
    type Wizard = Free Dialog
    ```

    `Wizard` is now an ordered sequence of dialog boxes, where the choice of the
    next box can depend on result of the previous box. Contrast to `Ap Dialog`,
    where the choice of all dialog boxes must be made in advanced, up-front,
    before reading any input from the user.

    In having this, however, we loose the ability to be able to inspect each
    `f a` before interpreting anything.

    Because this has a `Monad` instance, you can use
    `(<*>) :: Free f (a -> b) -> Free f a -> Free f b` and
    `(>>=) :: Free f a -> (a -> Free f b) -> Free f b)` to sequence multiple
    `Free f`s together, and `pure :: a -> Free f a` to produce a "no-op" `Free`
    without any `f`s.

    `Free1` is a variety of `Free1` where you always have to have "at least one
    `f`". Can be useful if you want to ensure, for example, that your wizard has
    at least one dialog box.

    ``` haskell
    type NonEmptyWizard = Free1 Dialog
    ```

    Note that this is essentially `f` `Comp`d with itself multiple times; `Free`
    is the linked list made by `Comp` and `Free1` is the non-empty linked list
    made by `Comp`.

-   **Interpret**

    ``` haskell
    instance Monad f => Interpret Free  f
    instance Bind  f => Interpret Free1 f

    interpret @Free
        :: Monad f
        => g ~> f
        -> Free g ~> f

    interpret @Free1
        :: Bind f
        => g ~> f
        -> Free1 g ~> f
    ```

    Interpreting out of a `Free f` requires the target context to be `Monad`,
    and interpreting out of a `Free1 f` requires `Bind` (because you will never
    need the pure case).

### Lift / MaybeApply

-   **Origin**:
    *[Control.Applicative.Lift](https://hackage.haskell.org/package/transformers/docs/Control-Applicative-Lift.html)*
    /
    *[Data.Functor.Apply](https://hackage.haskell.org/package/semigroupoids/docs/Data-Functor-Apply.html)*
    (the same type)

-   **Enhancement**: Make `f` "optional" in the schema in a way that the
    interpreter can still work with as if the `f` was still there; it's the free
    `Pointed`.

    ``` haskell
    data Lift f a = Pure  a
                  | Other (f a)

    newtype MaybeApply f a = MaybeApply { runMaybeApply :: Either a (f a) }
        -- ^ same type, from semigroupoids
    ```

    Can be useful so that an `f a` is *optional* for the schema definition, but
    in a way where the consumer can still continue from it as if they *had* the
    `f`.

    It can be used, for example, to turn an required parameter `Param a` into an
    optional parameter `Lift Param a`.

    Contrast this to `MaybeF`: this allows the interpreter to still "continue
    on" as normal even if the `f` is not there. However, `MaybeF` forces the
    interpreter to abort if the `f` is not there.

    This can be thought of as `Identity :+: f`.

-   **Interpret**

    ``` haskell
    instance Pointed f => Interpret Lift f

    interpret @Lift
        :: Pointed f
        => g ~> f
        -> Lift g ~> f
    ```

    Interpreting out of a `Lift f` requires the target context to be `Pointed`,
    from
    *[Data.Pointed](https://hackage.haskell.org/package/pointed/docs/Data-Pointed.html)*
    --- it uses `point :: Pointed f => a -> f a` to handle the case where the
    `f` is not there.

### MaybeF

-   **Origin**:
    *[Control.Applicative.ListF](https://hackage.haskell.org/package/functor-combinators/docs/Control-Applicative-ListF.html)*

-   **Enhancement**: Make `f` "optional" in the schema in a way that the
    interpreter *must fail* if the `f` is not present.

    ``` haskell
    newtype MaybeF f a = MaybeF { runMaybeF :: Maybe (f a) }
    ```

    Can be useful so that an `f a` is *optional* for the schema definition; if
    the `f` is not present, the consumer must abort the current branch, or find
    some other external way to continue onwards.

    Contrast this to `Lift`, which is an "optional" `f` that the consumer may
    continue on from.

-   **Interpret**

    ``` haskell
    instance Plus f => Interpret MaybeF f

    interpret @MaybeF
        :: Plus f
        => g ~> f
        -> MaybeF g ~> f
    ```

    Interpreting out of a `Lift f` requires the target context to be `Plus` ---
    it uses `zero :: f a` to handle the case where the `f` is not there. Note
    that this is actually "over-constrained": we really only need `zero`, and
    not all of `Plus` (which includes `<!>`). However, there is no common
    typeclass in Haskell that provides this, so this is the most pragmatic
    choice.

### EnvT

-   **Origin**:
    *[Control.Comonad.Trans.Env](https://hackage.haskell.org/package/comonad/docs/Control-Comonad-Trans-Env.html)*

-   **Enhancement**: Provide extra (monoidal) data alongside `f a` that the
    interpreter can access. Basically tuples extra `e` alongside the `f a`.

    ``` haskell
    newtype EnvT e f a = EnvT e (f a)
    ```

    You can use this to basically tuple some extra data alongside an `f a`. It
    can be useful if you want to provide extra information that isn't inside the
    `f` for the interpreter use for interpretation.

    When using `inject :: Monoid e => f a -> EnvT e f a`, it uses `mempty` as
    the initial `e` value.

    One of my personal favorite uses of `EnvT` is the
    *[flare](https://github.com/sharkdp/purescript-flare)* purescript library,
    which uses the `e` as the observed HTML of a form, and the `f a` as an
    active way to get information from a form interactively. `inject` is used to
    insert an active form element without caring about its HTML representation,
    and `interpret` would "run" the active elements to get the results.

    This type exists specialized a few times here, as well:

    -   `Step` is `EnvT (Sum Natural)`
    -   `Flagged` is `EnvT Any`

```{=html}
<!-- -->
```
-   **Interpret**

    ``` haskell
    instance Interpret (EnvT e) f

    interpret @(EnvT e)
        :: g ~> f
        -> EnvT e g ~> f
    ```

    Interpreting out of `EnvT e` requires no constraints.

### MapF / NEMapF

-   **Origin**:
    *[Control.Applicative.ListF](https://hackage.haskell.org/package/functor-combinators/docs/Control-Applicative-ListF.html)*

-   **Enhancement**: Contain multiple `f a`s, each indexed at a specific *key*.

    ``` haskell
    newtype MapF   k f a = MapF   { runMapF :: Map   k (f a) }
    newtype NEMapF k f a = NEMapF { runMapF :: NEMap k (f a) }
    ```

    This is very similar in functionality to `ListF` and `NonEmptyF`, except
    instead of "positional" location, each `f a` exists at a given index.
    `NEMapF k` is the "non-empty" variant. You can think of this as a `ListF`
    plus `EnvT`: it's a "container" of multiple `f a`s, but each one exists with
    a given "tag" index `k`.

    In usage, like for `ListF`, the *definer* provides multiple "labeled"
    `f a`s, and the *interpreter* can choose to interpret some or all of them,
    with access to each labeled.

    `inject` creates a singleton `Map` at key `mempty`.

    This is very useful in schemas that have sub-schemas indexed at specific
    keys. For example, in a command line argument parser, if we have a functor
    that represents a single command:

    ``` haskell
    data Command a
    ```

    We can immediately promote it to be a functor representing *multiple
    possible* named commands, each at a given string:

    ``` haskell
    type Commands = MapF String Command
    ```

    So we can implement "git push" and "git pull" using:

    ``` haskell
    push :: Command Action
    pull :: Command Action

    gitCommands :: Commands Action
    gitCOmmands = MapF . M.fromList $
        [ ("push", push)
        , ("pull", pull)
        ]
    ```

    This is also useful for specifying things like routes in a server.

    This type exists specialized as `Steps`, which is `NEMapF (Sum Natural)`.

-   **Interpret**

    ``` haskell
    instance Plus f => Interpret (MapF  k) f
    instance Alt  f => Interpret (NEMap k) f

    interpret @(MapF k)
        :: Plus f
        => g ~> f
        -> MapF g ~> f

    interpret @(NEMapF k)
        :: Alt f
        => g ~> f
        -> NEMapF g ~> f
    ```

    Interpreting out of a `MapF k f` requires the target context to be `Plus`,
    and interpreting out of a `NEMapF k f` requires `Alt` (because you will
    never have the empty case). However, you can directly *look up* into the
    `Map` and pick an item you want directly, which requires no constraint.

### ReaderT

-   **Origin**:
    *[Control.Monad.Trans.Reader](https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Reader.html)*

-   **Enhancement**: Provide each `f a` with access to some "environment" `r`.

    ``` haskell
    newtype ReaderT r f a = ReaderT { runReaderT :: r -> f a }
    ```

    `ReaderT r` is often used to model some form of [dependency
    injection](https://en.wikipedia.org/wiki/Dependency_injection): it allows
    you to work "assuming" you had an `r`; later, when you *run* it, you provide
    the `r`. It delays the evaluation of your final result until you provide the
    missing `r`.

    Another way of looking at it is that it makes your entire functor have
    values that are *parameterized* with an `r`.

    For example, if you have a form data type:

    ``` haskell
    data FormElem a
    ```

    you can now make a form data type that is parameterized by the current
    server hostname:

    ``` haskell
    type FormElemWithHost = ReaderT HostName FormElem
    ```

    The actual structure of your `FormElem` is deferred until you provide the
    `HostName`.

    Note that, unlike `ReaderT`, most monad transformers from *transformers* are
    actually *not* valid functor combinators under our perspective here, because
    most of them are not *natural* on `f`: they require `Functor f`, at least,
    to implement `inject` or `hmap`.

-   **Interpret**

    ``` haskell
    instance MonadReader r f => Interpret (ReaderT r) f

    interpret @(ReaderT r)
        :: MonadReader r f
        => g ~> f
        -> ReaderT r g ~> f
    ```

    Interpreting out of a `ReaderT r` requires requires the target context to be
    `MonadReader r`, which means it must have access to
    `ask :: MonadReader r f => f r`.

    In a way, `ReaderT r` is the "free" instance of `MonadReader r`.

### Step

-   **Origin**:
    *[Control.Applicative.Step](https://hackage.haskell.org/package/functor-combinators/docs/Control-Applicative-Step.html)*

-   **Enhancement**: Tuples the `f a` with an extra natural number index.

    ``` haskell
    data Step f a = Step { stepPos :: Natural, stepVal :: f a }
    ```

    This is essentially a specialized `EnvT`: it's `EnvT (Sum Natural)`.

    This is a useful type because it can be seen as equivalent to
    `f :+: f :+: f :+: f :+: f ...` forever: it's an `f`, but at some index. In
    *[Control.Applicative.Step](https://hackage.haskell.org/package/functor-combinators/docs/Control-Applicative-Step.html)*,
    we have specialized functions `stepUp` and `stepDown`, which allows you to
    "match" on the "first" `f` in that infinite chain; it will increment and
    decrement the index relatively to make this work properly.

-   **Interpret**

    ``` haskell
    instance Interpret Step f

    interpret @Step
        :: g ~> f
        -> Step g ~> f
    ```

    Interpreting out of `Step` requires no constraints; we just drop the
    `Natural` data.

### Steps

-   **Origin**:
    *[Control.Applicative.Step](https://hackage.haskell.org/package/functor-combinators/docs/Control-Applicative-Step.html)*

-   **Enhancement**: The ability to offer multiple *indexed* options for the
    interpreter to pick from. Like `NonEmptyF`, except with each `f a` existing
    at an indexed position that the consumer/interpreter can look up or access.

    ``` haskell
    newtype Steps f a = Steps { getSteps :: NEMap Natural (f a) }
    ```

    This is like a mix between `NonEmptyF` and `Step`: multiple `f a` options
    (at least one) for the consumer/interpreter to pick from. Unlike
    `NonEmptyF`, each `f a` exists at an "index" --- there might be one at 0,
    one at 5, one at 100, etc.

    Another way of looking at this is like an infinite *sparse array* of `f a`s:
    it's an inifinitely large collection where each spot may potentially have an
    `f a`.

    Useful for "provide options that the consumer can pick from, index, or
    access", like `ListF`/`NonEmptyF`.

    This type can be seen as an infinite
    `` f `These1` f `These1` f `These1` f ... ``, and along these lines,
    `stepsDown` and `stepsUp` exist inside
    *[Control.Applicative.Step](https://hackage.haskell.org/package/functor-combinators/docs/Control-Applicative-Step.html)*
    analogous to `stepUp` and `stepDown` to treat a `Steps` in this manner.

-   **Interpret**

    ``` haskell
    instance Alt f => Interpret Steps f

    interpret @Steps
        :: Alt f
        => g ~> f
        -> Steps g ~> f
    ```

    Interpreting out of `Steps` requires an `Alt` to combine different
    possibilities. It does not require a full `Plus` constraint because we never
    need `zero`: a `Steps f a` always has at least one `f a`.

### Flagged

-   **Origin**:
    *[Control.Applicative.Step](https://hackage.haskell.org/package/functor-combinators/docs/Control-Applicative-Step.html)*

-   **Enhancement**: The ability to "tag" a functor value with a `True`/`False`
    boolean value.

    ``` haskell
    data Flagged f a = Flagged { flaggedFlag :: Bool, flaggedVal :: f a }
    ```

    This is essentially a specialized `EnvT`: it's `EnvT Any`.

    If created with `inject` or `pure`, it adds the flag `False`. This is
    helpful for helping indicate if the value was created using a "pure" method
    like `inject` or `pure`, or an "impure" method (any other method, including
    direct construction).

-   **Interpret**

    ``` haskell
    instance Interpret Flagged f

    interpret @Flagged
        :: g ~> f
        -> Flagged g ~> f
    ```

    Interpreting out of `Flagged` requires no constraints; we just drop the
    boolean flag.

### Final

-   **Origin**:
    *[Data.HFunctor.Final](https://hackage.haskell.org/package/functor-combinators/docs/Data-HFunctor-Final.html)*

-   **Enhancement**: `Final c` will lift `f` into a free structure of any
    typeclass `c`; it will give it all of the actions/API of a typeclass for
    "free". `Final c f` is the "free `c`" over `f`.

    ``` haskell
    data Final c f a
    ```

    In a way, this is the "ultimate free structure": it can fully replace all
    other free structures of typeclasses of kind `Type -> Type`. For example:

    ``` haskell
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
    `FreeOf` in
    *[Data.HFunctor.Final](https://hackage.haskell.org/package/functor-combinators/docs/Data-HFunctor-Final.html)*.

    In fact, `Final c` is often more performant for many operations than the
    actual concrete free structures.

    The main downside is that you cannot directly pattern match on the structure
    of a `Final c` the same way you can pattern match on, say, `Ap` or `ListF`.
    However, you can get often around this by using `Final Plus` for most of
    your operations, and then `interpret inject`-ing it into `ListF` when you
    want to actually pattern match.

    You can also think of this as the "ultimate `Interpret`", because with
    `inject` you can push `f` into `Final c f`, and with `interpret` you only
    ever need the `c` constraint to "run"/interpret this.

    So, next time you want to give an `f` the ability to `<*>` and `pure`, you
    can throw it into `Final Applicative`: `f` now gets "sequencing" abilities,
    and is equivalent to `Ap f`.

    If you want the API of a given typeclass `c`, you can inject `f` into
    `Final c`, and you get the API of that typeclass for free on `f`.

-   **Constraint**

    ``` haskell
    instance c f => Interpret (Final c) f

    interpret @(Final c)
        :: c f
        => g ~> f
        -> Final c g ~> f
    ```

    Interpreting out of a `Final c` requires `c`, since that is the extra
    context that `f` is lifted into.

### Chain / Chain1

-   **Origin**:
    *[Data.HFunctor.Chain](https://hackage.haskell.org/package/functor-combinators/docs/Data-HFunctor-Chain.html)*

-   **Enhancement**: `Chain t` will lift `f` into a linked list of `f`s chained
    by `t`.

    ``` haskell
    -- i is intended to be the identity of t
    data Chain t i f a = Done (i a)
                       | More (t f (Chain t i f a))
    ```

    For example, for `:*:`, `Chain (:*:) Proxy f` is equivalent to one of:

    ``` haskell
    Proxy   <=> Done Proxy                           <=> ListF []
    x       <=> More (x :*: Done Proxy)              <=> ListF [x]
    x :*: y <=> More (x :*: More (y :*: Done Proxy)) <=> ListF [x,y]
    -- etc.
    ```

    For `:+:`, `Chain (:+:) V1 f` is equivalent to one of:

    ``` haskell
    L1 x           <=> More (L1 x)                         <=> Step 0 x
    R1 (L1 y)      <=> More (R1 (More (L1 y)))             <=> Step 1 y
    R1 (R1 (L1 z)) <=> More (R1 (More (R1 (More (L1 z))))) <=> Step 2 z
    -- etc.
    ```

    This is useful because it provides a nice uniform way to work with all
    "linked list over tensors". That's because the following types are all
    isomorphic:

    ``` haskell
    ListF ~ Chain (:*:)  Proxy
    Ap    ~ Chain Day    Identity
    Free  ~ Chain Comp   Identity
    Step  ~ Chain (:+:)  Void
    Steps ~ Chain These1 Void
    ```

    This isomorphism is witnessed by `unroll` (turn into the `Chain`) and
    `reroll` (convert back from the `Chain`) in
    *[Data.HFunctor.Chain](https://hackage.haskell.org/package/functor-combinators/docs/Data-HFunctor-Chain.html)*.

    We can "fold down" a `Chain t (I t) f a` into an `f a`, if `t` is
    `Monoidal`, using `interpret id`. In fact, this ability could be used as a
    fundamental property of monoidal nature.

    We also have a "non-empty" version, `Chain1`, for non-empty linked lists
    over tensors:

    ``` haskell
    data Chain1 t f a = Done1 (f a)
                      | More1 (t f (Chain1 t f a))
    ```

    ``` haskell
    NonEmptyF ~ Chain1 (:*:)
    Ap1       ~ Chain1 Day
    Free1     ~ Chain1 Comp
    Step      ~ Chain1 (:+:)
    Steps     ~ Chain1 These1
    EnvT Any  ~ Chain1 LeftF
    Step      ~ Chain1 RightF
    ```

    We can "fold down" a `Chain1 t f a` into an `f a`, if `t` is
    `Semigroupoidal`, using `interpret id`. In fact, this ability could be used
    as a fundamental property of semigroupoidal nature.

    Using `ListF`, `Ap`, `Free`, `Step`, `Steps`, etc. can sometimes feel very
    different, but with `Chain` you get a uniform interface to pattern match on
    (and construct) all of them in the same way.

    Using `NonEmptyF`, `Ap1`, `Free1`, `Step`, `Flagged`, etc. can sometimes
    feel very different, but with `Chain1` you get a uniform interface to
    pattern match on (and construct) all of them in the same way.

    Universally, we can concatenate linked chains, with:

    ``` haskell
    appendChain
        :: Tensor t i
        => t (Chain t i f) (Chain t i f) ~> Chain t i f

    appendChain1
        :: Associative t
        => t (Chain1 t f) (Chain1 t f) ~> Chain1 t f
    ```

    These operations are associative, and this property is gained from the
    tensor nature of `t`.

    The construction of `Chain` is inspired by [Oleg Grenrus's blog
    post](http://oleg.fi/gists/posts/2018-02-21-single-free.html), and the
    construction of `Chain1` is inspired by implementations of finite automata
    and iteratees.

-   **Interpret**

    ``` haskell
    instance MonoidIn    t i f => Interpret (Chain  t i) f
    instance SemigroupIn t   f => Interpret (Chain1 t  ) f

    interpret @(Chain t i)
        :: MonoidIn t i f
        => g ~> f
        -> Chain t i g ~> f

    interpret @(Chain1 t)
        :: SemigroupIn t f
        => g ~> f
        -> Chain1 t g ~> f
    ```

    Interpreting out of a `Chain` requires only that `f` is a monoid in `t`.
    Interpreting out of a `Chain1` requires only that `f` is a semigroup in `t`.

    For example, we have:

    ``` haskell
    instance Plus f => Interpret (Chain  (:*:) Proxy) f
    instance Alt  f => Interpret (Chain1 (:*:)      ) f

    interpret @(Chain (:*:) Proxy)
        :: Plus f
        => g ~> f
        -> Chain (:*:) Proxy g ~> f

    interpret @(Chain1 (:*:))
        :: Alt f
        => g ~> f
        -> Chain1 (:*:) f ~> f

    instance Applicative f => Interpret (Chain  Day Identity) f
    instance Apply       f => Interpret (Chain1 Day         ) f

    instance Monad f => Interpret (Chain  Comp Identity) f
    instance Bind  f => Interpret (Chain1 Comp         ) f
    ```

### IdentityT

-   **Origin**:
    *[Data.Functor.Identity](https://hackage.haskell.org/package/base/docs/Data-Functor-Identity.html)*

-   **Enhancement**: None whatsoever; it adds no extra structure to `f`, and
    `IdentityT f` is the same as `f`; it's the "free `Unconstrained`"

    ``` haskell
    data IdentityT f a = IdentityT { runIdentityT :: f a }
    ```

    This isn't too useful on its own, but it can be useful to give to the
    functor combinator combinators as a no-op functor combinator. It can also be
    used to signify "no structure", or as a placeholder until you figure out
    what sort of structure you want to have.

    In that sense, it can be thought of as a "`ListF` with always one item", a
    "`MaybeF` that's always `Just`"', an "`Ap` with always one sequenced item",
    a "`Free` with always exactly one layer of effects", etc.

-   **Constraint**

    ``` haskell
    instance Interpret IdentityT f

    interpret @IdentityT
        :: g ~> f
        -> IdentityT g ~> f
    ```

    Interpreting out of `IdentityT` requires no constraints --- it basically
    does nothing.

### ProxyF / ConstF

-   **Origin**:
    *[Data.HFunctor](https://hackage.haskell.org/package/functor-combinators/docs/Data-HFunctor.html)*

-   **Enhancement**: "Black holes" --- they completely forget all the structure
    of `f`, and are impossible to `interpret` out of. `Impossible`".

    ``` haskell
    data ProxyF f a = ProxyF
    data ConstF e f a = ConstF e
    ```

    `ProxyF` is essentially `ConstF ()`.

    These are both valid functor combinators in that you can inject into them,
    and `interpret id . inject == id` is *technically* true (the best kind of
    true).

    You can use them if you want your schema to be impossible to interpret, as a
    placeholder or to signify that one branch is uninterpretable. In this sense,
    this is like a "`ListF` that is always empty" or a "`MaybeF` that is always
    `Nothing`".

    Because of this, they aren't too useful on their own --- they're more useful
    in the context of swapping out and combining or manipulating with other
    functor combinators or using with functor combinator combinators.

-   **Interpret**

    You're not going to have any luck here --- you cannot interpret out of
    these, unfortunately!

## Contravariant Functor Combinators

**Addendum: Post functor-combinators-0.3.0.0**

Most of the above functor combinators have been "covariant" ones: an `t f a`
represents some "producer" or "generator" of `a`s. Many of them require a
`Functor` constraint on `f` interpret out of. However, there exist many useful
*contravariant* ones too, where `t f a` represents a "consumer" of `a`s; many of
these require a `Contravariant` constraint on `f` to interpret out of. These can
be useful as the building blocks of consumers like serializers.

I've included them all in a separate section because you to either be looking
for one or the other, and also because there are much less contravariant
combinators than covariant ones in the Haskell ecosystem.

Also note that many of the functor combinators in the previous sections are
compatible with both covariant *and* contravariant functors, like:

-   `:+:`/`Sum`
-   `LeftF`/`RightF`
-   `EnvT`
-   `Step`
-   `Flagged`
-   `Final`
-   `Chain`
-   `IdentityT`

The following functor combinators in the previous section are also compatible
with both, but their instances in *functor-combinator* are designed around
covariant usage. However, some of them have contravariant twins that are
otherwise identical except for the fact that their instances are instead
designed around contravariant usage.

-   `:*:`/`Product` (contravariant version: the contravariant `Day`)
-   `These1`
-   `ListF`/`NonEmptyF` (contravariant versions: `Div` and `Div1`)
-   `MaybeF`

This section was added following the release of *functor-combinators-0.3.0.0*,
which added in support for contravariant and invariant functor combinators.

### Contravariant Day

-   **Origin**:
    *[Data.Functor.Contravariant.Day](https://hackage.haskell.org/package/kan-extensions/docs/Data-Functor-Contravariant-Day.html)*

-   **Mixing Strategy**: "Both, together": provide two consumers that are each
    meant to consume one part of the input.

    ``` haskell
    data Day f g a = forall x y. Day (f x) (g y) (a -> (x, y))
    ```

    This type is essentially equivalent to `:*:`/`Product` if `f` is
    `Contravariant`, so it is useful in every situation where `:*:` would be
    useful. It can be thought of as simply a version of `:*:` that signals to
    the reader that it is meant to be used contravariantly (as a consumer) and
    not covariantly (as a producer).

    Like for `:*:`, it has the distinguishing property (if `f` is
    `Contravariant`) of allowing you to use either the `f` or the `g`, as you
    please.

    ``` haskell
    dayOutL :: Contravariant f => Day f g ~> f
    dayOutL (Day x _ f) = contramap (fst . f) x

    dayOutR :: Contravariant g => Day f g ~> g
    dayOutR (Day _ y f) = contramap (snd . f) y
    ```

    In practice, however, I like to think of it as storing an `f` and a `g` that
    can each handle a separate "part" of an `a`. For example, the illustrative
    helper function

    ``` haskell
    day :: f a -> g b -> Day f g (a, b)
    day x y = Day x y id
    ```

    allows you to couple an `f a` consumer of `a` with a `g b` consumer of `b`
    to produce a consumer of `(a, b)` that does its job by handing the `a` to
    `x`, and the `b` to `y`.

-   **Identity**

    ``` haskell
    instance Tensor Day Proxy
    ```

    Since this type is essentially `(:*:)`, it has the same identity.

    ``` haskell
    day Proxy :: g b -> Day Proxy g (a, b)
    ```

    is the `Day` that would "ignore" the `a` part and simply pass the `b` to
    `g`.

-   **Monoids**

    ``` haskell
    instance Divise    f => SemigroupIn Day f
    instance Divisible f => MonoidIn    Day Proxy f

    binterpret @Day
        :: Divise f
        => g ~> f
        -> h ~> f
        -> Day g h ~> f

    inL   @(:*:) :: Divisible g => f     ~> Day f g
    inR   @(:*:) :: Divisible f => g     ~> Day f g
    pureT @(:*:) :: Divisible h => Proxy ~> h
    ```

    `Divise` from
    *[Data.Functor.Contravariant.Divise](https://hackage.haskell.org/package/functor-combinators/docs/Data-Functor-Contravariant-Divise.html)*
    can be thought of some version the "contravariant `Alt`": it gives you a way
    to merge two `f a`s into a single one in a way that represents having both
    the items consume the input as they choose. The usual way of doing this is
    by providing a splitting function to choose to give some part of the input
    to one argument, and some part to another:

    ``` haskell
    class Contravariant f => Divise f where
        divise :: (a -> (b, c)) -> f b -> f c -> f a
                      -- ^ what to give to the 'f b'
                         -- ^ what to give to the 'f c'
    ```

    `Divisible` from
    *[Data.Functor.Contravariant.Divisible](https://hackage.haskell.org/package/contravariant/docs/Data-Functor-Contravariant-Divisible.html)*,
    adds an identity that will ignore anything it is given: `conquer.`

    ``` haskell
    class Divise f => Divisible f where
        conquer :: f a
    ```

    (note: like with `Applicative` and `Apply`, the actual version requires only
    `Contravariant f`; `Divise` isn't an actual superclass, even though it
    should be.)

-   **List type**

    Basically,

    ``` haskell
    type NonEmptyBy Day = NonEmptyF
    type ListBy     Day = ListF
    ```

    Because the contravariant `Day` is equivalent to `:*:` for contravariant
    inputs, they have the exact same "list type". However, in the
    *functor-combinators* library, each list type can only have a single
    `Interpret` instance, so instead the list types are defined to be a separate
    (identical) type with a different name:

    ``` haskell
    type NonEmptyBy Day = Div1
    type ListBy     Day = Div
    ```

    Like for `Day`, it's something that can be used instead of `:*:` to mentally
    signify how the type is meant to be used. You can think of `Div f a` as a
    chain of `f`s, where the `a` is distributed over each `f`, but the intent of
    its usage is that each `f` is meant to consume a different part of that `a`.

    See the information later on `Div` alone for more information on usage and
    utility.

    `Div` is the possibly-empty version, and `Div1` is the nonempty version.

### Night

-   **Origin**:
    *[Data.Functor.Contravariant.Night](https://hackage.haskell.org/package/functor-combinators/docs/Data-Functor-Contravariant-Night.html)*

-   **Mixing Strategy**: "One or the other, but chosen at consumption-time":
    provide two consumers to handle input, but the choice of which consumer to
    use is made at consumption time.

    ``` haskell
    data Night f g a = forall x y. Night (f x) (g y) (a -> Either x y)
    ```

    This one represents *delegation*: `Night f g a` contains `f` and `g` that
    could process some form of the `a`, but which of the two is chosen to
    depends on the value of `a` itself.

    This can be thought of as representing
    [sharding](https://en.wikipedia.org/wiki/Shard_(database_architecture))
    between `f` and `g`. Some discriminator determins which of `f` or `g` is
    better suited to consume the input, and picks which single one to use based
    on that.

    The illustrative helper function can make this clear:

    ``` haskell
    night :: f a -> g b -> Night f g (Either a b)
    night x y = Night x y id
    ```

    allows you to couple an `f a` consumer of `a` with a `g b` consumer of `b`
    to produce a consumer of `Either a b` that does its job by using the `f` if
    given a `Left` input, and using the `g` if given a `b` input.

    This is technically still a day convolution (mathematically), but it uses
    `Either` instead of the typical `(,)` we use in Haskell. So it's like the
    opposite of a usual Haskell `Day` --- it's `Night` :)

-   **Identity**

    ``` haskell
    instance Tensor Night Not

    -- | Data type that proves @a@ cannot exist
    newtype Not a = Not { refute :: a -> Void }
    ```

    If `Night f g` assigns input to either `f` or `g`, then a functor that
    "cannot be chosen"/"cannot be used" would force the choice to the other
    side.

    That is, `Night f Not` must necessarily pass its input to `f`, as you cannot
    pass anything to a `Not`, since it only accepts passing in uninhabited
    types.

-   **Monoids**

    ``` haskell
    instance Decide   f => SemigroupIn Night f
    instance Conclude f => MonoidIn    Night Not f

    binterpret @Night
        :: Decide f
        => g ~> f
        -> h ~> f
        -> Night g h ~> f

    inL   @Night :: Conclude g => f   ~> Night f g
    inR   @Night :: Conclude f => g   ~> Night f g
    pureT @Night :: Conclude h => Not ~> h
    ```

    `Decide` from
    *[Data.Functor.Contravariant.Decide](https://hackage.haskell.org/package/contravariant/docs/Data-Functor-Contravariant-Decide.html)*
    can be thought of as a deterministic sharding typeclass: You can combine two
    consumers along with a decision function on which consumer to use.

    ``` haskell
    class Contravariant f => Decide f where
        decide :: (a -> Either b c) -> f b -> f c -> f a
                            -- ^ use the f b
                              -- ^ use the f c
    ```

    `Conclude` from
    *[Data.Functor.Contravariant.Conclude](https://hackage.haskell.org/package/functor-combinators/docs/Data-Functor-Contravariant-Conclude.html)*,
    adds support for specifying an `f` that cannot be chosen by the decision
    function when used with `decide`.

    ``` haskell
    class Decide f => Conclude f where
        conclude :: (a -> Void) -> f a
    ```

-   **List type**

    ``` haskell
    type NonEmptyBy Night = Dec1
    type ListBy     Night = Dec
    ```

    `Dec f` and `Dec1 f` represent a bunch of `f`s `Night`'d with each other ---
    you can think of `Dec f` was the sharding over many different `f`s (or even
    none), and `Dec1 f` as the sharding over at least one `f`.

    See the later section on `Dec` for more information.

### Contravariant Coyoneda

-   **Origin**:
    *[Data.Functor.Contravariant.Coyoneda](https://hackage.haskell.org/package/kan-extensions/docs/Data-Functor-Contravariant-Coyoneda.html)*

-   **Enhancement**: The ability to contravariantly map over the parameter; it's
    the free `Contravariant`.

    Can be useful if `f` is created using a `GADT` that cannot be given a
    `Contravariant` instance.

    For example, here is an indexed type that represents the type of a
    "prettyprinter", where the type parameter represents the type that is being
    pretty-printed output result of the form element.

    ``` haskell
    data PrettyPrim :: Type -> Type where
        PPString  :: PrettyPrim String
        PPInt     :: PrettyPrim Int
        PPBool    :: PrettyPrim Bool
    ```

    Then `Coyoneda PrettyPrim` has a `Contravariant` instance. We can now
    contramap over the input type of the pretty-printer; for example,
    `contramap :: (a -> b) -> Coyoneda PrettyPrim b -> Coyoneda PrettyPrim a`
    takes a prettyprinter of `b`s and turns it into a prettyprinter of `a`s.

-   **Interpret**

    ``` haskell
    instance Contravariant f => Interpret Coyoneda f

    interpret @Coyoneda
        :: Contravariant f
        => g ~> f
        -> Coyoneda g ~> f
    ```

    Interpreting out of a `Coyoneda f` requires the target context to itself be
    `Contravariant`. For example, if we want to "run" a `Coyoneda PrettyPrim` in
    `Op String` (`Op String a` is a function from `a` to `String`), this would
    be
    `interpret :: (forall x. PrettyPrim x -> Op String x) -> Coyoneda PrettyPrim a -> Op String a`.

### Div / Div1

-   **Origin**:
    *[Data.Functor.Contravariant.Divisible.Free](https://hackage.haskell.org/package/functor-combinators-0.3.2.0/docs/Data-Functor-Contravariant-Divisible-Free.html)*

-   **Enhancement**: The ability to provide multiple `f`s to each consume a part
    of the overall input.

    If `f x` is a consumer of `x`s, then `Div f a` is a consumer of `a`s that
    does its job by splitting `a` across /all/ `f`s, forking them out in
    parallel. Often times, in practice, this will utilized by giving each `f` a
    separate part of the `a` to consume.

    For example, let's say you had a type `Socket a` which represents some IO
    channel or socket that is expecting to receive `a`s. A `Div Socket b` would
    be a collection of sockets that expects a single `b` overall, but each
    individual `Socket` inside that `Div` is given some part of the overall `b`.

    Another common usage is to combine serializers by assigning each serializer
    `f` to one part of an overall input.

    *Structurally*, `Div` and `Div1` are basically lists of contravariant
    coyonedas:

    ``` haskell
    newtype Div  f a = Div  { unDiv  :: [Coyoneda f a]          }
    newtype Div1 f a = Div1 { unDiv1 :: NonEmpty (Coyoneda f a) }
    ```

    This could be implemented as simply a normal `[f a]` and `NonEmpty (f a)`
    (and so making them identical to `ListF`). For the most part, you could use
    the two interchangely, except in the case where you need to `Interpret` out
    of them: `ListF` requires a `Plus` constraint, and `Div` requires a
    `Divisible` constraint. The `Coyoneda` is also necessary for compatibility
    with the version of the contravariant `Day` convolution provided by
    *kan-extensions*.

    `Div1` is a variety of `Div` where you always have to have "at least one
    `f`". Can be useful if you want to ensure, for example, that *at least one
    socket* will be handling the input (and it won't be lost into the air).

-   **Interpret**

    ``` haskell
    instance Divisible f => Interpret Div  f
    instance Divise    f => Interpret Div1 f

    interpret @Div
        :: Divisible f
        => g ~> f
        -> Div g ~> f

    interpret @Div1
        :: Divise f
        => g ~> f
        -> Div1 g ~> f
    ```

    Interpreting out of an `Div f` requires the target context to be
    `Divisible`, and interpreting out of a `Div1 f` requires `Divise` (because
    you will never need the empty case).

### Dec / Dec1

-   **Origin**:
    *[Data.Functor.Contravariant.Divisible.Free](https://hackage.haskell.org/package/functor-combinators-0.3.2.0/docs/Data-Functor-Contravariant-Divisible-Free.html)*

-   **Enhancement**: The ability to provide multiple `f`s, one of which will be
    chosen to consume the overall input.

    If `f x` is a consumer of `x`s, then `Dec f a` is a consumer of `a`s that
    does its job by choosing a single one of those `f`s to handle that
    consumption, based on what `a` is received.

    Contrast this with `Div`, where the multiple `f` actions are *all* used to
    consume the input. `Dec` only uses *one single* `f` action to consume the
    input, chosen at consumption time.

    For example, let's say you had a type `Socket a` which represents some IO
    channel or socket that is expecting to receive `a`s. A `Dec Socket b` would
    be a collection of sockets that expects a single `b` overall, and will pick
    exactly one of those `Socket`s to handle that `b`.

    In this sense, you can sort of think of `Dec` as a "sharding" of `f`s: each
    `f` handles a different possible categorization of the input.

    Another common usage is to combine serializers by assigning each serializer
    `f` to one possible form of possible input.

    *Structurally*, `Dec` is built like a linked list of `f x`s, which each link
    being existentially bound together:

    ``` haskell
    data Dec :: (Type -> Type) -> Type -> Type where
        Lose   :: (a -> Void) -> Dec f a
        Choose :: f x -> Dec f y -> (a -> Either x y) -> Dec f a
    ```

    This is more or less the same construction as for `Ap`: see information on
    `Ap` for a deeper explanation on how or why this works.

    `Dec1` is a variety of `Dec` where you always have to have "at least one
    `f`". Can be useful if you want to ensure, for example, that there always
    exists at least one `f` that can handle the job.

-   **Interpret**

    ``` haskell
    instance Conclude f => Interpret Dec  f
    instance Decide   f => Interpret Dec1 f

    interpret @Dec
        :: Conclude f
        => g ~> f
        -> Dec g ~> f

    interpret @Dec1
        :: Decide f
        => g ~> f
        -> Dec1 g ~> f
    ```

    Interpreting out of an `Dec f` requires the target context to be `Conclude`,
    and interpreting out of a `Dec1 f` requires `Decide` (because you will never
    need the rejecting case).

## Combinator Combinators

There exist higher-order functor combinator combinators that take functor
combinators and return new ones, too. We can talk about a uniform interface for
them, but they aren't very common, so it is probably not worth the extra
abstraction.

### ComposeT

-   **Origin**:
    *[Control.Monad.Trans.Compose](https://hackage.haskell.org/package/mmorph/docs/Control-Monad-Trans-Compose.html)*

-   **Enhancement**: Compose enhancements from two different functor combinators

    ``` haskell
    newtype ComposeT s t f a = ComposeT { getComposeT :: s (t f) a }
    ```

    Can be useful if you want to layer or nest functor combinators to get both
    enhancements as a *single* functor combinator\*.

    Usually really only useful in the context of other abstractions that expect
    functor combinators, since this is the best way to turn two functor
    combinators into a third one.

-   **Interpret**

    ``` haskell
    instance (Interpret s f, Interpret t f) => Interpret (ComposeT s t) f

    interpret @(ComposeT s t)
        :: (Interpret s f, Interpret t f)
        => g ~> f
        -> ComposeT s t g ~> f
    ```

    Interpreting out of these requires the constraints on *both* layers.

### HLift

-   **Origin**:
    *[Data.HFunctor](https://hackage.haskell.org/package/functor-combinators/docs/Data-HFunctor.html)*

-   **Enhancement**: `HLift t f` lets `f` exist either unchanged, or with the
    structure of `t`.

    ``` haskell
    data HLift t f a
        = HPure  (f a)
        | HOther (t f a)
    ```

    Can be useful if you want to "conditionally enhance" `f`. Either `f` can be
    enhanced by `t`, or it can exist in its pure "newly-injected" form.

    If `t` is `Identity`, we get `EnvT Any`, or `f :+: f`: the "pure or impure"
    combinator.

-   **Interpret**

    ``` haskell
    instance Interpret t f => Interpret (HLift t) f

    interpret @(HLift t)
        :: Interpret t f
        => g ~> f
        -> HLift t g ~> f
    ```

    Interpreting out of these requires the constraint on `t`, to handle the
    `HOther` case.

### HFree

-   **Origin**:
    *[Data.HFunctor](https://hackage.haskell.org/package/functor-combinators/docs/Data-HFunctor.html)*

-   **Enhancement**: `HFree t f` lets `f` exist either unchanged, or with
    multiple nested enhancements by `t`.

    ``` haskell
    data HFree t f a
        = HReturn (f a)
        | HJoin   (t (HFree t f) a)
    ```

    It is related to `HLift`, but lets you lift over arbitrary many compositions
    of `t`, enhancing `f` multiple times. This essentially creates a "tree" of
    `t` branches.

    One particularly useful functor combinator to use is `MapF`. In our earlier
    examples, if we have

    ``` haskell
    data Command a
    ```

    to represent the structure of a single command line argument parser, we can
    use

    ``` haskell
    type Commands = MapF String Command
    ```

    to represent *multiple* potential named commands, each under a different
    `String` argument. With `HFree`, we can also use:

    ``` haskell
    type CommandTree = HFree (MapF String) Command
    ```

    to represent *nested* named commands, where each nested sub-command is
    descended on by a `String` key.

    For another example, `HFree IdentityT` is essentially `Step`.

-   **Interpret**

    ``` haskell
    instance Interpret t f => Interpret (HFree t) f

    interpret @(HFree t)
        :: Interpre t f
        => g ~> f
        -> HFree t g ~> f
    ```

    Interpreting out of these requires the constraint on `t`, to handle the
    `HJoin` case.

    However, it is probably usually more useful to directly pattern match on
    `HReturn` and `HJoin` and handle the recursion explicitly.

    Alternatively, we can also define a recursive folding function (provided in
    *[Data.HFunctor](https://hackage.haskell.org/package/functor-combinators/docs/Data-HFunctor.html)*)
    to recursively fold down each branch:

    ``` haskell
    foldHFree
        :: HFunctor t
        => (g ~> f)
        -> (t g ~> f)
        -> HFree t g ~> f
    ```

    This can be useful because it allows you to distinguish between the
    different branches, and also requires no constraint on `g`.

    Applied to the `CommandTree` example, this becomes:

    ``` haskell
    foldHFree @(MapF String) @Command
        :: Command ~> f
        -> MapF String ~> f
        -> CommandTree ~> f
    ```

## Closing Comments

As I discover more interesting or useful functor combinators (or as the
abstractions in *functor-combinators* change), I will continue to update this
post. And, in the upcoming weeks and months I plan to present specific programs
I have written (and simple examples of usage) that will help show this design
pattern in use within a real program.

For now, I hope you can appreciate this as a reference to help guide your
exploration of unique "a la carte" (yet not fixed-point-centric) approach to
building your programs! You can jump right into using these tools to build your
program *today* by importing
*[Data.Functor.Combinator](https://hackage.haskell.org/package/functor-combinators/docs/Data-Functor-Combinator.html)*
or wherever they can be found.

I'd be excited to hear about what programs you are able to write, so please do
let me know! You can leave a comment, find me on [twitter at
@mstk](https://twitter.com/mstk "Twitter"), or find me on freenode irc idling on
*#haskell* as *jle\`* if you want to share, or have any questions.

## Special Thanks

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts. Very special
thanks to my supporter at the "Amazing" level on
[patreon](https://www.patreon.com/justinle/overview), Josh Vera! :)

Also a special thanks to [Koz Ross](https://twitter.com/KozRoss), who helped
proofread this post as a draft.

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

[^1]: On the surface, this functor combinator design pattern might look like it
    fills a similar space to effects systems and libraries like
    *[mtl](https://hackage.haskell.org/package/mtl)*,
    *[polysemy](https://hackage.haskell.org/package/polysemy)*,
    *[freer-simple](https://hackage.haskell.org/package/freer-simple)*, or
    *[fused-effects](https://hackage.haskell.org/package/fused-effects)*.
    However, this design pattern actually exists on a different level.

    Functor combinator design patterns can be used to help build the *structure*
    of the *data types* and schemas that define your program/DSL. Once you build
    these nice structures, you then *interpret* them into some target context.
    This "target context" is the realm that libraries like *mtl* and *polysemy*
    can fill; functor combinators serve to help you define a structure for your
    program *before* you interpret it into whatever Applicative or Monad or
    effects system you end up using.

