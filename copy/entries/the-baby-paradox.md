---
title: The Baby Paradox in Haskell
categories: Haskell
tags: functional programming, logic
create-time: 2025/08/16 18:19:36
identifier: the-baby-paradox
slug: the-baby-paradox-in-haskell
---

*[Everybody Loves My Baby][]* is a Jazz Standard from 1924 with the famous
lyric:

[Everybody Loves My Baby]: https://en.wikipedia.org/wiki/Everybody_Loves_My_Baby

> Everybody loves my baby, but my baby don't love nobody but me

Which is often formalized as:

$$
\begin{align}
\text{Axiom}_1 . & \forall x. \text{Loves}(x, \text{Baby})  \\
\text{Axiom}_2 . \forall x. & \text{Loves}(\text{Baby}, x) \implies x = me
\end{align}
$$

Let's prove in Haskell that these two statements, taken together, imply that I
am my own baby.

## The normal proof

The normal proof using propositional logic goes as follows:

1. If everyone loves Baby, Baby must love baby. (instantiate axiom 1 with $x =
   \text{Baby}$).
2. If baby loves someone, that someone must be me. (axiom 2)
3. Therefore, because baby loves baby, baby must be me. (instantiate axiom 2
   with axiom 1 with $x = \text{Baby}$)

## Haskell as a Theorem Prover

First, some background: when using Haskell as a theorem prover, you represent
the theorem as a type, and _proving_ it involves _constructing_ a value of that
type --- you create an inhabitant of that type.

Using the Curry-Howard correspondence (often also called the Curry-Howard
isomorphism), we can pair some simple logical connectives with types:

1.  Logical "and" corresponds to tupling. If `(a, b)` is inhabited, it means
    that both `a` and `b` are inhabited.
2.  Logical "or" corresponds to sums, `Either a b` being inhabited implies that
    either `a` or `b` are inhabited. They might both the inhabited, but `Either
    a b` requires the "proof" of only one.
3.  Constructivist logical implication is a function: If `a -> b` is inhabited,
    it means that an inhabitant of `a` can be used to create an inhabitant of
    `b`.
4.  Any type with a constructor is "true": `()`, `Bool`, `String`, etc.; any
    type with no constructor (`data Void`) is "false" because it has no
    inhabitants.
5.  Introducing type variables (`forall a.`) corresponds to...well, for all. If
    `forall a. Either a ()` means that `Either a ()` is "true" (inhabited) for
    all possible `a`.  This one represented logically as $\forall x. x \lor
    \text{True}$.

You can see that, by chaining together those primitives, you can translate a
lot of simple proofs. For example, the proof of "If both `x` and `y` imply `z`,
then `x` implies that `y` implies `z`":

$$
\forall x y z. ((x \wedge y) \implies z) \implies (x \implies (y \implies z))
$$

Can be expressed as:

```haskell
curry :: forall a b c. ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)
```

Or maybe, "If either x or y imply z, then x implies z and y implies z,
independently:"

$$
\forall x y z. ((x \lor y) \implies z) \implies ((x \implies z) \land (y \implies z)))
$$

In haskell:

```haskell
unEither :: (Either a b -> c) -> (a -> c, b -> c)
unEither f = (f . Left, f . Right)
```

And, we have a version of negation: if `a -> Void` is inhabited, then `a` must
be non-inhabited. Let's prove that "'x or y' being false implies both x and y
are false": $\forall x y. \neg(x \lor y) \implies (\neg x \wedge \neg y)$

```haskell
bothFalse :: (Either a b -> Void) -> (a -> Void, b -> Void)
bothFalse f = (f . Left, f . Right)
```

Maybe surprisingly, that's the same proof as `unEither`!

We can also think of "type functions" (type constructors that take arguments)
as "parameterized propositions":

```haskell
data Maybe a = Nothing | Maybe a
```

`Maybe a` (like $\text{Maybe}(x)$) is the proposition that $\text{True} \lor x$:
`Maybe a` is always inhabited, because "True or X" is always True. Even `Maybe
Void` is inhabited, as `Nothing :: Maybe Void`.

The sky is the limit if we use GADTs. We can create arbitrary propositions by
restricting what types constructors can be called with. For example, we can
create a proposition that `x` is an element of a list:

```haskell
data Elem :: k -> [k] -> Type where
    Here :: Elem x (x : xs)
    There :: !(Elem x ys) -> Elem x (y : ys)
```

Read this as "`Elem x xs` is true if either `x` is the first item, or if `x` is
an elem of the tail of the list". So for example, `Elem 5 [1,5,6]` is inhabited
but `Elem 7 [1,5,6]` is not:[^decidablenote]

```haskell
itsTrue :: Elem 5 [1,5,6]
itsTrue = There Here

itsNotTrue :: Elem 7 [1,5,6] -> Void
itsNotTrue = \case {}     -- GHC is smart enough to know both cases are invalid
```

[^decidablenote]: I'm not sure if anyone has ever used it for anything useful,
but I wrote the entire *[decidable][]* library around manipulating propositions
like this.

[decidable]: https://hackage.haskell.org/package/decidable

We can create a two-argument proposition that two types are equal, `a :~: b`:

```haskell
data (:~:) :: k -> k -> Type where
    Refl :: a :~: a
```

The proposition `a :~: b` is only inhabited if `a` is equal to `b`, since
`Refl` is its only constructor.

## The Baby Paradox

Now we have enough to express the baby paradox in Haskell. Let's parameterize
it over a proposition `loves`, where `loves a b` being inhabited means that `a`
loves `b`.

We can express our axiom as a record of propositions in terms of the atoms
`loves`, `me`, and `baby`:

```haskell
data BabyAxioms loves me baby = BabyAxioms
    { everybodyLovesMyBaby :: forall x. loves x baby
    , myBabyOnlyLovesMe :: forall x. loves baby x -> x :~: me
    }
```

The first axiom `everybodyLovesMyBaby` means that for _any_ `x`, `loves x baby`
must be "true" (inhabited). The second axiom `myBabyOnlyLovesMe` means that
_if_ we have a `loves baby x` (if my baby loves someone), then it must be that
`x ~ me`: we can prove that that person the baby loves is indeed `me`.

The expression of the baby paradox then relies on writing the function

```haskell
babyParadox :: BabyAxioms loves me baby -> me :~: baby
```

And indeed if we play around with GHC enough, we'll get this typechecking
implementation:

```haskell
babyParadox :: BabyAxioms loves me baby -> me :~: baby
babyParadox BabyAxioms{everybodyLovesMyBaby, myBabyOnlyLovesMe} =
    myBabyOnlyLovesMe everybodyLovesMyBaby
```

Using `x & f = f x` from *Data.Function*, this becomes a bit smoother to read:

```haskell
babyParadox :: BabyAxioms loves me baby -> me :~: baby
babyParadox BabyAxioms{everybodyLovesMyBaby, myBabyOnlyLovesMe} =
    everybodyLovesMyBaby & myBabyOnlyLovesMe
```

And we have just proved it! That is, given the `BabyAxioms loves me baby`, it
is possible to prove that `me` _must_ be equal to `baby`. That is, it is
impossible to create any `BabyAxioms` without `me` and `baby` being the same
type.

Remember that we made this parametric over `loves`: it means that for _any_
binary relationship `Loves x y`, _if_ that relationship follows these axioms,
it _must_ be true that `me` is `baby`. No matter what that relationship
actually _is_, concretely.

The actual structure of the proof went like this:

1.  First, we instantiated `everybodyLovesBaby` with `x ~ baby`, to get `loves
    baby baby`.
2.  Then, we used `myBabyOnlyLovesMe`, which normally takes `loves baby x` and
    returns `x :~: me`.  Because we give it `loves baby baby`, we get a `baby
    :~: me`!

And that's exactly the same structure of the original symbolic proof.

## Why

Nobody who listens to this song seriously believes that the speaker is
intending to convey that they are their own baby, or attempting to tantalize
the listener with an unintuitive tautology.  However, this is indeed a common
homework assignment in predicate logic classes, and I wasn't able to find
anyone covering this yet in Haskell, so I thought might as well be the first.

Sorry, teachers of courses that teach logic through Haskell.

I've also been using paradox as one of my go-to LLM stumpers, and it's actually
only recently (with GPT 5) that it's been able to get this right. Before this,
it would get stuck on trying to define a `Loves` GADT, which is a dead end.
Yay the future? None of them cat get [my dhall recursive GADT puzzle][dhall]
yet quite either even with a bit of coaching, but it's only a matter of time
before it ends up in the training data I suppose.

[dhall]: https://blog.jle.im/entry/faking-adts-and-gadts.html#recursive-gadts
