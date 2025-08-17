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
\begin{multiline}
\text{Axiom}_1 . \forall x. L(x, \text{Baby})  \\
\text{Axiom}_2 . \forall x. L(\text{Baby}, x) \implies x = me
\end{multiline}
$$

Let's prove in Haskell that these two statements, taken together, imply that I
am my own baby.

## Haskell as a Theorem Prover

First, some background: when using Haskell as a theorem prover, you represent
the theorem as a type, and _proving_ it involves _constructing_ a value of that
type --- you create an inhabitant of that type.

Using the Curry-Howard isomorphism, we can pair some simple logical connectives
with types:

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
    \Top$.

You can see that, by chaining together those primitives, you can translate a
lot of simple proofs, like $\forall x y z. ((x \vor y) \implies z) \implies (x
\implies z)$

Translated into Haskell, that's `forall a b c. (Either a b -> c) -> a -> c`, which
you can write as:

```haskell
foo :: (Either a b -> c) -> a -> c
foo f x = f (Left x)
```

We can also think of "type functions" (type constructors that take arguments)
as "parameterized propositions":

```haskell
data Maybe a = Nothing | Maybe a
```

`Maybe a` (like $\text{Maybe}(x)$) is the proposition that $\Top \vor x$:
`Maybe a` is always inhabited, because "True or X" is always True. Even `Maybe
Void` is inhabited, as `Nothing :: Maybe Void`.

The sky is the limit if we use GADTs: we can have:

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

We can express our axiom as a record of propositions in terms of `loves`, `me`,
and `baby`:

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

## Why

Nobody who listens to this song seriously believes that the speaker is
intending to convey an unintuitive tautology.  However, this is indeed a common
homework assignment in predicate logic classes, and I wasn't able to find
anyone covering this yet in Haskell, so I thought might as well be the first.

Sorry, teachers of courses that teach logic through Haskell.
