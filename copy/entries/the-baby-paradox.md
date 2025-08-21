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

> Everybody loves my baby, but my baby don't love nobody but me.

Which is often formalized as:

$$
\begin{align}
\text{Axiom}_1 . & \forall x. \text{Loves}(x, \text{Baby})  \\
\text{Axiom}_2 . \forall x. & \text{Loves}(\text{Baby}, x) \implies x = me
\end{align}
$$

Let's prove in Haskell (in one line) that these two statements, taken together,
imply that I am my own baby.

## The normal proof

I haven't seen any official agreed upon name for this paradox. It could
reasonably be called the "Everybody loves my baby" paradox, but I'm going to
refer to it as "The Baby Paradox" because it's catchier.

The normal proof using propositional logic goes as follows:

1. If everyone loves Baby, Baby must love baby. (instantiate axiom 1 with $x =
   \text{Baby}$).
2. If baby loves someone, that someone must be me. (axiom 2)
3. Therefore, because baby loves baby, baby must be me. (instantiate axiom 2
   with axiom 1 with $x = \text{Baby}$)

## Haskell as a Theorem Prover

Anyway now let's talk about Haskell and how we can express and prove this
paradox in it.  First, some background: when using Haskell as a theorem prover,
you represent the theorem as a type, and _proving_ it involves _constructing_ a
value of that type --- you create an inhabitant of that type.

Using the Curry-Howard correspondence (often also called the Curry-Howard
isomorphism), we can pair some simple logical connectives with types:

1.  Logical "and" corresponds to tupling (or records of values). If `(a, b)` is
    inhabited, it means that both `a` and `b` are inhabited.
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
lot of simple proofs. For example, the proof of "If `x` and `y` together imply
`z`, then `x` implies that `y` implies `z`":

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
be uninhabited (the [principle of explosion][]). Let's prove that "'x or y'
being false implies both x and y are false": $\forall x y. \neg(x \lor y)
\implies (\neg x \wedge \neg y)$

[principle of explosion]: https://en.wikipedia.org/wiki/Principle_of_explosion

```haskell
deMorgan :: (Either a b -> Void) -> (a -> Void, b -> Void)
deMorgan f = (f . Left, f . Right)
```

(Maybe surprisingly, that's the same proof as `unEither`!)

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

Read this as "`Elem x xs` is true (inhabited) if either `x` is the first item,
or if `x` is an elem of the tail of the list". So for example, `Elem 5 [1,5,6]`
is inhabited but `Elem 7 [1,5,6]` is not:[^decidablenote]

```haskell
itsTrue :: Elem 5 [1,5,6]
itsTrue = There Here

itsNotTrue :: Elem 7 [1,5,6] -> Void
itsNotTrue = \case {}     -- GHC is smart enough to know both cases are invalid
```

[^decidablenote]: I'm pretty sure nobody has ever used it for anything useful,
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
`x ~ me`: we must be able to derive that person the baby loves is indeed
`me`.

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

And we have just proved it! Basically a one-liner. So, given the `BabyAxioms
loves me baby`, it is possible to prove that `me` _must_ be equal to `baby`.
That is, it is impossible to create any `BabyAxioms` without `me` and `baby`
being the same type.

The actual structure of the proof goes like this:

1.  First, we instantiated `everybodyLovesBaby` with `x ~ baby`, to get `loves
    baby baby`.
2.  Then, we used `myBabyOnlyLovesMe`, which normally takes `loves baby x` and
    returns `x :~: me`.  Because we give it `loves baby baby`, we get a `baby
    :~: me`!

And that's exactly the same structure of the original symbolic proof.


### What is Love?

We made `BabyAxioms` parametric over `loves`, `me`, and `baby`, which means
that these apply in _any_ universe where love, me, and baby follow the rules of
the song lyrics.

Essentially this means that for _any_ binary relationship `Loves x y`, _if_
that relationship follows these axioms, it _must_ be true that me is baby.
No matter what that relationship actually _is_, concretely.

That being said, it might be fun to play around with what this might look like
in concrete realizations of love, me, and my baby.

First, we could imagine that Love is completely mundane, and can be created
between any two operands without any extra required data or constraints ---
essentially, a `Proxy` between two phantoms:

```haskell
data Love a b = Love
```

In this case, it's impossible to create a `BabyAxioms` where `me` and `baby`
are different:

```haskell
data Love a b = Love

-- | me ~ baby is a cosntraint required by GHC
proxyLove :: (me ~ baby) => BabyAxioms Love me baby
proxyLove = BabyAxioms
    { everybodyLovesMyBaby = Love
    , myBabyOnlyLovesMe = \_ -> Refl
    }
```

The `me ~ baby` constraint being required by GHC is actually an interesting
manifestation of the paradox itself, without an explicit proof required on our
part.

We can imagine another concrete universe where it is only possible to love my
baby, and my baby is the singular recipient of love in this entire universe:

```haskell
data LoveOnly :: k -> k -> k -> Type where
    LoveMyBaby :: LoveOnly baby x baby

onlyBaby :: BabyAxioms (LoveOnly baby) me baby
onlyBaby = BabyAxioms
    { everybodyLovesMyBaby = LoveMyBaby
    , myBabyOnlyLovesMe = \case LoveMyBaby -> Refl
    }
```

Now we get both axioms fulfilled for free! Basically if we ever have a
`LoveOnly baby x me`, the only possible constructor is is `LoveMyBaby ::
LoveOnly baby x baby`, so me _must_ be baby!

Finally, we could imagine that love has no possible construction, with no way
to construct or realize. In this case, love is the uninhabited `Void`:

```haskell
data Love a b
```

In this universe, we can finally fulfil `myBabyOnlyLovesMe` without `me` being
`baby`, because "my baby don't love nobody but me" is vacuously true if there
is no possible love.  However, we cannot fulfil `everybodyLovesMyBaby` because
no love is possible, except in the case that the universe of people (`k`) is
also empty. But GHC doesn't have any way to encode empty kinds, I believe, so
we cannot realize these axioms even if `forall (x :: k)` is truly empty.

Note that we cannot fully encode the axioms purely as a GADT in Haskell --- our
`LoveOnly` was close, but it is too restrictive: in a fully general
interpretation of the song, we want to be able to allow other recipients of
love besides baby. Basically, Haskell GADTs cannot express the eliminators
necessary to encode `myBabyOnlyLovesMe` purely structurally, as far as I am
aware. But I could be wrong.

## Why

Nobody who listens to this song seriously believes that the speaker is
intending to convey that they are their own baby, or attempting to tantalize
the listener with an unintuitive tautology.  However, this is indeed a common
homework assignment in predicate logic classes, and I wasn't able to find
anyone covering this yet in Haskell, so I thought might as well be the first.

Sorry, teachers of courses that teach logic through Haskell.

I've also been using paradox as one of my go-to LLM stumpers, and it's actually
only recently (with GPT 5) that it's been able to get this right. Yay the
future? Before this, it would get stuck on trying to define a `Loves` GADT,
which is a dead end as previously discussed.

