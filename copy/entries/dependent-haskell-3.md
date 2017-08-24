---
title: "Practical Dependent Types: A Deeper Look at Proofs"
categories: Haskell, Ramblings
series: Practical Dependent Types in Haskell
tags: functional programming, dependent types, numerical, haskell, singletons, types, linear algebra, artificial neural networks, machine learning, existential types
create-time: 2016/06/29 16:57:24
date: Never
identifier: dependent-haskell-3
slug: practical-dependent-types-in-haskell-3
---

Hi!  This will be the final post of the [dependently typed neural network
series][series].  This post has been long put-off because there really wasn't
*too* much more we can learn about dependent types and programming with
dependent types from this example.  Still, this example still has a bit more to
offer us, so let's take one last look :)  Think of this post as an epilogue.

[series]: https://blog.jle.im/entries/series/+practical-dependent-types-in-haskell.html

In [Part 1][], we tried solving our problem in an "untyped" way, recognized all
of those Haskell red flags, and then added types to make things safer and reap
benefits in API clarity, compiler support, and a smoother code writing
experience.  In [Part 2][], we looked at how integrating our typed programs
with the real world, where types might vary at runtime, by levering existential
types in two different forms.

[Part 1]: https://blog.jle.im/entry/practical-dependent-types-in-haskell-1.html
[Part 2]: https://blog.jle.im/entry/practical-dependent-types-in-haskell-2.html

The Secret Life of Proofs
-------------------------

So far in this journey, we've hand-waved the idea of "proofs", and their role
in this dependently typed programming.  In fact, I've sort of intentionally
left the idea vague, because I feel like a lot of other dependent type
introductions/tutorials over-emphasize proofs, to the point there is a
public impression that [dependently typed programming is all about
proofs][reddit]:

[reddit]: https://www.reddit.com/r/haskell/comments/62uv6g/verify_your_typeclass_instances_in_haskell_today/dfpt2g7/

> I think dependent types are pretty cool, but honestly the sheer difficulty jump
> from quickcheck tests to actual proofs to me seems a bit too massive to justify
> having to deal with them much.
>
> I think if we just had testable functions (basically just predicates that
> should always return true) alongside classes that could be quickchecked
> against. And actually took it very seriously when an instance isn't lawful
> (basically just disallow it almost always), then I think we would be in a
> pretty darn good spot.

There's a popular (mis?)conception that dependent types are basically a
replacement for QuickCheck, or refinement types (like Liquid Haskell).  But
really, as we have seen, they are much more --- they let you leverage the
compiler to help you write your code, help you enforce properties at the
structural level for your types, create more expressive API's, and much more.
They aren't "just" to prove properties about your program.

In reality, the idea of dependent types and the idea of proofs sort of are
separate but complementing ideas.  Dependent types enable this "proofy" style
discussed in the comment, but, also, the ability to dynamically construct and
manipulate proofs opens up many doors for the capabilities of dependently typed
programs.

### What are they?

In the context of dependent types (in Haskell), a "proof" is general
(non-rigorous, informal) term for a *runtime value* that GHC/Haskell can use
for *type-level* shenanigans.  In other words, it's a *term-level* witness for
a *type-level* truth.

It's the critical *run-time* values that we use to power our *type-level* fun,
and are also commonly known as "witnesses".

I don't believe there is any rigorous definition, but the term comes up
whenever we talk about generating or using run-time values that give us
type-level power.

In this article, the simplest example of something that can be called a proof
or a witness is the singleton for `Nat`s: A value of type `Sing n` (for `Nat
n`) "witnesses" a `KnownNat n` constraint.

We saw it earlier when we wanted to pass along evidence that our `n` has an
instance of `KnownNat`:

```haskell
nIsKnownNatWePromise :: forall n. Sing n -> Integer
nIsKnonwNatWePromise = \case
    SNat -> natVal (Proxy :: Proxy n)
```

In the case statement above, in the `SNat ->` branch, GHC knows that `n` has a
`KnownNat` instance.  This runtime value, `SNat`, carries the fact that we have
an instance of `KnownNat n`.  Remember that
`natVal :: KnownNat n => p n -> Integer` -- it only works of `n` is an instance
of `KnownNat`.  `SNat :: Sing n` is a runtime value that "gives us" that
instance.  It's a "proof" that `n` is an instance of `KnownNat`.

### First-Class Values

The *key* to their usefulness is that, as runtime values, they are *first-class
values* in Haskell.  We can manipulate them, pass them, construct them, etc.,
just as if they were normal values.  You can construct complex proofs from
simple ones, compose them, etc.

For example, check out the function `%:+` from the *singletons* package:

```haskell
(%:+) :: forall (n :: Nat) (m :: Nat). Sing n -> Sing m -> Sing (n + m)
```

Given, at runtime, a witness for `KnownNat n` (our `Sing n`) and a witness for
`KnownNat m`, we can construct, at runtime a witness for `KnownNat (n + m)`:

```haskell
add :: Integer -> Integer -> Integer
add x y = withSomeSing x $ \(sx :: Sing x) ->
          withSomeSing y $ \(sy :: Sing y) ->
            case sx %:+ sy of
              -- z is x + y
              (SNat :: Sing z) -> natVal (Proxy :: Proxy z)
```

```haskell
ghci> add 5 7
12
```



<!-- Uniting Existential Contexts -->
<!-- ---------------------------- -->

<!-- In the last exercise, we introduced `SomeNet`: -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "data SomeNet" -->
<!-- ~~~ -->

<!-- `SomeNet` is actually a big step above `OpaqueNet` because now its external API -->
<!-- (the size of vectors that it takes/outputs) is now existentially quantified, so -->
<!-- this presents some unique challenges. -->

<!-- Recall that we was able to write `runOpaqueNet` without much problems, because -->
<!-- the types guaranteed that everything made sense: -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "runOpaqueNet ::" -->
<!-- ~~~ -->

<!-- In fact, GHC actually enforces that everything works out --- it knows that you -->
<!-- run a `n'` with an `R i`, and sees that `x` is an `R i`, and also knows that -->
<!-- whatever the internal structure is, an `R o` is always what pops out regardless -->
<!-- if `hs` is `'[]`, `'[5,3]` or `'[100,200,4]`. -->

<!-- But can we write a sensible `runSomeNet`?  What would the type even be?  Let's -->
<!-- try an initial attempt: -->

<!-- ~~~haskell -->
<!-- runSomeNet :: (KnownNat i, KnownNat o) -->
<!--            => SomeNet -->
<!--            -> R i -->
<!--            -> R o -->
<!-- runSomeNet n x = case n of -->
<!--                    SNet n' -> runNet n' x -->
<!-- ~~~ -->

<!-- Hm.  This clearly won't work, because the network inside `SomeNet` might not -->
<!-- even take the `R i` that we give it.  What if it takes a `R 5`, but we pass in -->
<!-- an `R 10`?  Remember, because of universal quantification, `runSomeNet` has to -->
<!-- work with *any* `i`, be it 5, 10, or 100.  But the internal network might not -->
<!-- be so accommodating.  If we try to write it, GHC will complain immediately.  In -->
<!-- short, `runSomeNet` should be *partial*, and return a `Maybe`. -->

<!-- ~~~haskell -->
<!-- runSomeNet :: (KnownNat i, KnownNat o) -->
<!--            => SomeNet -->
<!--            -> R i -->
<!--            -> Maybe (R o) -->
<!-- ~~~ -->

<!-- We can see another problem here --- We can't have it return `R o`, of course, -->
<!-- because `o` is universally quantified here, so the user can decide `o`.  But -->
<!-- `o` isn't free for the user to pick...it's determined by the network inside -->
<!-- `SNet`.  So, the `o` has to be existentially quantified.  We'll return a -->
<!-- continuation-style existentially quantified `o` here, because *hmatrix* doesn't -->
<!-- come with a built-in constructor-style quantifier: -->

<!-- ~~~haskell -->
<!-- runSomeNet :: KnownNat i -->
<!--            => SomeNet -->
<!--            -> R i -->
<!--            -> (forall o. KnownNat o => R o -> r) -->
<!--            -> Maybe r -->
<!-- ~~~ -->

<!-- And finally, we have a type signature that makes sense: give a `SomeNet` and an -->
<!-- `R i`, and possibly get in return an existentially quantified `R o`.  If the `R -->
<!-- i` doesn't fit into the `SomeNet`, the result will be `Nothing`. -->

<!-- Now that we have a type, let's try implementing it: -->

<!-- ~~~haskell -->
<!-- runSomeNet :: KnownNat i -->
<!--            => SomeNet -->
<!--            -> R i -->
<!--            -> (forall o. KnownNat o => R o -> r) -->
<!--            -> Maybe r -->
<!-- runSomeNet n x f = case n of -->
<!--                      SNet (n' :: Network i' hs o) -> -->
<!--                        if natVal (Proxy @i') == natVal (Proxy @i) -->
<!--                          then Just (f (runNet n' x)) -->
<!--                          else Nothing -->
<!-- ~~~ -->

<!-- First, we open it and check if the `i'` inside the `SNet` is the same as the -->
<!-- `i` we get as input.  If it is, we return `Just`, and if not, `Nothing`. -->

<!-- Unfortunately, this doesn't really work.  That's because our silly little `==` -->
<!-- doesn't actually prove to GHC that the two lengths are equal.  GHC will still -->
<!-- believe that `i` and `i'` are different, in general. -->

<!-- And why should it believe that `i ~ i'`, just because of `==`?  Remember that -->
<!-- `==` is a user-defined function, and can return anything.  Why should the type -->
<!-- checker be ~~fooled~~ convinced by a silly user-defined function? -->

<!-- The problem is that the `Bool` returned doesn't really tell the compiler -->
<!-- anything.  It's just a bit of information, and doesn't really come with any -->
<!-- proof that the two types are actually equal.  What we need is a way to *prove* -->
<!-- to the compiler (and the typechecker) that the two are equal. -->

<!-- ### Proofs -->

<!-- We got far without talking about proofs, but really, you can only expect to -->
<!-- get so far when talking about dependently typed programming without talking -->
<!-- about proofs!  Proofs are, in a way, essential to the very essence of -->
<!-- dependently typed programming.[^proofs] -->

<!-- [^proofs]: One thing I've noticed, however, is that a lot of dependently typed -->
<!-- programming introductions *begin* with proofs, and go to applications later. -->
<!-- Here, I hope I can change that trend by starting with the applications, and -->
<!-- bringing proofs later after being able to see their motivation! -->
