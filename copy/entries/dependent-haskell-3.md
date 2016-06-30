---
title: "Practical Dependent Types in Haskell: Type-Safe Neural Networks (Part 3)"
categories: Haskell, Ramblings
series: Practical Dependent Types in Haskell
tags: functional programming, dependent types, numerical, haskell, singletons, types, linear algebra, artificial neural networks, machine learning, existential types
create-time: 2016/06/29 16:57:24
date: Never
identifier: dependent-haskell-3
slug: practical-dependent-types-in-haskell-3
---




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
