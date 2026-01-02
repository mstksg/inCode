---
title: "\"Five Point Haskell\" Part 2: Unconditional Election"
categories: Haskell
tags: functional programming, type safety
create-time: 2026/01/01 21:51:17
identifier: five-point-haskell-2
slug: five-point-haskell-part-2-unconditional-election
series: five-point-haskell
---

<!-- ### Squished Pipeline -->

<!-- Along the same lines, there is often the temptation to squish multiple stages -->
<!-- along a pipeline into the same type. -->

<!-- For example, your "checkout" workflow might incrementally set `Maybe` fields: -->

<!-- ```haskell -->
<!-- data Checkout = Checkout -->
<!--     { items :: [Item] -->
<!--     , address :: Maybe Address -->
<!--     , payment :: Maybe Token -->
<!--     } -->
<!-- ``` -->

<!-- You start with an empty `Checkout` state...then you add `[Item]`s...then you -->
<!-- add `Maybe Address`...then you add `Maybe Token` for payment. However, payment -->
<!-- requires an address: -->

<!-- ```haskell -->
<!-- pay :: Checkout -> IO Checkout -->
<!-- pay c = case address c of -->
<!--   Just addr -> do --> 
<!--     tok <- processPayment (items c) addr -->
<!--     pure $ c { payment = Just tok } -->
<!--   Nothing -> -- uh.... -->
<!-- ``` -->

<!-- `pay` doesn't _really_ take a `Maybe Address`, it requires an actual `Address`! -->
<!-- Its input type is too "big". This is a subtle manifestation of the same -->
<!-- problems as shotgun parsing: there is no indication in the type about the -->
<!-- actual stage it is in and what operations can legally be done. -->

<!-- To fix this, we can just...not keep them all as the same type. -->

<!-- ```haskell -->
<!-- data PreCheckout = PreCheckout [Item] -->
<!-- data PrePayment = PrePayment [Item] Address -->
<!-- data PostPayment = PostPayment [Item] Address Token -->

<!-- pay :: PrePayment -> IO PostPayment -->
<!-- pay (PrePayment items addr) = do -->
<!--     tok <- processPayment items addr -->
<!--     pure $ PostPayment items addr tok -->
<!-- ``` -->

