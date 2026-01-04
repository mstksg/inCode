---
title: "\"Five Point Haskell\" Part 2: Unconditional Election"
categories: Haskell
tags: functional programming, type driven development
create-time: 2026/01/01 21:51:17
identifier: five-point-haskell-2
slug: five-point-haskell-part-2-unconditional-election
series: five-point-haskell
---

Welcome back to my series, *[Five-Point Haskell][]*! This is my attempt to
codify principles of writing robust, maintainable, correct, clear, and
effective code in Haskell and to dispel common bad practices or heresies I have
ran into in my time.

[Five-Point Haskell]: https://blog.jle.im/entries/series/+five-point-haskell.html

In the last post, we talked about [Total Depravity][], which is about treating
any mentally-tracked constraint or condition as inevitably leading to a
catastrophe and denouncing the reliance on our flawed mental context windows.
Embracing the doctrine of total depravity helps us eliminate bugs and potential
errors through our types.

[Total Depravity]: https://blog.jle.im/entry/five-point-haskell-part-1-total-depravity.html

But, types don't only prevent bugs: they also help us in the process of
designing and structure our programs! So, let's jump right into our second
point in five-point Haskell: **Unconditional Election**!

> Unconditional Election: The _structure_ of your types fully determine the
> values and states it will ever take. Nothing at runtime can ever circumvent
> this.
>
> Therefore, take advantage and design the structure of your _types_ to
> anticipate the logic you want to model. The program is pre-destined before
> you even write any functions.


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

