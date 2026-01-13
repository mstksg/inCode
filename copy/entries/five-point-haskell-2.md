---
title: "\"Five Point Haskell\": Unconditional Election (Parametric Polymorphism)"
categories: Haskell
tags: functional programming, parametric polymorphism
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

[Total Depravity]: https://blog.jle.im/entry/five-point-haskell-part-1-total-depravity.html

However, stopping here gives us an incomplete picture. Firstly, types aren't
just about preventing bad behaviors. They're about designing good code.
Secondly, there is only so much you can do by picking good structures and
making invalid states unrepresentable. Human tools and human flaws.

The next point, to me, is about an aspect of the type system that I see little
coverage of outside of Haskell and typed FP circles, but is a principle of
design that I find permeating everything I write. It's about leveraging the
unyielding properties of the universe _itself_ to take care of our fate, even
when we are unable to structure our types well. It's **Unconditional
Election**!

> Unconditional Election: The power of the `forall` to elect or reprobate
> instantiations and implementations through parametric polymorphism.
>
> Invariants and correctness are not based on the foreseen merits of the
> structure of types you choose, or on runtime reflection and ad-hoc
> polymorphim, but rather in the predestination of universal quantification.
>
> Therefore, surrender to your control to parametric polymorphism in all
> things. Embrace one of Haskell's greatest unexpected strengths: the type
> parameter.

