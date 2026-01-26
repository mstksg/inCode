---
title: "\"Five Point Haskell\" Part 3: Limited Atonement"
categories: Haskell
tags: functional programming, type safety
create-time: 2026/01/03 16:17:50
identifier: five-point-haskell-3
slug: five-point-haskell-part-3-limited-atonement
series: five-point-haskell
---

Hi! We're in Part 3 of *[Five-Point Haskell][]*! I've been trying to build a
framework to describe how I write maintainable and effective Haskell and also
highlight anti-principles I reject, and this the third part of that framework.

[Five-Point Haskell]: https://blog.jle.im/entries/series/+five-point-haskell.html
[Total Depravity]: https://blog.jle.im/entry/five-point-haskell-part-1-total-depravity.html
[Unconditional Election]: https://blog.jle.im/entry/five-point-haskell-part-2-unconditional-election.html

In [Total Depravity][], we talked about how the failure of mental context
windows is always only a matter of time, and how to use types defensively in
that light. In [Unconditional Election][], we talked about how mathematical
properties ensure the behavior of our instantiations regardless of any foreseen
merit of the implementations.

<!-- Welcome back to *[Five-Point Haskell][]*! This is my attempt to codify -->
<!-- principles of writing robust, maintainable, correct, clear, and effective code -->
<!-- in Haskell and to dispel common bad practices or heresies I have run into in my -->
<!-- time. -->

<!-- In the last two posts, we talked about [Total Depravity][] and [Unconditional -->
<!-- Election][]. We learned to distrust our own mental bookkeeping, and to lean on -->
<!-- types and parametricity to keep us honest. -->

<!-- However, the world does not stand still. Programs do IO, talk to services, -->
<!-- hit the network, read clocks, and have to live with messy, contingent reality. -->
<!-- We can't wish that away. But we *can* decide where it lives. -->

<!-- So, when writing Haskell, remember **Limited Atonement**. -->

<!-- > Limited Atonement: The goal in Haskell is not "universal purity", but the -->
<!-- > correct partition of pure and impure. -->
