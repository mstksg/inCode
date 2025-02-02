---
title: Faking ADTs and GADTs in Languages That Shouldn't Have Them
categories: Haskell
tags: functional programming, haskell, purescript, dhall, java
create-time: 2024/11/12 13:59:35
identifier: fake-adts-and-gadts
slug: faking-adts-gadts-in-languages-that-shouldnt-have-them
---

Haskell is the world's best programming language[^best], but let's face the harsh
reality that a lot of times in life you'll have to write in other programming
languages. But alas you have been fully [Haskell-brained][kmett] and lost all
ability to program unless it is type-directed, you don't even know how to start
writing a program without imagining its shape as a type first.

[^best]: I bet you thought there was going be some sort of caveat in this
footnote, didn't you?
[kmett]: https://x.com/kmett/status/1844812186608099463

Well, fear not. The foundational theory behind ADTs and GADTs are so
fundamental that they'll fit (somewhat) seamlessly into whatever language
you're forced to write. After all, if they can fit [profunctor optics in
Microsoft's Java code][profunctor], the sky's the limit!

[profunctor]: https://www.reddit.com/r/haskell/comments/9m2o5r/digging_reveals_profunctor_optics_in_mineacraft/

ADTs and the Visitor Pattern
----------------------------

Let's get normal ADT's out of the way. Most languages do have structurs

