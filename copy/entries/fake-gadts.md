---
title: Faking GADTs in Languages That Shouldn't Have Them
categories: Haskell
tags: functional programming, haskell, purescript, dhall, java
create-time: 2024/11/12 13:59:35
identifier: fake-gadts
slug: faking-gadts-in-languages-that-shouldnt-have-them
---

Haskell is the world's best programming language, but let's face the harsh
reality that a lot of times in life you'll have to write in other programming
languages. But alas you have been fully [Haskell-brained][kmett] and lost all
ability to program unless it is type-directed, you don't even know how to start
writing a program without imagining its shape as a type first.

[kmett]: https://x.com/kmett/status/1844812186608099463

To feed your addiction you've already learned how to fake ADTs and monads in
Java and C++, and maybe you're writing in a language that actually supports
them already like purescript or dhall. But you're actually looking for the good
stuff, that GADT hit. Alas, even langauges that support ADTs (or can be
coerced into supporting them) might not support GADTs. Unless ...
