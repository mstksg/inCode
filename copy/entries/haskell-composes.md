---
title: An open list of functions to compose functions in Haskell
categories: Haskell, Lists
tags: haskell
create-time: 2015/01/24 16:00:38
date: Never
identifier: haskell-composes
slug: an-open-list-of-functions-to-compose-functions
old-slugs: an-open-list-of-was-to-compose-functions, an-open-list-of-ways-to-compose-functions
entry-id: 32
---

Hi all, just a fun post here :)  I've been telling myself for a long time to
compile a list of all the ways you can compose two functions, `(a -> b)` and
`(b -> c)` using functions in base and common libraries (and their simple
manipulations). There are an embarassingly large amount of them, and I'm sure
that I'll find more over time.  If any of you have suggestions, feel free to
leave a comment or find me on [twitter][] or #haskell on freenode as *jle\`* :)

[twitter]: https://twitter.com/mstk

1.  `(.)` (Prelude)
2.  `fmap` (Prelude)
3.  `(<$>)` (Data.Functor)
4.  `liftA` (Control.Applicative)
5.  `liftM` (Control.Monad)
6.  `(.)` (Control.Category)
7.  `(<<<)` (Control.Category)
9.  `(<<^)` (Control.Arrow)
10. `(^<<)` (Control.Arrow)
13. `rmap` (Data.Profunctor)
14. `dimap id` (Data.Profunctor)

So, am I missing any?


