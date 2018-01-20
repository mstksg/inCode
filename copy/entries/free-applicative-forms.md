---
title: Abstract Validating Forms with Free Applicative/Alternative
categories: Haskell
tags: functional programming, haskell, types, free, applicative, alternative, ghcjs
create-time: 2018/01/19 23:30:51
date: none
identifier: free-applicative-forms
slug: forms-with-free-applicative-alternative
---

One tool I've been finding myself using a lot recently is the *Free
Applicative* (and *Free Alternative*), from the *[free][]* package.

[free]: https://hackage.haskell.org/package/free

Free Monads are great, and they're often used to implement the "interpreter
pattern" (although I personally prefer *[operational][]*, as I wrote about in a
[previous blog post][duet], for that design pattern).  However, Free
Applicatives are really a completely different type of thing, and the use cases
for each are pretty disjoint.

[duet]: https://blog.jle.im/entry/interpreters-a-la-carte-duet.html

If I had to make a general statement, I'll say that free monads are especially
good at representing the idea of abstract *sequential* generators (sequences
that are chained dependently one after the other), and that
free applicatives are especially good at representing the idea of abstract
*parallel* generators (things operating in parallel without any interconnected
data dependences).

The general approach to utilizing the Free Applicative is to start with some
Functor `F` (`F a` represents the act of generating a value of type `a`).  Once
you throw `F` into `Ap` (or `Alt`) to get `Ap F`, you now are able to *combine
`F`s in parallel* with `<$>`, `<*>`, `liftA2`, `sequence`, `traverse`, etc.,
even though `F` normally could not support such combinations.  Then, finally,
you have the ability to provide a concrete generator function `forall a.
Applicative f => F a -> f a` (given `F a`, return an actual generator of `a`s
in some `Applicative`), and the magic of the Free Applicative will go in and
actually run all of your combined `F` actions "in parallel".  The trick is
that, with the same value of `Ap F a`, you can *run multiple different
concrete generators* on it, so you can realize `Ap F` in multiple different
contexts and situations, adapting it for whatever you need.

For this post, I'll be talking about using the Free Applicative `Ap` (and the
Free Alternative, `Alt`) in order to generate an abstract representation of a
validating form, and leveraging this representation to realize these forms in
terminal IO, JSON/YAML, PDF documents, and even on the browser using *ghcjs*
and *[miso][]*.

[miso]: https://hackage.haskell.org/package/miso
