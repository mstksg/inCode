---
title: You could have invented Algebraic Data Types
categories: Math
tags: haskell, functional programming, type theory
create-time: 2020/02/25 15:35:45
date: never
identifier: adt
slug: you-could-have-invented-adts
---

If you follow the buzz around hot programming language features these days,
there is a chance you might have heard the term "algebraic data type" at some
point.

And, if you're like me, your reaction might have been something like "I
understand all of those words but it literally makes no sense to put them
together". Data? Types? Algebra?

Algebra is things like memorizing the quadratic equation...right?  Data is
stuff that user information that big tech corporations like to guzzle up.  And
types are classes, or ... wait, what was the difference between a type and a
class again?

Hopefully over the course of this blog post, you will learn not only what those
words mean in that context, but also feel confident that *you* could have
invented the idea yourself, just by playing around with types.  And maybe, as a
bonus along the way, you'll be able to also see why they might be a useful
concept.

<blockquote class="twitter-tweet"><p lang="en" dir="ltr">Is this what they mean by &quot;sum types&quot; <a href="https://t.co/uli9snR0TB">https://t.co/uli9snR0TB</a></p>&mdash; Joe Groff (@jckarter) <a href="https://twitter.com/jckarter/status/1232419073511706624?ref_src=twsrc%5Etfw">February 25, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

Hi.
