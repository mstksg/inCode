---
title: Lenses embody Products, Prisms embody Sums
categories: Haskell
tags: lenses, profunctors
create-time: 2018/05/22 23:29:16
identifier: lenses-and-prisms
slug: lenses-products-prisms-sums
---

I've written about a variety of topics on this blog, but one thing I haven't
touched in too much detail is the topic of lenses and optics.  A big part of
this is because there are already so many great resources on lenses, like the
famous (and my favorite) [lenses over tea][tea] series.

[tea]: https://artyom.me/lens-over-tea-1

This post won't be a "lens tutorial", but rather a dive into a (what I believe
is an) insightful perspective on lenses and prisms that I've heard repeated
many times, but not yet all gathered together into a single place.  In
particular, I'm going to talk about the perspective of lenses and prisms as
embodying the essences of products and sums (respectively), and how that
observation can help you with a more "practical" understanding of lenses and
prisms.
