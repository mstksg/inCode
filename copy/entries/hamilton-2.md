---
title: Quick Soft-body Simulation in Haskell
categories: Haskell
tags: functional programming, haskell, physics, numerical methods, dependent types
create-time: 2024/09/08 21:21:01
identifier: hamilton-2
slug: quick-softbody-simulation-haskell
series: Hamilton
---

Since I started working at a Haskell company in a senior position last year,
I've had the pleasure of asking Haskell interview questions. For anyone with
any sort of physics or engineering/numerical I often ask them to walk me
through a very simple numerical soft-body physics simulation.[^physics]

[^physics]: Don't worry, I only ask this for people who have a background in
physics or numerical methods (which happens to be a big portion of the
applicants, due to the nature of the position) and it's not about memorizing
the equations but more about memory management and control flow.

Typically this involves setting up position and velocity vectors of discretized
positions and stepping them through update equations: a rope hanging between
two fixed points allowed to sag, with the rope split up into finite elements
and each element treated as if they were suspended between the two neighbors by
an ideal spring.

An actual solution involving straightforward Euler integration is honestly only
a few lines of Haskell, and the question is more to gauge memory management and
control flow, so the straightforward solution is more than enough to
demonstrate those principles.

But! If you've been programming Haskell long enough, you often make a bunch of
libraries[^libraries] and years later you re-implement them after forgetting
that you wrote them. And the other day I just realized that that's exactly what
happened here --- I actually wrote a whole [blog post series][hamilton-blog] on
the *[hamilton][]* library and then forgot all about it. So, just for fun,
let's leverage the *hamilton* library to get a very very quick softbody physics
simulator in Haskell!

[hamilton-blog]: https://blog.jle.im/entries/series/+hamilton.html
[hamilton]: https://hackage.haskell.org/package/hamilton

[^libraries]: It is a well established fact that Haskellers are addicted to
writing libraries.
