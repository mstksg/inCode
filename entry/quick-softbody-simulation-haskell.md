Quick Soft-body Simulation in Haskell
=====================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/quick-softbody-simulation-haskell.html)

Since I started working at a Haskell company in a senior position last year,
I've had the pleasure of asking Haskell interview questions. For anyone with any
sort of physics or engineering/numerical I often ask them to walk me through a
very simple numerical soft-body physics simulation.[^1]

Typically this involves setting up position and velocity vectors of discretized
positions and stepping them through update equations: a rope hanging between two
fixed points allowed to sag, with the rope split up into finite elements and
each element treated as if they were suspended between the two neighbors by an
ideal spring.

An actual solution involving straightforward Euler integration is honestly only
a few lines of Haskell, and the question is more to gauge memory management and
control flow, so the straightforward solution is more than enough to demonstrate
those principles.

But! If you've been programming Haskell long enough, you often make a bunch of
libraries[^2] and years later you re-implement them after forgetting that you
wrote them. And the other day I just realized that that's exactly what happened
here --- I actually wrote a whole [blog post
series](https://blog.jle.im/entries/series/+hamilton.html) on the
*[hamilton](https://hackage.haskell.org/package/hamilton)* library and then
forgot all about it. So, just for fun, let's leverage the *hamilton* library to
get a very very quick softbody physics simulator in Haskell!

--------------------------------------------------------------------------------

Hi, thanks for reading! You can reach me via email at <justin@jle.im>, or at
twitter at [\@mstk](https://twitter.com/mstk)! This post and all others are
published under the [CC-BY-NC-ND
3.0](https://creativecommons.org/licenses/by-nc-nd/3.0/) license. Corrections
and edits via pull request are welcome and encouraged at [the source
repository](https://github.com/mstksg/inCode).

If you feel inclined, or this post was particularly helpful for you, why not
consider [supporting me on Patreon](https://www.patreon.com/justinle/overview),
or a [BTC donation](bitcoin:3D7rmAYgbDnp4gp4rf22THsGt74fNucPDU)? :)

[^1]: Don't worry, I only ask this for people who have a background in physics
    or numerical methods (which happens to be a big portion of the applicants,
    due to the nature of the position) and it's not about memorizing the
    equations but more about memory management and control flow.

[^2]: It is a well established fact that Haskellers are addicted to writing
    libraries.

