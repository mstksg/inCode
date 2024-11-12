Faking GADTs in Languages That Shouldn't Have Them

===================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/faking-gadts-in-languages-that-shouldnt-have-them.html)

Haskell is the world's best programming language, but let's face the harsh
reality that a lot of times in life you'll have to write in other programming
languages. But alas you have been fully
[Haskell-brained](https://x.com/kmett/status/1844812186608099463) and lost all
ability to program unless it is type-directed, you don't even know how to start
writing a program without imagining its shape as a type first.

To feed your addiction you've already learned how to fake ADTs and monads in
Java and C++, and maybe you're writing in a language that actually supports them
already like purescript or dhall. But you're actually looking for the good
stuff, that GADT hit. Alas, even langauges that support ADTs (or can be coerced
into supporting them) might not support GADTs. Unless ...

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

