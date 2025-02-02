Faking ADTs and GADTs in Languages That Shouldn't Have Them

============================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/faking-adts-gadts-in-languages-that-shouldnt-have-them.html)

Haskell is the world's best programming language[^1], but let's face the harsh
reality that a lot of times in life you'll have to write in other programming
languages. But alas you have been fully \[Haskell-brained\]\[kmett\] and lost
all ability to program unless it is type-directed, you don't even know how to
start writing a program without imagining its shape as a type first.

Well, fear not. The foundational theory behind ADTs and GADTs are so fundamental
that they'll fit (somewhat) seamlessly into whatever language you're forced to
write. After all, if they can fit [profunctor optics in Microsoft's Java
code](https://www.reddit.com/r/haskell/comments/9m2o5r/digging_reveals_profunctor_optics_in_mineacraft/),
the sky's the limit!

## ADTs and the Visitor Pattern

Let's get normal ADT's out of the way. Most languages do have structurs

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

[^1]: I bet you thought there was going be some sort of caveat in this footnote,
    didn't you? \[kmett\]: https://x.com/kmett/status/1844812186608099463

