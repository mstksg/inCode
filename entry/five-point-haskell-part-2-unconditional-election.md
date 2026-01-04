"Five Point Haskell" Part 2: Unconditional Election
===================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/five-point-haskell-part-2-unconditional-election.html)

Welcome back to my series, *[Five-Point
Haskell](https://blog.jle.im/entries/series/+five-point-haskell.html)*! This is
my attempt to codify principles of writing robust, maintainable, correct, clear,
and effective code in Haskell and to dispel common bad practices or heresies I
have ran into in my time.

In the last post, we talked about [Total
Depravity](https://blog.jle.im/entry/five-point-haskell-part-1-total-depravity.html),
which is about treating any mentally-tracked constraint or condition as
inevitably leading to a catastrophe and denouncing the reliance on our flawed
mental context windows. Embracing the doctrine of total depravity helps us
eliminate bugs and potential errors through our types.

But, types don't only prevent bugs: they also help us in the process of
designing and structure our programs! So, let's jump right into our second point
in five-point Haskell: **Unconditional Election**!

> Unconditional Election: The *structure* of your types fully determine the
> values and states it will ever take. Nothing at runtime can ever circumvent
> this.
>
> Therefore, take advantage and design the structure of your *types* to
> anticipate the logic you want to model. The program is pre-destined before you
> even write any functions.

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

