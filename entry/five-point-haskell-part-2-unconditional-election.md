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

However, picking good structures, making invalid states unrepreentable, etc. can
only go so far.

Types aren't just about preventing bad behaviors. They're about designing good
code. And there is one principle that helps guide this design by leveraging the
unyielding properties of the universe *itself* to take care of our fate, even
when we are unable to structure our types well.

Let's jump into the second point in five-point Haskell: **Unconditional
Election**!

> Unconditional Election: The power of the `forall` to elect or reprobate
> instantiations and implementations through parametric polymorphism.
>
> Invariants and correctness are not based on the foreseen merits of the
> structure of types you choose, or on runtime reflection and ad-hoc
> polymorphim, but rather in the predestination of universal quantification.
>
> Therefore, surrender to your control to parametric polymorphism in all things.
> In simple terms: add a type parameter.

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

