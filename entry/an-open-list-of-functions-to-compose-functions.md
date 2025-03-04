An open list of functions to compose functions in Haskell

==========================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/an-open-list-of-functions-to-compose-functions.html)

Hi all, just a fun post here :) I've been telling myself for a long time to
compile a list of all the ways you can compose two functions, `(a -> b)` and
`(b -> c)` using functions in base and common libraries (and their simple
manipulations). There are an embarrassingly large amount of them, and I'm sure
that I'll find more over time. If any of you have suggestions, feel free to
leave a comment or find me on [twitter](https://twitter.com/mstk) or #haskell on
freenode as *jle\`* :)

1.  `(.)` (Prelude)
2.  `fmap` (Prelude)
3.  `(<$>)` (Data.Functor)
4.  `liftA` (Control.Applicative)
5.  `liftM` (Control.Monad)
6.  `(.)` (Control.Category)
7.  `(<<<)` (Control.Category)
8.  `(<<^)` (Control.Arrow)
9.  `(^<<)` (Control.Arrow)
10. `rmap` (Data.Profunctor)
11. `dimap id` (Data.Profunctor)

So, am I missing any?

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

