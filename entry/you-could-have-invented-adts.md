You could have invented Algebraic Data Types

=============================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/you-could-have-invented-adts.html)

If you follow the buzz around hot programming language features these days,
there is a chance you might have heard the term "algebraic data type" at some
point.

And, if you're like me, your reaction might have been something like "I
understand all of those words but it literally makes no sense to put them
together". Data? Types? Algebra?

Algebra is things like memorizing the quadratic equation...right? Data is stuff
that user information that big tech corporations like to guzzle up. And types
are classes, or ... wait, what was the difference between a type and a class
again?

Hopefully over the course of this blog post, you will learn not only what those
words mean in that context, but also feel confident that *you* could have
invented the idea yourself, just by playing around with types. And maybe, as a
bonus along the way, you'll be able to also see why they might be a useful
concept.

The idea for this post came from a tweet I saw from [Joe
Groff](https://twitter.com/jckarter):

```{=html}
<blockquote class="twitter-tweet">
```
```{=html}
<p lang="en" dir="ltr">
```
Is this what they mean by \"sum types\"
`<a href="https://t.co/uli9snR0TB">`{=html}https://t.co/uli9snR0TB`</a>`{=html}
```{=html}
</p>
```
--- Joe Groff (@jckarter)
`<a href="https://twitter.com/jckarter/status/1232419073511706624?ref_src=twsrc%5Etfw">`{=html}February
25, 2020`</a>`{=html}
```{=html}
</blockquote>
```
```{=html}
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
```
It got me thinking: if you *didn't* know what sum types where, how *would* you
attempt to make sense out of `typeof a + typeof b`? What answer makes the most
sense?

## Pocket full of posies

The root "problem" of the silly javascript snippet is that `typeof a` returns a
`string`, and not ... well, a type.

So from the start, let's establish the idea of a "data type" as whatever it is
that `typeof a` might return.

We're going to exit the "world" of javascript a bit and talk about what it would
mean to add (and do other stuff) to *types* together. We're not going to be
literally implementing `+` for types, but rather imagining "what the result" of
adding two types might be, in an abstract way.

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

