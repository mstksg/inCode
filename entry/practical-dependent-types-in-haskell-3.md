Practical Dependent Types: A Deeper Look at Proofs

===================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/practical-dependent-types-in-haskell-3.html)

Hi! This will be the final post of the [dependently typed neural network
series](https://blog.jle.im/entries/series/+practical-dependent-types-in-haskell.html).
This post has been long put-off because there really wasn't *too* much more we
can learn about dependent types and programming with dependent types from this
example. Still, this example still has a bit more to offer us, so let's take one
last look :) Think of this post as an epilogue.

In [Part
1](https://blog.jle.im/entry/practical-dependent-types-in-haskell-1.html), we
tried solving our problem in an "untyped" way, recognized all of those Haskell
red flags, and then added types to make things safer and reap benefits in API
clarity, compiler support, and a smoother code writing experience. In [Part
2](https://blog.jle.im/entry/practical-dependent-types-in-haskell-2.html), we
looked at how integrating our typed programs with the real world, where types
might vary at runtime, by levering existential types in two different forms.

## The Secret Life of Proofs

So far in this journey, we've hand-waved the idea of "proofs", and their role in
this dependently typed programming. In fact, I've sort of intentionally left the
idea vague, because I feel like a lot of other dependent type
introductions/tutorials over-emphasize proofs, to the point there is a public
impression that [dependently typed programming is all about
proofs](https://www.reddit.com/r/haskell/comments/62uv6g/verify_your_typeclass_instances_in_haskell_today/dfpt2g7/):

> I think dependent types are pretty cool, but honestly the sheer difficulty
> jump from quickcheck tests to actual proofs to me seems a bit too massive to
> justify having to deal with them much.
>
> I think if we just had testable functions (basically just predicates that
> should always return true) alongside classes that could be quickchecked
> against. And actually took it very seriously when an instance isn't lawful
> (basically just disallow it almost always), then I think we would be in a
> pretty darn good spot.

There's a popular (mis?)conception that dependent types are basically a
replacement for QuickCheck, or refinement types (like Liquid Haskell). But
really, as we have seen, they are much more --- they let you leverage the
compiler to help you write your code, help you enforce properties at the
structural level for your types, create more expressive API's, and much more.
They aren't "just" to prove properties about your program.

In reality, the idea of dependent types and the idea of proofs sort of are
separate but complementing ideas. Dependent types enable this "proofy" style
discussed in the comment, but, also, the ability to dynamically construct and
manipulate proofs opens up many doors for the capabilities of dependently typed
programs.

### What are they?

In the context of dependent types (in Haskell), a "proof" is general
(non-rigorous, informal) term for a *runtime value* that GHC/Haskell can use for
*type-level* shenanigans. In other words, it's a *term-level* witness for a
*type-level* truth.

It's the critical *run-time* values that we use to power our *type-level* fun,
and are also commonly known as "witnesses".

I don't believe there is any rigorous definition, but the term comes up whenever
we talk about generating or using run-time values that give us type-level power.

In this article, the simplest example of something that can be called a proof or
a witness is the singleton for `Nat`s: A value of type `Sing n` (for `Nat n`)
"witnesses" a `KnownNat n` constraint.

We saw it earlier when we wanted to pass along evidence that our `n` has an
instance of `KnownNat`:

``` haskell
nIsKnownNatWePromise :: forall n. Sing n -> Integer
nIsKnonwNatWePromise = \case
    SNat -> natVal (Proxy :: Proxy n)
```

In the case statement above, in the `SNat ->` branch, GHC knows that `n` has a
`KnownNat` instance. This runtime value, `SNat`, carries the fact that we have
an instance of `KnownNat n`. Remember that
`natVal :: KnownNat n => p n -> Integer` -- it only works of `n` is an instance
of `KnownNat`. `SNat :: Sing n` is a runtime value that "gives us" that
instance. It's a "proof" that `n` is an instance of `KnownNat`.

### First-Class Values

The *key* to their usefulness is that, as runtime values, they are *first-class
values* in Haskell. We can manipulate them, pass them, construct them, etc.,
just as if they were normal values. You can construct complex proofs from simple
ones, compose them, etc.

For example, check out the function `%:+` from the *singletons* package:

``` haskell
(%:+) :: forall (n :: Nat) (m :: Nat). Sing n -> Sing m -> Sing (n + m)
```

Given, at runtime, a witness for `KnownNat n` (our `Sing n`) and a witness for
`KnownNat m`, we can construct, at runtime a witness for `KnownNat (n + m)`:

``` haskell
add :: Integer -> Integer -> Integer
add x y = withSomeSing x $ \(sx :: Sing x) ->
          withSomeSing y $ \(sy :: Sing y) ->
            case sx %:+ sy of
              -- z is x + y
              (SNat :: Sing z) -> natVal (Proxy :: Proxy z)
```

``` haskell
ghci> add 5 7
12
```

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

