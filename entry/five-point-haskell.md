Five Point Haskell
==================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/five-point-haskell.html)

Uhh I'm going to use this for planning. To make sure we don't accidentally use
the same example for all of them and we have enough to go around.

## Total Depravity

Idea: Any mistake/bug that could be made will eventually be made, don't trust
your mental modeling abilities

Theme: "Postmortems" of real world accidents, programming gore.

-   ID mixups (2022 Atlassian Outage)
-   Phantoms for environments (2017 Digital Ocean Outage)
-   Units (Mars Orbiter Failure)
-   Billion dollar problem --- sigil values
-   Use-after-free --- continuations, Acquire, ResourceT
-   Shotgun validation/parser inside database, accidentally save unvalidated
    data
-   Boolean blindness

## Unconditional Election

Idea: Parametrically polymorphic code will pre-destine what functions are
possible. Take advantage of it to enforce invariants and what you can write,
instead of using ie refinement types or postconditions.

Theme: Guessing game, what functions are allowed?

Be careful to not frame the programmer as the person who is electing the terms.
Frame it as the type system electing the valid terms

Give away your free will -- just be elective! constraint is liberation.

Oh that's the key: "Constraint is liberation"! it is not by works or foreseen
faith! the more you surrender....the safer you are! principle of least strength!
also can maybe combine with purity in multi-threaded. restrictions are better.

Principle of Least Strength

-   Parametric polymorphism --- guess the implementation
    -   `[Int] -> [Int]` vs `[a] -> [a]`
    -   Compare with refinement types
-   higher-kinded data, parametric over functors
-   Subtyping via parametric polymorphism
-   Phantoms + parametric polymoirphism, restrictions, ST trick
-   Princple of least strength, Monad vs Applicative
    -   typeclass-based limitation of functions

## Limited Atonement

Idea: Effects must be bounded, typed, named, disciplined (IO, ST, STM, StateT,
capabilities), the goal is not universal purity but rather correct partitioning

Theme: Extensible Effects, free monads, etc.

-   StateT instead of IO
-   ST to do mutation in vectors without full IO
-   STM does not allow IO
-   Free monads to describe exactly what actions you want
-   Free monads for mocking
-   Extensible effects to allow you selectively eliminate handlers until you are
    done
-   ReaderT vs global vars
-   Bracket lets you bound effects
-   Error monads vs IO exceptions

``{=html} ``{=html} ``{=html} ``{=html} ``{=html} ``{=html} ``{=html} ``{=html}
``{=html} ``{=html} ``{=html} ``{=html} ``{=html} ``{=html} ``{=html} ``{=html}
``{=html} ``{=html} ``{=html} ``{=html} ``{=html} ``{=html} ``{=html} ``{=html}
``{=html} ``{=html} ``{=html} ``{=html}

## Irresistible Grace

Idea: When you set up your type-safety correctly, the compiler forces you to
handle things appropriately

Distinction from Unconditional election: happens when writing functions, not
when writing types. I guess?

Theme: Sum type branches, GADTs and witnsses, handler based programming, church
encodings?

-   Sum type --- properly require every handler, or else the compiler complains
-   Lists mean you have to check for null
-   GADTs --- the type of the GADT can tell you what you need to handle. Message
    pattern, Expr pattern
-   Church encodings --- each continuation must be addressed
-   Typed holes to help program
-   Instance resolution to auto-derive programs, Deriving Via, etc.

Actually yeah maybe we should go all-in on Code Gen.

## Perseverance of the Saints

Idea: Benefits of Immutability

-   Immutability
-   Sharing --- all new data structures
-   Multi-threaded
-   Locks, STM

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

