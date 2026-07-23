Principle of Documentation as Liability
=======================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/principle-of-documentation-as-liability.html)

One of the more nuanced daily decisions I make writing Haskell for my day job is
how "strong" to make the types in my APIs. Haskell is very good at giving you
multiple levels and gradations of type safety (as I wrote about last year in
[Seven levels of type safety in
Haskell](https://blog.jle.im/entry/levels-of-type-safety-haskell-lists.html)),
so it seems Haskellers are bit spoiled for choice. Of course, everyone agrees
that you shouldn't have `String -> String -> String -> ..` stringly-typed code
or completely `Map String Value` json argument maps, and also that you shouldn't
go to the full extreme of having your preconditions be so exact that the user
ends up re-implementing the entire function just to be able to call it. But
in-between, there is some nuance.

Lately I've noticed I have been falling back to a certain principle/heuristic
that I have started calling the *"Principle of Documentation as Liability"*. The
basic ideas are:

1.  *All* non-code documentation, though sometimes useful, is a liability and
    should be treated with suspicion.
2.  Pre-conditions that require the user to read documentation to know about
    them are bad usability. As an author you should assume the user hasn't read
    or doesn't want to read it.
    -   So, make your argument types as self-evident as reasonably possible.
    -   Breaking changes in the pre-conditions should require breaking changes
        in the types --- stricter conditions should break compilation until
        addressed.
3.  Post-conditions that only exist in documentation cannot be trusted to be
    up-to-date. As a consumer you should assume it is out of date and must be
    verified at runtime.
    -   So, make the structure of your result types as self-evident as
        reasonably possible.
    -   Breaking changes in the semantics of the results should require a change
        in the types --- looser conditions should break compilation until
        addressed.
4.  Reserve documentation for explanation of design decisions, high-level usage
    instructions, example usages, etc.

## Documentation-Only Pre-Conditions are a Liability

Don't do it!

## No Post-Conditions in Documentation

Just don't!

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

