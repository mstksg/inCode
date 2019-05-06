---
title: The Applicative Interpreter Combinator Design Pattern
categories: Haskell
tags: functional programming, free, haskell, interpreters
create-time: 2019/05/04 15:07:15
identifier: applicative-interpreters
slug: interpreter-combinators
---

Recently I've been having a lot of fun with what I have been calling the
"Applicative Interpreter Combinator" design pattern.  It is heavily influenced
by ideas like [Data types a la Carte][dtalc] and [unified free monoidal
functors][ufmf], but the end goal is slightly different in spirit.

[dtalc]: http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf
[ufmf]:  http://oleg.fi/gists/posts/2018-02-21-single-free.html

The goal is to represent Applicative (typically non-monadic) computations
(things like parsers, things to execute, things to consume or produce data) by
assembling "self-evident" basic primitives and subjecting them to many
*different* successive transformations and combiners.  The process of doing so:

1.  Forces you to make explicit decisions about the structure of your
    computation type as an ADT.
2.  Allows you to retain isolation of fundamental parts of your domain as
    separate types
3.  Lets you manipulate the structure of your final computation type through
    *normal Haskell techniques* like pattern matching.  The structure is
    available throughout the entire process, so you can replace individual
    components and values within your structure.
4.  Allows you to fully *reflect* the structure of your final computation
    through pattern matching and folds, so you can inspect the structure and
    produce useful summaries.

Like "data types a la carte" and free monad/applicative/alternative designs,
these techniques allow you to separate the assembly and inspection of your
programs from the "running" of them.  However, the main difference is that here
we focus not just on products and sums, but many different varied and
multi-purpose combinators --- a bona fide "zoo" of combinators.  Furthermore,
our goal is not to design a functor that we can throw into `Fix` or `Free` in
the end.  We might use a fixed-point or two, not as a "big picture", but rather
as an intermediate step.  The *functor itself* is the goal, *not* its fixed
point.

This post will be a tour of many different combinators (taken from all over the
Haskell ecosystem --- places like [kan-extensions][], [transformers][],
[free][], and even [base][]) and try to compile and compare them in a
systematic way. We'll be looking at how they act on a couple of base
primitives, and seeing the effect that each one has on our primitives.

[kan-extensions]: https://hackage.haskell.org/package/kan-extensions
[transformers]: https://hackage.haskell.org/package/transformers
[free]: https://hackage.haskell.org/package/free
[base]: https://hackage.haskell.org/package/base

# Setting the Playing Field

First, let's set up our base primitive functors that we will be playing around
with and seeing how all of these primitives are affected by our combinators.

In the end, we're going to be building a *command line options schema*, which
we can run as a parser or summarize.

A command line options schema has two basic parts:

*   *Positional arguments* (think `mv <src> <dest>`)
*   *Options* (think `ls --all`)

These two will be the building blocks of our parser!

First, a Functor to represent a schema for a positional argument:

```haskell
!!!misc/applicative-interp.hs "data Arg a"
```

A schema describing an argument that parses a value of type `a` contains a
name, a help message, and a `ReadM a`, which is *optparse-applicative*'s string
parser data type (it contains information on how to parse a `String` into an
`a`).  For the most part, we only need to care about two `ReadM a`s, `auto ::
Read a => ReadM a`, a `ReadM` that works for all `Read` instances, and `str ::
IsString s => ReadM a`, a `ReadM` that works for all string-like types (like
`String` and `Text`).

Let's define a simple interpreter for this primitive, creating an
*optparse-applicative* parser:

```haskell
!!!misc/applicative-interp.hs "argParser ::"
```

Here we using the `-XRecordWildcards` extension to bind all of the fields in
`Arg` for us to use, for convenience.  This is just *optparse-applicative*'s
method of describing parser with a single argument, and when we "run" it, it
will parse it as a single positional argument using the `ReadM`.

For example, we'll make a test `Arg` that parses a name:

```haskell
!!!misc/applicative-interp.hs "nameArg ::"
```

And we'll create handy tester:

```haskell
!!!misc/applicative-interp.hs "testParser ::"
```

We can now test it out:

```haskell
ghci> testParser (argParser nameArg) "--help"
-- Usage: <interactive> <name>
--
-- Available options:
--   <name>                   A person's name
--   -h,--help                Show this help text
ghci> testParser (argParser nameArg) "alice"
-- "alice"
ghci> testParser (argParser nameArg) "bob"
-- "bob"
```

So if we enter a single positional argument, it gets parsed as itself.

Note that `Arg` is a `Functor`, so we can fmap a transformation on the result:

```haskell
ghci> testParser (argParser (map toUpper <$> nameArg)) "carol"
"CAROL"
```


# Sums

# Products

# Convolutions

# Compositions

<!-- # Single Functor Transformers -->

<!-- # Two-functor Transformers -->

<!-- # Monadic Transformers -->


<!-- I'm sure I'm not the first person to -->
<!-- use this type of design and I'm not the first person to study them, -->
<!-- kkkjjjjjjjjjjjjjjjjjjjjjjk -->

<!-- I had the pleasure of working with both the *[servant][]* and -->
<!-- *[optparse-applicative][]* libraries recently, which culminated in the -->
<!-- *[servant-cli][]* library.  At first, I did everything by directly manipulating -->
<!-- a `Parser` type, which is the type of command line argument parsers from -->
<!-- *optparse-applicative*.  However, I ran into an issue very quickly: I needed to -->
<!-- be able to manipulate the *structure* of a command line parser directly --- -->
<!-- deleting and modifying commands, the desire to be able to pattern match, -->
<!-- reflect on the arguments needed, move arguments around, etc.  By working -->
<!-- directly with `Parser`, I threw everything into a black box that I could not -->
<!-- easily inspect.  Have you ever felt like this with IO, or any other -->
<!-- Applicative (parsers or otherwise) you've had to use? -->

<!-- [servant]: https://hackage.haskell.org/package/servant -->
<!-- [optparse-applicative]: https://hackage.haskell.org/package/optparse-applicative -->
<!-- [servant-cli]: https://hackage.haskell.org/package/servant-cli -->

<!-- I realized that I needed to make an algebraic data type that could represent -->
<!-- the *structure* of a command line parser.  I needed an ADT that I could: -->

<!-- *   Pattern match on to modify, shift, and re-arrange components of the parser -->
<!-- *   Reflect to inspect what sort of arguments the parser will ask for, what -->
<!--     command line options are being asked for, etc. -->
<!-- *   Build from on very simple components that I could combine together in a -->
<!--     logical way with semantic combinators. -->
<!-- *   Finally, "run" as an *optparse-applicative* combinator, an interactive -->
<!--     wizard, or generate rich documentation, etc. -->

<!-- During this journey, I ended up falling in love with a pattern I am calling -->
<!-- "Applicative Interpreters a la Carte", that I believe is unique to -->
<!-- Functor and Applicative (that is, explicitly non-monadic) interpreters. -->

<!-- In this post we'll be creating such a structure from simple components using -->
<!-- Applicative Interpreter Combinators such as `Day`, `:*:`, `:+:`, `:*:`, `Ap` -->
<!-- (the free Applicative), `Alt` (the free Alternative), `Coyoneda` (the free -->
<!-- Functor), `Lift` (the free Pointed), and more! We'll also explore fundamental -->
<!-- principles of each combinator as they relate to a bigger picture. -->

<!-- The Goal -->
<!-- -------- -->

<!-- Let's define the overall structure of our command line menu: We will have a -->
<!-- hierarchy of nested sub-command menus that we can dig down.  We can go down a -->
<!-- menu by specifying a "command" (with a catch-all wildcard command), and once we -->
<!-- get down to the action we want, we can finish up by specifying `--option`s. -->

<!-- For example, we can make a command line tool like "git" -->

<!-- * -->
