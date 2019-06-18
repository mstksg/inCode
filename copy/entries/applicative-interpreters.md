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

Setting the Playing Field
-------------------------

First, let's set up our base primitive functors that we will be playing around
with and seeing how all of these primitives are affected by our combinators.

In the end, we're going to be building a *command line options schema*, which
we can run as a parser or summarize.

A command line options schema has two basic parts:

*   *Positional arguments* (think `mv <src> <dest>`)
*   *Options* (think `ls --all`)

These two will be the building blocks of our parser!

These blocks will represent *schemas* for building a command line argument
parser (or otherwise).  They will have kind `Type -> Type` (that is, they will
take one type parameter, or at least be able to be partially applied to that
point), and the type parameter represents "what" the schema parses.  They will
*usually* be `Functor` instances, for convenience...but not necessarily always.

### Arg

First, a Functor to represent a schema for a positional argument:

```haskell
!!!misc/applicative-interp.hs "data Arg a"
```

An `Arg a` will be a schema describing an argument that parses a value of type
`a`.  So, an `Arg Int` would be an `Int` argument retrieved from the command
line.

A schema describing an argument that parses a value of type `a` contains a
name, a help message, and a `ReadM a`, which is *optparse-applicative*'s string
parser data type (it contains information on how to parse a `String` into an
`a`).  For the most part, we only need to care about two `ReadM a`s, `auto ::
Read a => ReadM a`, a `ReadM` that works for all `Read` instances, and `str ::
IsString s => ReadM a`, a `ReadM` that works for all string-like types (like
`String` and `Text`).

Let's define two simple *interpreters* for this schema primitive.

An *interpreter* is a *natural transformation* from our *schema* to some other
functor that we will "execute" our primitive in.  A *natural transformation*
between functors `F` and `G` is a function `forall a. F a -> G a`, that works
for *all* `a`s.  An interpreter will basically *interpret a schema*.

Here's a simple interpreter that executes our argument into a "summary
aggregator", which aggregates information into a list of summary lines:

```haskell
!!!misc/applicative-interp.hs "type Summary" "argSummary ::"
```

Here we using the `-XRecordWildcards` extension to bind all of the fields in
`Arg` for us to use, for convenience.

We build an "action" in the `Summary` type, where we just log a single help
line.  The `Summary` Applicative is a data type containing a list of strings,
where the sequencing of `Summary` actions is the appending of those strings
together.  More on this later!

For example, we'll make a test `Arg` that parses a name:

```haskell
!!!misc/applicative-interp.hs "nameArg ::"
```

Let's run it:

```haskell
ghci> argSummary nameArg
Const ["<name>: A person's name"]
```

Okay, that's a simple one.  How about a slightly more complicated one?  We can
define an interpreter into an *optparse-applicative* command line argument
parser:

```haskell
!!!misc/applicative-interp.hs "argParser ::"
```

This how you use *optparse-applicative* to describe a parser with a single
argument.  When we "run" it, it will parse it as a single positional argument
using the `ReadM`.

To see this in action, let's create a handy tester:

```haskell
!!!misc/applicative-interp.hs "testParser ::"
```

We can now test out `nameArg`:

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

### Opt

Now, let's define `Opt`, schema for non-positional `--option <blah>`s in a
command line interface.   We can do this pretty much the same way:

```haskell
!!!misc/applicative-interp.hs "data Opt"
```

An `Opt a` is a schema describing an option "flag" that expects a value of type
`a`.

Again, we'll lay out our interpreters:

```haskell
!!!misc/applicative-interp.hs "optSummary ::" "optParser ::"
```

Here's a sample `Opt` getting a person's age:

```haskell
!!!misc/applicative-interp.hs "ageOpt ::"
```

```haskell
ghci> optSummary ageOpt
Const ["--age <int>: A person's age"]
ghci> testParser (optParser ageOpt) "--help"
-- Usage: <interactive> --age <int>
--
-- Available options:
--   --age <int>              A person's age
--   -h,--help                Show this help text
ghci> testParser (optParser ageOpt) "--age 25"
-- 25
ghci> testParser (optParser ((*2) <$> ageOpt)) "--age 25"
-- 50
```

<!-- ### Flag -->

<!-- One final special interpreter, representing flags that can either be "on" -->
<!-- (`True`) or "off" (`False`).  We're going to be implementing this using a GADT, -->
<!-- which means it can't directly be a `Functor`, for demonstrative purposes: -->

<!-- ```haskell -->
<!-- !!!misc/applicative-interp.hs "data Flag" -->
<!-- ``` -->

<!-- A `Flag a` is a schema describing a command line "flag" (an option that is -->
<!-- either "on" or "off"). -->

<!-- If you aren't familiar with GADT syntax, here it's being used to say that if -->
<!-- you use the `Flag` constructor, it'll return a `Flag Bool` --- specifically -->
<!-- `Bool`, and not any other type.  Here we use a GADT to fix a type with a -->
<!-- constructor.   If you use the `Flag` constructor, you will get a `Flag Bool`: a -->
<!-- schema describing a command line flag producing a `Bool`. -->

<!-- Our interpreters look straightforward again: -->

<!-- ```haskell -->
<!-- !!!misc/applicative-interp.hs "flagSummary ::" "flagParser ::" -->
<!-- ``` -->

<!-- Note that the type `Flag a -> Parser a` means that whatever the `a` is, you -->
<!-- haver to parse a value of that type.  So, if we pattern match on the `Flag` -->
<!-- constructor, we know that `a` must be `Bool`, so we can return a `Parser Bool` -->
<!-- (which is what `switch`, from *optparse-applicative*, returns). -->

<!-- Here is an example of a flag indicating whether or not a person has pets: -->

<!-- ```haskell -->
<!-- !!!misc/applicative-interp.hs "petsFlag ::" -->
<!-- ``` -->

<!-- ```haskell -->
<!-- ghci> flagSummary petsFlag -->
<!-- Const ["--pets: Has pets"] -->
<!-- λ: testParser (flagParser petsFlag) "--help" -->
<!-- -- Usage: <interactive> [--pets] -->
<!-- -- --> 
<!-- -- Available options: -->
<!-- --   --pets                   Has pets -->
<!-- --   -h,--help                Show this help text -->
<!-- -- *** Exception: ExitSuccess -->
<!-- λ: testParser (flagParser petsFlag) "--pets" -->
<!-- True -->
<!-- λ: testParser (flagParser petsFlag) "" -->
<!-- False -->
<!-- ``` -->

<!-- Note that unlike `Arg` and `Opt`, `Flag` cannot be a `Functor`.  That's because -->
<!-- fundamentally, a `Functor` must be able to support any type argument...but you -->
<!-- can only ever produce a `Flag Bool`, and never a `Flag Int` or `Flag String`. -->
<!-- There are no possible constructors!  But we'll soon see a way to deal with this -->
<!-- and make `Flag` effectively a `Functor`. -->

### Imagining the Combinations

Now that we laid out our basic schemas, let's now think about how we might
want to *combine* them into richer schemas.  How about:

*   A schema that can have multiple `Arg`s
*   A schema that can have multiple `Opt`s
*   A schema that can take a single *optional* `Opt`.
<!-- *   A schema that can have multiple `Flag`s -->
*   A schema that can have a single `Arg`, and multiple `Opt`s (or vice versa)
*   A schema that has different `Opt`s and `Arg`s according to different
    subcommands
*   A schema that specifies an `Arg` or an `Opt`, but not both.
*   A schema that specifies both an `Arg` and an `Opt`, but where the
    interpreting function has the option to pick which one
*   A schema that specifies both an `Arg` and an `Opt`, but where the
    interpreting function *must* present both to the user.

Think about all of the interesting schemas you could build using a combination
of `Arg` and `Opt`.  Now, let's see what tools we have at our
disposal!

Combining Schemas
-----------------

One simple thing we can imagine is *combining* different schema types in
different ways to produce new schemas, compositionally.  There are a few ways
we can imagine combining two different schemas together, and we have different
*combinators* to describe each different way.

A "schema-combining combinator" will have kind:

```haskell
(Type -> Type) -> (Type -> Type) -> (Type -> Type)
```

That is, given two different `Type -> Type`s, provide a new `Type -> Type`.

All of these combinators should ideally be associative, and there should be an
*identity* schema where combining a schema with the identity should return the
original schema.

### Either-Or

The first one we have is `Sum` from *[Data.Functor.Sum][]*; however, I like to
use the equivalent type `:+:` from *[GHC.Generics][]*.

[Data.Functor.Sum]: https://hackage.haskell.org/package/base/docs/Data-Functor-Sum.html
[GHC.Generics]: https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html

`Arg :+: Opt` is a schema that can either ask for an `Arg` *or* an `Opt`.



<!-- Schema Enhancers -->
<!-- ---------------- -->

<!-- Schema Enhancers -->
<!-- ---------------- -->

<!-- First of all, let's look at ways we can enhance *single* schemas. -->

<!-- Schema Sums -->
<!-- ----------- -->

<!-- One way we can create a "combined schema type" is by saying "either-or".  We -->
<!-- provide the user with the ability to use *one or the other* as a part of their -->
<!-- schema. -->


<!-- Products -->
<!-- -------- -->

<!-- Sums -->
<!-- ---- -->

<!-- Products -->
<!-- -------- -->

<!-- Convolutions -->
<!-- ------------ -->

<!-- Compositions -->
<!-- ------------ -->

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
