The Applicative Interpreter Combinator Design Pattern

======================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/interpreter-combinators.html)

Recently I've been having a lot of fun with what I have been calling the
"Applicative Interpreter Combinator" design pattern. It is heavily influenced by
ideas like [Data types a la
Carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf) and
[unified free monoidal
functors](http://oleg.fi/gists/posts/2018-02-21-single-free.html), but the end
goal is slightly different in spirit.

The goal is to represent Applicative (typically non-monadic) computations
(things like parsers, things to execute, things to consume or produce data) by
assembling "self-evident" basic primitives and subjecting them to many
*different* successive transformations and combiners. The process of doing so:

1.  Forces you to make explicit decisions about the structure of your
    computation type as an ADT.
2.  Allows you to retain isolation of fundamental parts of your domain as
    separate types
3.  Lets you manipulate the structure of your final computation type through
    *normal Haskell techniques* like pattern matching. The structure is
    available throughout the entire process, so you can replace individual
    components and values within your structure.
4.  Allows you to fully *reflect* the structure of your final computation
    through pattern matching and folds, so you can inspect the structure and
    produce useful summaries.

Like "data types a la carte" and free monad/applicative/alternative designs,
these techniques allow you to separate the assembly and inspection of your
programs from the "running" of them. However, the main difference is that here
we focus not just on products and sums, but many different varied and
multi-purpose combinators --- a bona fide "zoo" of combinators. Furthermore, our
goal is not to design a functor that we can throw into `Fix` or `Free` in the
end. We might use a fixed-point or two, not as a "big picture", but rather as an
intermediate step. The *functor itself* is the goal, *not* its fixed point.

This post will be a tour of many different combinators (taken from all over the
Haskell ecosystem --- places like
[kan-extensions](https://hackage.haskell.org/package/kan-extensions),
[transformers](https://hackage.haskell.org/package/transformers),
[free](https://hackage.haskell.org/package/free), and even
[base](https://hackage.haskell.org/package/base)) and try to compile and compare
them in a systematic way. We'll be looking at how they act on a couple of base
primitives, and seeing the effect that each one has on our primitives.

## Setting the Playing Field

First, let's set up our base primitive functors that we will be playing around
with and seeing how all of these primitives are affected by our combinators.

In the end, we're going to be building a *command line options schema*, which we
can run as a parser or summarize.

A command line options schema has two basic parts:

-   *Positional arguments* (think `mv <src> <dest>`)
-   *Options* (think `ls --all`)

These two will be the building blocks of our parser!

These blocks will represent *schemas* for building a command line argument
parser (or otherwise). They will have kind `Type -> Type` (that is, they will
take one type parameter, or at least be able to be partially applied to that
point), and the type parameter represents "what" the schema parses. They will
*usually* be `Functor` instances, for convenience...but not necessarily always.

### Arg

First, a Functor to represent a schema for a positional argument:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/applicative-interp.hs#L27-L32

data Arg a = Arg
    { argName  :: String
    , argHelp  :: String
    , argRead  :: ReadM a
    }
  deriving Functor
```

An `Arg a` will be a schema describing an argument that parses a value of type
`a`. So, an `Arg Int` would be an `Int` argument retrieved from the command
line.

A schema describing an argument that parses a value of type `a` contains a name,
a help message, and a `ReadM a`, which is *optparse-applicative*'s string parser
data type (it contains information on how to parse a `String` into an `a`). For
the most part, we only need to care about two `ReadM a`s,
`auto :: Read a => ReadM a`, a `ReadM` that works for all `Read` instances, and
`str :: IsString s => ReadM a`, a `ReadM` that works for all string-like types
(like `String` and `Text`).

Let's define two simple *interpreters* for this schema primitive.

An *interpreter* is a *natural transformation* from our *schema* to some other
functor that we will "execute" our primitive in. A *natural transformation*
between functors `F` and `G` is a function `forall a. F a -> G a`, that works
for *all* `a`s. An interpreter will basically *interpret a schema*.

Here's a simple interpreter that executes our argument into a "summary
aggregator", which aggregates information into a list of summary lines:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/applicative-interp.hs#L25-L50

type Summary = Const [String]

argSummary :: Arg a -> Summary a
argSummary Arg{..} = Const [ argName ++ ": " ++ argHelp ]
```

Here we using the `-XRecordWildcards` extension to bind all of the fields in
`Arg` for us to use, for convenience.

We build an "action" in the `Summary` type, where we just log a single help
line. The `Summary` Applicative is a data type containing a list of strings,
where the sequencing of `Summary` actions is the appending of those strings
together. More on this later!

For example, we'll make a test `Arg` that parses a name:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/applicative-interp.hs#L76-L81

nameArg :: Arg String
nameArg = Arg
    { argName = "<name>"
    , argHelp = "A person's name"
    , argRead = str
    }
```

Let's run it:

``` haskell
ghci> argSummary nameArg
Const ["<name>: A person's name"]
```

Okay, that's a simple one. How about a slightly more complicated one? We can
define an interpreter into an *optparse-applicative* command line argument
parser:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/applicative-interp.hs#L52-L55

argParser :: Arg a -> Parser a
argParser Arg{..} = argument argRead $
       help    argHelp
    <> metavar argName
```

This how you use *optparse-applicative* to describe a parser with a single
argument. When we "run" it, it will parse it as a single positional argument
using the `ReadM`.

To see this in action, let's create a handy tester:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/applicative-interp.hs#L97-L100

testParser :: Parser a -> String -> IO a
testParser p = handleParseResult
             . execParserPure defaultPrefs (info (p <**> helper) mempty)
             . words
```

We can now test out `nameArg`:

``` haskell
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

``` haskell
ghci> testParser (argParser (map toUpper <$> nameArg)) "carol"
"CAROL"
```

### Opt

Now, let's define `Opt`, schema for non-positional `--option <blah>`s in a
command line interface. We can do this pretty much the same way:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/applicative-interp.hs#L34-L40

data Opt a = Opt
    { optString :: String
    , optHelp   :: String
    , optMeta   :: String
    , optRead   :: ReadM a
    }
  deriving Functor
```

An `Opt a` is a schema describing an option "flag" that expects a value of type
`a`.

Again, we'll lay out our interpreters:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/applicative-interp.hs#L57-L65

optSummary :: Opt a -> Summary a
optSummary Opt{..} = Const
    [ "--" ++ optString ++ " " ++ optMeta ++ ": " ++ optHelp ]

optParser :: Opt a -> Parser a
optParser Opt{..} = option optRead $
       long optString
    <> help optHelp
    <> metavar optMeta
```

Here's a sample `Opt` getting a person's age:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/applicative-interp.hs#L83-L89

ageOpt :: Opt Int
ageOpt = Opt
    { optString = "age"
    , optHelp   = "A person's age"
    , optMeta   = "<int>"
    , optRead   = auto
    }
```

``` haskell
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

### Imagining the Combinations

Now that we laid out our basic schemas, let's now think about how we might want
to *combine* them into richer schemas. How about:

-   A schema that can have multiple `Arg`s
-   A schema that can have multiple `Opt`s
-   A schema that can take a single *optional* `Opt`. ``{=html}
-   A schema that can have a single `Arg`, and multiple `Opt`s (or vice versa)
-   A schema that has different `Opt`s and `Arg`s according to different
    subcommands
-   A schema that specifies an `Arg` or an `Opt`, but not both.
-   A schema that specifies both an `Arg` and an `Opt`, but where the
    interpreting function has the option to pick which one
-   A schema that specifies both an `Arg` and an `Opt`, but where the
    interpreting function *must* present both to the user.

Think about all of the interesting schemas you could build using a combination
of `Arg` and `Opt`. Now, let's see what tools we have at our disposal!

## Combining Schemas

One simple thing we can imagine is *combining* different schema types in
different ways to produce new schemas, compositionally. There are a few ways we
can imagine combining two different schemas together, and we have different
*combinators* to describe each different way.

A "schema-combining combinator" will have kind:

``` haskell
(Type -> Type) -> (Type -> Type) -> (Type -> Type)
```

That is, given two different `Type -> Type`s, provide a new `Type -> Type`.

All of these combinators should ideally be associative, and there should be an
*identity* schema where combining a schema with the identity should return the
original schema.

### Either-Or

The first one we have is `Sum` from
*[Data.Functor.Sum](https://hackage.haskell.org/package/base/docs/Data-Functor-Sum.html)*;
however, I like to use the equivalent type `:+:` from
*[GHC.Generics](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html)*.

`Arg :+: Opt` is a schema that can either ask for an `Arg` *or* an `Opt`.

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

