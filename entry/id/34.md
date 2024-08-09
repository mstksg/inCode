Introducing: the Auto library!

===============================

> Originally posted by [Justin Le](https://blog.jle.im/) on March 24, 2015.
> [Read online!](https://blog.jle.im/entry/introducing-the-auto-library.html)

**Auto**: [README](https://github.com/mstksg/auto/blob/master/README.md) (with
examples) / [hackage](http://hackage.haskell.org/package/auto) /
[tutorial](https://github.com/mstksg/auto/blob/master/tutorial/tutorial.md) /
[examples](https://github.com/mstksg/auto-examples) /
[github](https://github.com/mstksg/auto)

(Before anything, maybe take a quick look at the detailed description in the
[README](https://github.com/mstksg/auto/blob/master/README.md) for a quick
motivating example and explanation of the library)

Today I'm announcing and beginning promotion of my *auto* library, a denotative
and locally stateful programming DSL and platform, now [on
hackage](http://hackage.haskell.org/package/auto). *auto* is suitable when your
program involves an input or output that is a discrete stream of things ---
events, views, etc., like turn based games, GUI's, numerical computations...; it
allows you to state (possibly cyclic) complex relationships between streams of
values by composing simple, primitive ones. You can read the
[README](https://github.com/mstksg/auto/blob/master/README.md) too for a
detailed buzz-word laden exposition with nice well-commented short demos and
examples, get started with [the
tutorial](https://github.com/mstksg/auto/blob/master/tutorial/tutorial.md),
check out the directory of [sample
projects](https://github.com/mstksg/auto-examples), and even see a live running
[todoMVC](http://mstksg.github.io/auto-examples/todo/)
([source](https://github.com/mstksg/auto-examples/blob/master/src/Todo.hs))
example!

Over the next week or two I'm going to be breaking down real-world projects
written on *auto*, and even be talking about the design processes of programs
written using *auto*. You can follow along on [the series
page](http://blog.jle.im/entries/series/+all-about-auto), follow me on
[twitter](https://twitter.com/mstk "Twitter"), or just subscribe to the [rss
feed](http://blog.jle.im/rss) feed; expect a post on designing, from start to
finish,

1.  A fully running chat bot
2.  A GUI-based todo app on ghcjs
3.  A text-based adventure game a la the classic
    [rogue](http://en.wikipedia.org/wiki/Rogue_%28video_game%29)
4.  A numerical computation DSL

But enough of that...what is *auto*, why does it exist, and what are its design
principles?

## Auto

### on State

I designed *auto* because there really aren't any good solutions in Haskell for
declaratively describing locally stateful programs in a compositional way. And a
lack of denotational semantics to reason with them.

The go-to implementation for a turn-based game is to have a "giant state monad".
It is a clever "hack", but really, all we've done is began programming a game
with *global mutable state*. People have tried getting over this by using lenses
and zoomers, but this processes doesn't quite scale.

Even now many games, GUI's, numerical computations, etc. are written as folds or
state compositions over a giant state. Surely there is a better way?

*auto* provides denotational semantics for the composition and transformation of
*stream transformers*. At a high level, it is an example of "locally stateful
programming". Each component and stream transformer really operates as its own
entity, separate from the world, in a composable way. Typical programs involve
building a (possibly cyclic) graph of *relationships* between quantities over a
stream.

Composing two transformers side-by-side or end-to-end creates a new
transformer...and the state of each trasnformer is "closed off" from the other
and the rest of the world.

``` haskell
sumAndProduct = proc x -> do
    sums  <- sumFrom 0     -< x
    prods <- productFrom 1 -< x
    id -< sums + prods
```

`sumFrom 0` denotes a relationship between `x` and `sums` such that `sums` is
the cumulative sum of all `x`'s seen. `productFrom 1` denotes a relationship
between `x` and `prods` that `prods` is the cumulative product of all `x`s seen.
With `sumAndProduct`, we *built* a new relationship --- the output is the sum of
the cumulative sum and the cumulative product of the inputs --- by composing two
primitives.

``` haskell
-- running our Autos over the stream [1..10] to get a new stream
ghci> streamAuto' (sumFrom 0) [1..10]
[1,3, 6,10, 15, 21,  28,   36,    45,     55]
ghci> streamAuto' (productFrom 1) [1..10]
[1,2, 6,24,120,720,5040,40320,362880,3628800]
ghci> streamAuto' sumAndProduct [1..10]
[2,5,12,34,135,741,5068,40356,362925,3628855]
```

Each of them maintain their own "state"...and even `sumAndProduct` will maintain
its own internal state as you compose it with other things.

You build complex programs. For games, you might have an enemy monster, or a
player character...why should a player character's update be able to access the
state of the enemy monster? Why should the enemy monster be able to access the
state of the player? Now, the enemy monster's state is only accessible to the
enemy monster itself...nobody else can touch it.

In my opinion, this really is the only sane way to have stateful loops. Separate
out your update into the composition of primitives, and describe it using
denotative, pure *relationships*.

## on Architecture

Using *auto*, your entire program is structured as a stream transformer, built
from the composition of smaller primitives, and using the various combinators
and semantic streams of the library.

In the end, "running" it is simply gathering the input one at a time, pushing it
through, and acting on the result.

For example, in the [todo example](http://mstksg.github.io/auto-examples/todo/),
the entire application is just one `Auto` that takes a stream of input commands
and outputs a stream of output GUI views. It's launch so that the `Auto` waits
on a `Chan` queue to consume inputs as they come in. All the javascript
front-end has to do is render the output gui view, and hook up the DOM elements
to trigger events that add new inputs to the queue. That's it!

How do we build that `Auto`? By composing smaller, simpler ones. Each denoting
their own relationship, each keeping track of their state locally. The "full GUI
state" never *exists* anywhere...it is aggregated during composition.

By the way, *auto* does allow you to take "snap shots" of the actual states of
`Auto`s as they are run, as a binary...so you can serialize, freeze, and resume
`Auto`s from any previous state at-will. Free undos, *free save states*. And
this serialization **composes**, so the combination of two serialized `Auto`s
with internal state will also be serialized appropriately.

### on Comparisons

Throughout its development *auto* has been compared to FRP libraries like
netwire. A full address of this comparison is offered on [the
readme](https://github.com/mstksg/auto#relation-to-frp). The main difference is
that FRP offers an expressive language for working with, manipulating, and
transforming continuous-time behaviors. *auto* borrows some aspects of FRP as
well as some practical API aspects in order to build something separate,
manipulating and transforming causal (discrete) streams. There are many
situations where FRP is not quite suited --- it'd be like using a vector art
program to describe a bitmap. There are domains where *auto* is not suited ---
that is, the semantic model doesn't allow you to say anything meaningful about
continuous time behaviors.

*auto* has also been compared to pipes and conduit, but there are some major
differences in design and philosophy. pipes is a more general-purpose co-routine
library with an emphasis on being able to apply equational reasoning to many
different domains. conduit focuses around the problem of effective streaming
with resource management. Both work "sources" that come from underlying monads
like IO; *auto* discourages this except for disciplined exceptions, and it's
definitely not the norm. *auto* works as *value stream transformers*, working
with "pure" one-by-one transformations on streams of *values*; pipes and conduit
provide *effect stream manipulators*, managing streams of *effects* in constant
space, with resource management, etc...and often involving output effects as a
result ("consumers").[^1]

## on the Future

So, I'll be using this blog to post complete walk-throughs on designing specific
apps like the ones in the [examples](https://github.com/mstksg/auto-examples)
repo. Along the way hopefully some general design principles will become
apparent, too. I've been working on this library and have been writing "real
world" code for it for almost a year now, so I have some experience with design
practices and patterns that work and don't work.

For now, you can try reading over the
[tutorial](https://github.com/mstksg/auto/blob/master/tutorial/tutorial.md) if
you want, or peruse the [examples](https://github.com/mstksg/auto-examples)
repo!

Also, I definitely welcome any criticism on the design of the library or the
semantic model, or of its use cases. I've been more or less working on this
alone for almost a year, so now is the time for any delusions of mine to be
vetted out in public!

### on Support

For now, the official support channel is *#haskell-auto* on freenode (I'm
*jle\`*), but you can always use the [issue
tracker](https://github.com/mstksg/issues) too, [email
me](mailto:justin@jle.im), or find me on twitter as
[mstk](https://twitter.com/mstk "Twitter").

All this being said, *auto* is still kind of technically in a sorta pre-release
state, because not all of the tests are written yet. But the API should be
stable and updates before `0.3.x` are going to all be backwards compatible
(API-wise) bug fixes or filling in holes.

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

[^1]: One could still use a subset of pipes that does not stream effects, but
    merely values, and *that* does somewhat fill a similar role; this is used in
    the [mvc](https://hackage.haskell.org/package/mvc) library to build similar
    applications that *auto* tries to build. However, due to mvc's "global
    state" nature, you lose many of the local statefulness idioms in *auto*, and
    a lot of *auto*'s benefits and design philosophies go away, for other
    reasons as well.

