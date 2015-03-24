Introducing: the Auto library!
==============================

Categories
:   Haskell
:   Ramblings
:   Projects
:   Auto
Tags
:   auto
:   haskell
CreateTime
:   2015/03/24 01:02:06
PostDate
:   2015/03/24 10:30:57
Series
:   All About Auto
Identifier
:   auto-intro

**Auto**: [hackage][auto] / [README][] / [tutorial][] /
[examples][auto-examples] / [github][autogh]

Today I'm announcing and beginning promotion of my *auto* library, a
denotative and locally stateful programming DSL and platform, now [on
hackage][auto].  *auto* is suitable when your program involves an input or
output that is a discrete stream of things --- events, views, etc., like turn
based games, GUI's, numerical computations...; it allows you to state
(possibly cyclic) complex relationships between streams by composing simple,
primitive ones.  You can read the [README] too for a detailed buzz-word laden
exposition with nice well-commented short demos and examples, get started with
[the tutorial][tutorial], check out the directory of [sample
projects][auto-examples], and even see a live running [todoMVC][]
([source][todosrc]) example!

[auto]: http://hackage.haskell.org/package/auto
[README]: https://github.com/mstksg/auto/blob/master/README.md
[tutorial]: https://github.com/mstksg/auto/blob/master/tutorial/tutorial.md
[auto-examples]: https://github.com/mstksg/auto-examples
[todoMVC]: http://mstksg.github.io/auto-examples/todo/
[todosrc]: https://github.com/mstksg/auto-examples/blob/master/src/Todo.hs
[autogh]: https://github.com/mstksg/auto

Over the next week or two I'm going to be breaking down real-world projects
written on *auto*, and even be talking about the design processes of programs
written using *auto*.  You can follow along on [the series page][series], or
just subscribe to the [rss feed][rss] feed; expect a post on designing, from
start to finish,

1.  A fully running chat bot
2.  A GUI-based todo app on ghcjs
3.  A text-based adventure game a la the classic [rogue][]
4.  A numerical computation DSL

[series]: http://blog.jle.im/entries/series/+all-about-auto
[rss]: http://blog.jle.im/rss
[rogue]: http://en.wikipedia.org/wiki/Rogue_%28video_game%29

But enough of that...what is *auto*, why does it exist, and what are its design
principles?

Auto
----

### on State

I designed *auto* because there really aren't any good solutions in Haskell
for declaratively describing locally programs in a compositional way.  And a
lack of denotational semantics to reason with them.

The go-to implementation for a turn-based game is to have a "giant state
monad".  It is a clever "hack", but really, all we've done is began
programming a game with *global mutable state*.  Why have we done this to
ourselves?  Why progress back before language design principles of the 1960's?
People have tried getting over this by using lenses and zoomers, but this
processes doesn't quite scale.

Even now many games, GUI's, numerical computations, etc. are written as
folds or state compositions over a giant state.  Surely there is a better way?

*auto* provides denotational semantics for the composition and transformation
of *stream transformers*.  At a high level, it is an example of "locally
stateful programming".  Each component and stream transformer really operates
as its own entity, separate from the world, in a composable way.  Typical
programs involve building a (possibly cyclic) graph of *relationships* between
quantities over a stream.

Composing two transformers side-by-side or end-to-end creates a new
transformer...and the state of each trasnformer is "closed off" from the other
and the rest of the world.

~~~haskell
sumAndProduct = proc x -> do
    sums  <- sumFrom 0     -< x
    prods <- productFrom 1 -< x
    id -< sums + prods
~~~

`sumFrom 0` denotes a relationship between `x` and `sums` such that `sums` is
the cumulative sum of all `x`'s seen.  `productFrom 1` denotes a relationship
between `x` and `prods` that `prods` is the cumulative product of all `x`s
seen.  With `sumAndProduct`, we *built* a new relationship --- the output is
the sum of the cumulative sum and the cumulative product of the inputs --- by
composing two primitives.

~~~haskell
-- running our Autos over the stream [1..10] to get a new stream
ghci> streamAuto' (sumFrom 0) [1..10]
[1,3, 6,10, 15, 21,  28,   36,    45,     55]
ghci> streamAuto' (productFrom 1) [1..10]
[1,2, 6,24,120,720,5040,40320,362880,3628800]
ghci> streamAuto' sumAndProduct [1..10]
[2,5,12,34,135,741,5068,40356,362925,3628855]
~~~

Each of them maintain their own "state"...and even
`sumAndProduct` will maintain its own internal state as you compose it with
other things.

You build complex programs.  For games, you might have an enemy monster, or a
player character...why should a player character's update be able to access
the state of the enemy monster?  Why should the enemy monster be able to
access the state of the player?  Now, the enemy monster's state is only
accessible to the enemy monster itself...nobody else can touch it.

In my opinion, this really is the only sane way to have stateful loops.
Separate out your update into the composition of primitives, and describe it
using denotative, pure *relationships*.

on Architecture
---------------

Using *auto*, your entire program is structured as a stream transformer, built
from the composition of smaller primitives, and using the various combinators
and semantic streams of the library.

In the end, "running" it is simply gathering the input one at a time, pushing
it through, and acting on the result.

For example, in the [todo example][todoMVC], the entire application is just
one `Auto` that takes a stream of input commands and outputs a stream of
output GUI views.  It's launch so that the `Auto` waits on a `Chan` queue to
consume inputs as they come in.  All the javascript front-end has to do is
render the output gui view, and hook up the DOM elements to trigger events
that add new inputs to the queue.  That's it!

How do we build that `Auto`?  By composing smaller, simpler ones.  Each
denoting their own relationship, each keeping track of their state locally.
The "full GUI state" never *exists* anywhere...it is aggregated during
composition.

By the way, *auto* does allow you to take "snap shots" of the actual states of
`Auto`s as they are run, as a binary...so you can serialize, freeze, and
resume `Auto`s from any previous state at-will.  Free undos, free save states.

on the Future
-------------

So, I'll be using this blog to post complete walk-throughs on designing
specific apps like the ones in the [examples][auto-examples] repo.  Along the
way hopefully some general design principles will become apparent, too.  I've
been working on this library and have been writing "real world" code for it
for almost a year now, so I have some experience with design practices and
patterns that work and don't work.

For now, you can try reading over the [tutorial][] if you want, or peruse the
[examples][auto-examples] repo!

### on Support

For now, the official support channel is *#haskell-auto* on freenode (I'm
*jle`*), but you can always use the [issue tracker][tracker] too, [email
me](mailto:justin@jle.im), or find me on twitter as [mstk][twitter].

[tracker]: https://github.com/mstksg/issues
[twitter]: https://twitter.com/mstk "Twitter"

All this being said, *auto* is still kind of technically in a sorta
pre-release state, because not all of the tests are written yet.  But the API
should be stable and updates before `0.3.x` are going to all be backwards
compatible (API-wise) bug fixes or filling in holes.
