Auto: A Todo GUI application with Auto and GHCJS
================================================

Categories
:   Haskell
:   Auto
:   Tutorials
Tags
:   auto
:   haskell
:   ghcjs
CreateTime
:   2015/03/26 00:15:21
PostDate
:   Never
Series
:   All About Auto
:   Beginner/Intermediate Haskell Projects
Identifier
:   auto-todo

Continuing along with [All About Auto][series], let's look at another exciting
and useful application of the [*auto*][] library: GUI's.  We're going to look
at the canonical "hello world" of GUI apps these days --- the todo app.  We're
going to be using the specs of [todoMVC][] to build a todoMVC "candidate" that
follows the specs...and along the way see what *auto* offers in its tools of
managing isolated state components and modeling GUI logic.  We're really going
to be focusing on application logic --- "control" and "model" --- and not
looking too close on "views", which *auto* doesn't quite try to offer and
where you can really pick your own view rendering system, making this
adaptable to really any platform --- javascript/web, desktop, command line...

[series]: http://blog.jle.im/entries/series/+all-about-auto
[auto]: http://hackage.haskell.org/package/auto
[todoMVC]: http://todomvc.com/

This post does assume *some* concepts from the [tutorial][]...if not all, then
at least those in the [introductory post][intro] or the [README][].  If you
ever find yourself thinking that these concepts are completely new and crazy,
you might want to try looking through the [tutorial][] or [docs][auto] to
refresh your mind.  As always, comments are welcome, and I'm also usually on
*#haskell-auto* as *jle`*, and also on [twitter][]

[tutorial]: https://github.com/mstksg/auto/blob/master/tutorial/tutorial.md
[intro]: http://blog.jle.im/entry/introducing-the-auto-library
[README]: https://github.com/mstksg/auto/blob/master/README.md
[twitter]: https://twitter.com/mstk "Twitter"

