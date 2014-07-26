From Auto to Wire & Continuous Time (Intro to Machines & Arrow Kinda, Part 3)
=============================================================================

Categories
:   Haskell
:   Ramblings
Tags
:   haskell
:   functional reactive programming
:   arrows
:   netwire
CreateTime
:   2014/07/21 21:28:28
PostDate
:   Never
Series
:   Intro to Machines and Arrows
Identifier
:   machines-3

Hello!  In our [last post][part2], we looked deeper into the Auto type, played
around with instancing it as familiar typeclasses, saw it as a member of the
powerful *Category* and *Arrow* typeclasses, and took advantage of this by
composing Autos both manually and using proc/do notation, and were freed from
the murk and mire of explicit recursion.  We observed the special nature of
this compoisition, and saw some neat properties, like local statefulness.

[part2]: http://blog.jle.im/entry/auto-as-category-applicative-arrow-intro-to-machines

We've talked a lot about Machines and Arrow --- and truth be told, there is a
lot more we can say.  But hopefully you've gained a bit of insight on how
mealy and moore machines work, and have been shown a motivating example of the
Arrow typeclass and proc/do notation.

But now, we specialize our knowledge and focus on Auto, and the road to
implementing FRP and continuous time semantics in our framework :)  This has
been my not-so-secret ulterior motive all along!

As always, feel free to leave a comment if you have any questions, drop by
freenode's *#haskell*, or find me on [twitter][] :)

[twitter]: https://twitter.com/mstk "Twitter"

Fancy Bells And Whistles
------------------------

### Adding Inhibition

### Inhibition with a value

### Over a Monad

### Samples

Functional Reactive Programming and Continuous Time
---------------------------------------------------

### Continuous Time

### Denotative semantics

### The Model

Adding Time
-----------

### Samples

### Preserving continuous time

All together
------------

Looking forward
---------------


