Pipes: Streaming Huffman Compression in Haskell (Part 3)
========================================================

Categories
:   Haskell
:   Tutorials
Tags
:   haskell
:   pipes
CreateTime
:   2014/04/12 19:06:07
PostDate
:   Never
Series
:   Huffman Compression
:   Beginner/Intermediate Haskell Projects
Identifier
:   huffman-3

Let's finally finish up our Streaming Huffman Compression project by actually
implementing the "streaming" part :)  In [part 1][] we looked at the data
structures which we used to implement our compression logic; in [part 2][] we
looked at the actual compression/decompression algorithm and implemented it.
Finally, let's wrap it all up and actaully implement a streaming interface!

[part 1]: http://blog.jle.im/entry/streaming-huffman-compression-in-haskell-part-1-trees
[part 2]: http://blog.jle.im/entry/streaming-huffman-compression-in-haskell-part-2-binary

If we were using an imperative approach, this would usually involve some sort
of loop --- read a byte, process it, write the resulting byte, read the next,
process it, write it...it's a step of instructions that a computer will be
able to perform step-by-step.

In Haskell, when we can, we try to look for a pure, declarative approach based
on compositions of abstractions.  That's what Haskell does best, after all.
So let's see what we can do!

Choosing Pipes
--------------

So we are searching for an abstraction to handle *constant-space* IO
streaming.--- that is, we only ever have in memory exactly what we are
processing at that moment, and nothing else. For this, there are a couple
go-to abstractions we can use that provides this (at the low level).

1.  We have the always-classic conceptually simple **lazy IO**. Basically, we
    construct a series of IO operations on a file that operates on it
    line-by-line (or whatever your buffering settings are), as if it were
    actually all in memory.

    We then rely on the GHC runtime's *lazy evaluation* to "not read the IO
    until we ask for it".  Just like lazy evaluation for normal values (values
    that are "defined" are not evaluated until you ask for them).  In this
    way, the file is only read whenever you actually ask for it/need it, into
    memory line-by-line.  When you no longer need it, it is garbage collected
    --- just like normal values.  This maintains the "constant space".

    While this solution is apparently elegant at first, it suffers from much
    of the problems that plague lazy evaluation in general: unpredictable
    resource usage.  With lazy semantics, what is done when is very
    unpredictable because you rely on the runtime system --- something not in
    the control of your code.  You have very little control of specifying when
    you read anything or when files are closed.

    For this reason, lazy IO is discouraged for most resource-sensitive
    applications in the real world.

2.  We have a whole selection of libraries based on Coroutines.  Born out of
    the first Iteratee libraries, over the past five or so years,
    implementations and libraries have risen and fallen and, as of 2014, two
    main ones stand: [conduit][] and [pipes][].

    [pipes]: http://hackage.haskell.org/package/pipes
    [conduit]: https://hackage.haskell.org/package/conduit

    Both conduits and pipes are based around the idea of a "processing
    pipeline" that, at each step (each "pipe"), transforms streaming data
    explicitly piece-by-piece.  We build these pipelines out of components ---
    some are provided by libraries, and some we write ourselves using a very
    simple monadic DSL/API based on await/yield semantics.

    No relying on laziness to time our IO; the library handles it all for us
    in a meaningful, predicatable, and controllable way --- and the cleanup,
    too!


The choice between *conduit* and *pipes* depends a lot on what you want to
accomplish.  There was a very nice [Haskell Cast][hc] episode on this matter
(and more) that I would highly recommend.  Both libraries come from very
different backgrounds and histories.

[hc]: http://www.haskellcast.com/episode/006-gabriel-gonzalez-and-michael-snoyman-on-pipes-and-conduit/

Conduit focuses around safe resource handling, and pipes focuses on equational
reasoning and applied mathematical abstractions.

We're going to use pipes for this tutorial.  More specifically, pipes
augmented with [pipes-parse][].  Why not conduit, which has built-in
leftover/end-of-stream detection?  Well, no major reason.  You could actually
translate much of what is described here to conduit with little work.  But I
wanted to show pipes to maybe display some of the nice equational reasoning
possible with mathematics-based abstractions that Haskell is so famous for.

[pipes-parse]: http://hackage.haskell.org/package/pipes-parse


Structure
---------


