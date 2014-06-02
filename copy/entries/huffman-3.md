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
Finally, let's wrap it all up and actually implement a streaming interface!

[part 1]: http://blog.jle.im/entry/streaming-huffman-compression-in-haskell-part-1-trees
[part 2]: http://blog.jle.im/entry/streaming-huffman-compression-in-haskell-part-2-binary

If we were using an imperative approach, this would usually involve some sort
of loop --- read a byte, process it, write the resulting byte, read the next,
process it, write it...it's a step of instructions that a computer will be
able to perform step-by-step.

In Haskell, when we can, we try to look for a pure, declarative approach based
on compositions of abstractions.  That's what Haskell does best, after all.
So let's see what we can do!

Pipes
-----

### Choosing Pipes

So we are searching for an abstraction to handle *constant-space* IO
streaming.--- that is, we only ever have in memory exactly what we are
processing at that moment, and nothing else. For this, there are a couple
go-to abstractions we can use that provides this (at the low level).

We can use lazy IO, which basically relies on Haskell's built in laziness
semantics that we know and love to control when IO happens.  The problem here
is that your IO actions are no longer first-class members of the language ---
they are "runtime magic".  You can no longer really reason about when file
handles are closed and exactly when reads happen.  This really is a bit
antithetical to Haskell, a language where we actually have the ability to move
IO into a first-class member of the language and make it something that we can
actually reason about.

There have been many solutions developed to this problem and in modern times,
[conduit][] and [pipes][] have emerged, built on the backs of early
coroutine-based libararies.  These libraries are built on the idea of purely
assembling and "declaring" the IO pipline that you want, with each pipeline
component having very explicit and composable and reasonable IO
read/write/close semantics.

[pipes]: http://hackage.haskell.org/package/pipes
[conduit]: https://hackage.haskell.org/package/conduit

The choice between *conduit* and *pipes* depends a lot on what you want to
accomplish.  There was a very nice [Haskell Cast][hc] episode on this matter
(and more) that I would highly recommend.  Both libraries come from very
different backgrounds and histories.

[hc]: http://www.haskellcast.com/episode/006-gabriel-gonzalez-and-michael-snoyman-on-pipes-and-conduit/

Conduit focuses around safe resource handling, and pipes focuses on equational
reasoning and applied mathematical abstractions.

We're going to use pipes for this tutorial, with some elements from
[pipes-parse][] for our limited requirements of leftover support.  Why not conduit,
which has built-in leftover/end-of-stream detection?  Well, no major reason.
You could actually translate much of what is described here to conduit with
little work.  But I wanted to show pipes to maybe display some of the nice
equational reasoning possible with mathematics-based abstractions that Haskell
is so famous for.

[pipes-parse]: http://hackage.haskell.org/package/pipes-parse

### Starting Pipes

Before you proceed, it is recommended that you read over or are at least
somewhat familiar with the amazing [pipes tutorial][ptut], which is a part of
the actual pipes documentation.  This post does not attempt to be a substitute
for it, only a "what's next?".

[ptut]: http://hackage.haskell.org/package/pipes-4.1.2/docs/Pipes-Tutorial.html

Basically, the entire meat of our program (and the bulk of the design process)
will be in "declaring" chains of producers, pipes, and consumers.

This approach should be very familiar with anyone who has ever used unix pipes
--- you can do amazing things by just chaining simple utilities.  At each step
of the way, each "pipe" transforms an input and pops out an output, which is
received by the next step.

And without any further delay, let's write `encode.hs`!

Encoding
--------

### Design

Let's think of the main pipeline we want here.

Remembering that we can only read and write `ByteString`s directly from a
file:

1.  Read in `ByteString`s from a file (our *Producer*, the source of the
    stream)
2.  Turn those `ByteString`s into `Byte`s (`Word8`), emitting one at a time
3.  Look up each `Word8` and turn them into a stream of `Direction`s, emitting
    one direction at a time.
4.  Clump up incoming `Direction`s into chunks of 8, and re-emit them as
    `Word8`s.
5.  Take the incoming `Word8`s and convert them into `ByteString`s
6.  Write each incoming `ByteString` to an output file (our *Consumer*, the
    terminal of the stream)

Sounds simple enough, right?  Basically like using unix pipes!

#### The Problem

Unfortunately, the naive approach displays some cracks in our case in that
vanilla pipes do not have *leftover support*.  That is, the stream terminates
as soon as the Producer terminates.

This is normally not a problem (it won't be an issue at all for our decoding
program), except here in step 4: we need to clump up incoming directions into
groups of 8 to turn them into bytes.

If our incoming stream of directions stop mid-byte (that is, maybe we only
have 15 directions instead of 16), ideally we would want to be able to send a
full byte downstream to write it down (padding the extra space with 0's).

However, in the way that vanilla pipes works, once our incoming direction
stream stops...that in-progress chunk disappears and is never written.

#### pipes-parse

This is the problem that is solved with *pipes-parse*.  *pipes-parse* provides
*pipe transformers* that let us elegantly tackle the problem of leftovers.

For example, if we look at `p1 >-> p2 >-> p3` (`p1` being the pipe at step 1
above, etc.), we can think of it as a "`Direction` producer":

~~~haskell
fileBS    :: Producer ByteString           IO r
bsToBytes :: Pipe     ByteString Word8     m  r
toDirs    :: Pipe     Byte       Direction m r

directionProducer :: Producer Direction IO r
directionProducer = fileBS >-> bsToBytes >-> toDirs
~~~

`bytestrings` is a producer of `ByteStrings`...and we "pipe it" to `bsToBytes`
and `toDirs` and call the whole thing a producer of `Direction`s.

In the naive attempt, we would attempt to pipe this into a `dirsToBytes`:

~~~haskell
dirsToBytes :: Pipe Direction Word8 m r

byteProducer :: Producer Word8 IO r
byteProducer = directionProducer >-> dirsToBytes
~~~

Which gets something of the right type (a producer of `Word8`s)...but not the
right behavior.

However, we can define a `directionClumper` *producer transformer*:

~~~haskell
directionClumper :: Producer Direction m r -> Producer Word8 m r

byteProducer = directionClumper directionProducer
~~~

where `directionClumper` does exactly what we want to (clumps up all
directions from the given producer, and handles the leftovers as expected).

*pipes-parse* gives us the tools to write a `directionClumper` that does what
we need.

### Down to it

So let's get down to it.

First, our imports:

~~~haskell
!!!huffman/encode.hs "-- General imports" "-- Pipes imports" "-- Working with Binary" "-- Huffman imports
~~~

It's a doozy, admitedly!

Now `main`:

~~~haskell
!!!huffman/encode.hs "main ::"
~~~

Just straight-forward, more or less.  The error handling is kind of not too
great, but we won't go into that too deeply here :)

#### File metadata

`analyzeFile` is going to be how we build the Huffman Tree for the encoding,
as discussed in part 1.  It'll go through an entire pass of the file
and count up the number of occurrences for each byte and build a Huffman
Encoding Tree out of it.  It'll also give us the length of the file in bytes,
which is actually necessary for *decoding* the file later, because it tells us
where to stop decoding (lest we begin decoding the leftover padding bits).

~~~haskell
!!!huffman/encode.hs "analyzeFile ::"
~~~

We encounter our first usage of pipes here, in the definition of `freqs`,
which uses `PP.fold` (from pipes prelude) to basically run a giant "fold" over
all of the incoming items of the given producer.

The fold is identical in logic to `listFreq` from a Part 2:

~~~haskell
!!!huffman/Huffman.hs "listFreq ::"
~~~

Except instead of folding over a list, we fold over the elements of the
producer.

The producer we use is the `fromHandle handle` Producer, which is a Producer
that emits every `ByteString` from the file at the given handle.  We opened
the handle with [`withFile`][withFile] from System.IO, which gives us a file
handler for a given filepath.

[withFile]: http://hackage.haskell.org/package/base-4.7.0.0/docs/System-IO.html#v:withFile

Now, we want to fold over all of the `Word8`s, not all of the `ByteString`s,
so we use `bsToBytes`:

~~~haskell
!!!huffman/encode.hs "bsToBytes ::"
~~~

which uses `B.unpack :: ByteString -> [Word8]`, which turns a `ByteString`
into a list of its constituent `Word8`s.  We use `PP.mapFoldable`, which is
sort of like `concatMap` --- it applies the given function to every incoming
element in the stream, and emits the items in the resulting list[^foldable]
one-by-one.  So `bsToBytes` is a Pipe that takes in `ByteString`s and emits
each contained `Word8` one-by-one.

[^foldable]: It actually works on all `Foldable`s, not just `[]`.

We use `sum` from `Data.Foldable`, which sums up all the numbers in our
frequency map.  Then we use what we learned about the State monad in Part 1 to
build our tree (review Part 1 if you do not understand the declaration of
`tree`).  `tree` is a `Maybe (PreTree Word8)`; we then tag on the length to
our `tree` using `fmap` and the TupleSections extension.  (That is, `(,y)` is
sugar for `(\x -> (x,y))`).

#### The Encoding Pipeline

Once we have that, we can get onto the actual encoding process.

~~~haskell
!!!huffman/encode.hs "encodeFile ::"
~~~

First, we open our file handles for our input and output files.  Then, we use
what we learned in Part 2 to get binary serializations of our length and our
tree using `encode`, and use `B.hPut` to write it to our file, as the
metadata.  `BL.hPut` from `Data.ByteString.Lazy` takes a file handle and a
lazy `ByteString`, and writes that `ByteString` out to the file.  We use the
lazy version because `encode` gives us a lazy `ByteString` by default.

Now, we get to our actual pipes.  The first "pipeline" is `dirStream`, which
is our stream (producer) of `Direction`s encoding

Now we get to our actual pipes --- first, our producer and source, `fromHandle
hIn`, which is a producer of `ByteString`s from the given handle.

<!-- say something about `view` -->






