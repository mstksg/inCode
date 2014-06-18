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
go-to abstractions we can use that provide this (at the low level).

We can use lazy IO, which basically relies on Haskell's built in laziness
semantics that we know and love to control when IO happens.  The problem here
is that your IO actions are no longer [first-class members][pureio] of the
language --- they are "runtime magic".  You can no longer really reason about
when file handles are closed and exactly when reads happen.  This really is a
bit antithetical to Haskell, a language where we actually have the ability to
move IO into a first-class member of the language and make it something that
we can actually reason about.

[pureio]: http://blog.jle.im/entry/the-compromiseless-reconciliation-of-i-o-and-purity

There have been many solutions developed to this problem and in modern times,
[conduit][] and [pipes][] have emerged, built on the backs of early
coroutine-based libraries.  These libraries are built on the idea of purely
assembling and "declaring" the IO pipeline that you want, with each pipeline
component having very explicit and comparable and able-to-reason-with IO
read/write/close semantics.

[pipes]: http://hackage.haskell.org/package/pipes
[conduit]: https://hackage.haskell.org/package/conduit

The choice between *conduit* and *pipes* depends a lot on what you want to
accomplish.  There was a very nice [Haskell Cast][hc] episode on this matter
(and more) that I would highly recommend.  Both libraries come from very
different backgrounds and histories.

[hc]: http://www.haskellcast.com/episode/006-gabriel-gonzalez-and-michael-snoyman-on-pipes-and-conduit/

This picture is slightly simplified, but conduit focuses around safe resource
handling, and pipes focuses on equational reasoning and applied mathematical
abstractions.

We're going to use pipes for this tutorial, with some elements from
[pipes-parse][] for our limited requirements of leftover support.  Why not
conduit, which has built-in leftover/end-of-stream detection?  Well, no major
reason. You could actually translate much of what is described here to conduit
with little work.  But I wanted to show pipes to maybe display some of the
nice equational reasoning possible with mathematics-based abstractions that
Haskell is so famous for.

[pipes-parse]: http://hackage.haskell.org/package/pipes-parse

### Starting Pipes

Before you proceed, it is recommended that you read over or are at least
somewhat familiar with the amazing [pipes tutorial][ptut], which is a part of
the actual pipes documentation.  This post does not attempt to be a substitute
for it, only a "what's next?".

[ptut]: http://hackage.haskell.org/package/pipes-4.1.2/docs/Pipes-Tutorial.html

Basically, the entire meat of our program (and the bulk of the design process)
will be in "declaring" and "transforming" chains of producers, pipes, and
consumers.

This approach should be very familiar with anyone who has ever used unix pipes
--- you can do amazing things by just chaining simple utilities.  At each step
of the way, each "pipe" processes an input and pops out an output, which is
received by the next step.

We also have ways to "modify components": "pipe transformers"; an analogy in
bash would be like `sudo`, which takes a normal bash commands and "turns it
into" a super user command.

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
!!!huffman/encode.hs "-- General imports" "-- Pipes imports" "-- Working with Binary" "-- Huffman imports"
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

The fold is identical in logic to `listFreq` from a [Part 2][part 2]:

~~~haskell
!!!huffman/Huffman.hs "listFreq ::"
~~~

Except instead of folding over a list, we fold over the elements of the
producer.

The producer we use is the `PB.fromHandle handle` Producer, which is a
Producer that emits every `ByteString` from the file at the given handle.  We
opened the handle with [`withFile`][withFile] from System.IO, which gives us a
file handler for a given filepath.

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
frequency map.  Then we use what we learned about the State monad in [Part
1][part 1] to build our tree (review [Part 1][part 1] if you do not understand
the declaration of `tree`).  `tree` is a `Maybe (PreTree Word8)`; we then tag
on the length to our `tree` using `fmap` and the TupleSections extension.
(That is, `(,y)` is sugar for `(\x -> (x,y))`).

#### The Encoding Pipeline

Once we have that, we can get onto the actual encoding process.

~~~haskell
!!!huffman/encode.hs "encodeFile ::"
~~~

First, we open our file handles for our input and output files.  Then, we use
what we learned in [Part 2][part 2] to get binary serializations of our length
and our tree using `encode`, and use `BL.hPut` to write it to our file, as the
metadata.  `BL.hPut` from `Data.ByteString.Lazy` takes a file handle and a
lazy `ByteString`, and writes that `ByteString` out to the file.  We use the
lazy version because `encode` gives us a lazy `ByteString` by default.

Note that we can "put" `(len, tree)` together as a tuple instead of putting
`len` and `tree` one after the other.  This is okay because we will be
eventually reading them back as a tuple in our decoding script (also, the
`Binary` instance for `(a, b)` is literally just to put `a`, then put `b`).

Now, we get to our actual pipes.  The first "pipeline" is `dirStream`, which
is our stream (producer) of `Direction`s encoding the input file.  As can be
read, `dirStream` is `PB.fromHandle hIn` (a `ByteString` producer from the
given handle) piped into our old friend `bsToBytes` piped into `encodeByte
encTable`, which is a pipe taking in bytes (`Word8`), looks them up in
`encTable` (the table mapping `Word8` to `[Direction]`, which we built in
[Part 2][part 2]), and spits out the resulting `Direction`s one at a time.

`encodeByte encTable` is implemented "exactly the same" as `bsToBytes`:

~~~haskell
!!!huffman/encode.hs "encodeByte ::"
~~~

instead of using `mapFoldable` with a `ByteString -> [Word8]`, we use
`mapFoldable` with a `Word8 -> [Direction]`, which does the same thing ---
apply the function to every incoming item, and spit out the items in the
resulting list one at a time.

`(!) :: Map k v -> k -> v` is the lookup function for `Map`s.

#### Parser

So now we have `dirStream :: Producer Direction IO r`, `PB.fromHandle hIn >->
bsToBytes >-> encodeByte encTable`, which is a producer of `Direction` drawn
from the file.  It's now time to "group up" the directions, using the
"producer transformer" tactic we discussed earlier.

~~~haskell
!!!huffman/encode.hs "dirsBytes ::"
~~~

`dirsBytes` turns out `Direction` producer into a `Word8` producer by running
the *parser* `dirsBytesP` onto the producer, and looping onto itself.  We'll
look at `dirsBytesP` later, but for now, know that it is a parser that
attempts to consume eight `Direction`s and returns them together in a `Just
byte` with zero padding if the stream runs out; if the stream is already
empty to start with, it returns `Nothing`.


Remember that in *pipes-parse*:

~~~haskell
runStateT :: Parser a m b -> Producer a m r -> m (b, Producer a m r)
~~~

Basically, `runStateT parser` takes a `Producer a` and "parses" a value out of
it, returning the parsed value and the "leftover/used" `Producer`.

In our case:

~~~haskell
runStateT :: Parser   Direction IO (Maybe Word8)
          -> Producer Direction IO r
          -> IO (Maybe Word8, Producer Direction IO r)
~~~

So we use the `dirsBytesP` parser onto the producer we are given.  If it
doesn't parse any bytes (`Nothing`), then we stop.  If it does (`Just byte`),
then we `yield` the parsed `Word8` and then start over again with the
leftovers producer.

Let's take a look at the `dirsBytesP` parser, which parses `Direction`s into a
`Word8`:

~~~haskell
!!!huffman/encode.hs "dirsBytesP ::"
~~~

This implemenation is pretty straightforward --- "if the producer is empty,
return `Nothing`.  Otherwise, start with `00000000` and draw `Direction`s one
at a time, flipping the appropriate bit when you get a `Right`."  For more
information on the exact functions for bitwise operators, look into the
[bits][] package, where they come from.

[bits]: http://hackage.haskell.org/package/bits

Note the usage of `draw`, which "returns" a `Nothing` if you draw from the end
of the producer, and a `Just x` if there is something to draw.  `draw` is
special to parsers, because it lets you react on end-of-input as a `Nothing`
(as opposed to `await`).  In `go`, we loop drawing until we either get all
eight bits (and return the resulting byte) or run out of inputs (and return
the byte that we have so far).

We can finally put it all together, by saying `bytesOut = dirsBytes
dirStream`.

#### view

Now, the next step could have been to pipe in the stream of `Word8` into
`PP.map B.pack`, which takes each incoming `Word8` and "packs" them into a
singleton `ByteString`.

However, this is a bad idea, as you are basically creating a full `ByteString`
and triggering a write on every incoming byte.  Instead, we use

~~~haskell
(view PB.pack) bytesOut
~~~

Which is an idiom that comes from the infamouse *lens* library.  Basically,
`PB.pack` from *pipes-bytestring* (and not the one from *bytestring*), contains
"producer transformers", (like `dirsBytes`).  It contains two, actually --- a
`Producer Word8 m r -> Producer ByteString m r`, and a `Producer ByteString m
r -> Producer Word8 m r`.

In fact, in a way, you can think of `PB.pack` as simply a tupling of the two
transformer functions together.  Practically, the tupling/packaging them
together is useful because the two are inverses of eachother.

<div class="note">
**Aside**

`PB.pack` is actually an isomorphism; if `toBS` is your forward
transformer and `toW8` is your backwards transformer, `PB.pack` is `iso toBS
toW8`.

You can then think of `view` as a function that obeys the properties:

~~~haskell
view (iso to from) = to
~~~

So when we say `view PB.pack`:

~~~haskell
view PB.pack
== view (iso toBS toW8)     -- definition of `PB.pack`
== toBS                     -- behavior of `view` and `iso`
~~~

</div>

Simply speaking, `view` is a function from *lens* that takes the "first" part
(kind of like `fst`) of that "tuple".

So *pipes-bytestring* gives us this tupling of two transformers (one going
from `Word8` to `ByteString` and one going from `ByteString` to `Word8`),
called `PB.pack`, and we use `view` to get the "forward" direction, the first
one.

It might be more clear to see this as

~~~haskell
let word8ToByte = view PB.pack
in  word8ToByte bytesOut
~~~

There are many reasons why *pipes-bytestring* gives us these useful
functions "tied together" in `pack` instead of just them separately, and they
have to do with ways you can manipulate them with *lens*.  But that's another
story :)

Anyways, the implementation of `view pack` basically takes a chunk
of `Word8`s and then packs them into a big `ByteString`, and uses "smart"
chunking that allows us to maximize both space and time usage.  Pretty smart!

Ok, now that we have a stream of `ByteString`, all that's left to do is write
it to disk.  And for that, we have `PB.toHandle out`, which is a consumer that
takes in `ByteString` and writes each incoming `ByteString` to the given file
handler.

#### All together

That gives us our final `pipeline`; we lay out a series of pipes and pipes
transformers that takes our file and streamingly processes the data and writes
it into the output file.

Once we have our `pipeline`, we use `runEffect` to "run" it; then...that's it!



### Testing it out

Cool, let's try it out with Leo Tolstoy's great classic [War and Peace][wp]
from Project Gutenberg!

[wp]: http://www.gutenberg.org/files/2600/2600.txt

~~~bash
$ ghc -O2 encode.hs
$ ./encode warandpeace.txt warandpeace.enc
$ du -h warandpeace.*
1.8M warandpeace.enc
3.1M warandpeace.txt
~~~

Cool, we compressed it to 58% of the original file size.  Not bad!  Using
`gzip` with default settings gives a compression of 39%, so it's not the best,
but it's something.  If we take out the encoding part of the script, we can
see that the dictionary itself only takes 259 bytes (which is negligible) ---
so 58% is pretty much the asymptotic compression rate.

At this point it's not as snappy (performance wise) as we'd like; a
compressing a 3.1M file is not "super slow" (it takes seven seconds on my
computer), but you probably won't be compressing a gigabyte.  We'll look into
performance in a later post!

Decoding
--------

### Design

Let's try to see the plan for our decoding script, applying what we learned
before; we want a pipe that:

1.  Reads in `ByteString`s from a file
2.  Turns those `ByteString`s into `Word8`s.
3.  Turns those `Word8`s into `Direction`s.
4.  Turns those `Direction`s into `Word8`s again, by using each incoming
    `Direction` to search our given Huffman encoding tree.
5.  Takes the `Word8` producer given by the composition/connecting of steps 1 -
    5, and transform it into a `ByteString` producer using `view PB.pack`
6.  Writes those incoming `ByteString`s to our output file.

### Down to it

Luckily we can use most of the concepts we learned in writing the encoding
script to write the decoding script; we also have a less imports, so it's a
sign that decoding is going to be slightly simpler than encoding.

~~~haskell
!!!huffman/decode.hs.hs "-- General imports" "-- Pipes imports" "-- Working with Binary" "-- Huffman imports"
~~~

`main` should seem very familiar:

~~~haskell
!!!huffman/decode.hs "main ::"
~~~

And now on to the juicy parts:

~~~haskell
!!!huffman/decode.hs "decodeFile ::"
~~~

#### Loading metadata

Loading the metadata is a snap, and it uses what we learned earlier from
`runStateT` and `Parser`s.

Here, our `Parser` is `PB.decode`, from the *pipes-binary* package (and not
from *binary*), and it does more or less exactly what you'd expect: it reads
in binary data from the source stream, consuming it until it has a successful
(or unsuccessful) parse, as given by the *binary* package talked about in
[Part 2][part 2].  The "result" is the `Either` containing the success or
failure, and the "leftover", consumed source stream.

In our case:

~~~haskell
runStateT
  :: Parser   ByteString IO (Either DecodingError (Int, PreTree Word8))
  -> Producer ByteString IO r
  -> IO (Either DecodingError (Int, PreTree Word8), Producer ByteString IO r)
~~~

So `metadata` is `Either DecodingError (Int, PreTree Word8)`.  If we get a
`Left e`, then we throw an error for unparseable/corrupted metadata.  If we get
a `Right (len, tree)`, then we are good to go.

#### The Decoding Pipeline

The rest just reads like poetry!

~~~haskell
let byteStream = decodingPipe >-> bsToBytes
             >-> bytesToDirs  >-> searchPT tree
             >-> PP.take len
~~~

Beautiful!  `decodingPipe` is the leftover producer after the parse of the
metadata.  `bsToBytes` is the same as from our encoder.  `bytesToDirs` is new,
but pretty simple:

~~~haskell
!!!huffman/decode.hs "bytesToDirs ::"
~~~

It uses the *bits* package to turn an incoming `Word8` into a list of its
constituent bits (in the form of `Direction`s), and yields each of them in
turn.

We have `searchPT tree`, which is a pipe turning incoming `Direction`s into
aggregate/outgoing `Word8`s by finding them on the given `PreTree`.  The
implementation is a bit tricky so we're going to go into it in more detail
later.

`PP.take len` is new; it's from `Pipes.Prelude`, and it basically says "take
`len` items from the source, then stop drawing."  This is necessary because,
because of the padding of 0's we did from the encoding script, there will be
more bits in the file than are actually a part of the encoding; using
`PP.take` ensures that we don't try to read the extra padding bits.  It'll
take up to `len` `Word8`s, and then stop.

And so now we have our `Word8`/byte Producer/stream!

#### searchPT

One could actually have written `searchPT` like this:

~~~haskell
searchPT :: PreTree a -> Pipe Direction Word8 m r
searchPT pt0 = go pt0
  where
    go (PTLeaf x) = do
        yield x
        go pt0
    go (PTNode pt1 pt2) = do
        dir <- await
        go $ case dir of
               DLeft  -> pt1
               DRight -> pt2
~~~

which looks a lot like the logic of our decoder functions from [Part 2][part
2].

However, we can do better.  This way sort of mixes together the "logic" of
decoding from the yielding/pipe-ness of it all.  Ideally we'd like to be able
to separate the logic.  This isn't *too* necessary, but doing this will expose
us to some nice *pipes* idioms :)

One way we can do it is to turn `searchPT` into a `Consumer'` (a `Consumer`
with the ends not sealed off) that consumes `Direction`s and *returns*
resulting `Word8`s.

Then we use `(>~ cat)`, which turns a `Consumer'` into something that is
forever consuming and re-yielding --- it turns a `Consumer'` returning values
into a `Pipe` repeatedly yielding the returned values.

~~~haskell
!!!huffman/decode.hs "searchPT ::"
~~~

The logic is slightly cleaner; the gain isn't that much, but just being able
to have this separation is nice.  This also is a good exposure to `(>~)`!

<div class="note">
**Aside**

`(>~)` is a pretty useful thing.  Basically, when you say

~~~haskell
consumer >~ pipe
~~~~

it is like saying "*Every time `pipe` `await`s, just use the result returned
by `consumer` instead*".

We can look at `cat`:

~~~haskell
cat :: Pipe a a m r
cat = forever $ do
        a <- await
        yield a
~~~

Which just simply echoes/sends back down whatever it receives.

When we say:

~~~haskell
consumer >~ cat
~~~

We basically say "every time we `await` something in `cat`, just use
`consumer`'s return value":

~~~haskell
consumer >~ cat
    = forever $ do
        a <- consumer
        yield a
~~~

Basically, `consumer >~ cat` repeatedly consumes the input and yields
downstream the return of the consuming.

(Remember, the value the pipe returns (the `r`) is different than the value
the pipe "sends downstream"; the downstream values are used in connecting with
`(>->)` and the like, and the return value is just the value that the specific
thing *returns* when ran, to the thing doing the running.)

Play around with `(>~)`-ifying different `Pipe`s and seeing what it does to
it; you might have some fun.

Why `Consumer'` and not `Consumer`?  Well, remember that all lines of the `do`
block have to have the same "yield" type (because the Monad is `Pipe a b m`,
so all lines have to be `Pipe a b m r` for different `r`'s --- the `a`'s and
`b`'s (the yield type) and `m`'s have to be the same), so `Consumer'` lets the
yield type be whatever it needs to be to match with the rest of the `do`
block.

Don't worry if this is a bit complicated; you don't need to really undersatnd
this to use *pipes* :)

Admittedly, my description isn't too great, so if anyone has a better one, I'd
be happy to use it here!
</div>


#### The Rest

And the rest is...well, we already know it!

We use `(view PB.pack) byteStream` like last time to turn our stream of `Word8`
into a stream of `ByteString`, with "smart chunking".  Then we pipe that in to
`PB.toHandle`, like we did last time, and have it all flow into the output file.

We have assembled our pipeline; all we have to do now is `runEffect`, to "run"
it.  And again, that's it!

### Testing

~~~haskell
$ ghc -O2 decode.hs
$ ./decode warandpeace.enc warandpeace.dec
$ md5sum warandpeace.txt
3c8168e48f49784ac3c2c25d15388e96  warandpeace.txt
$ md5sum warandpeace.dec
3c8168e48f49784ac3c2c25d15388e96  warandpeace.dec
~~~

And yup, we get an exact, lossless decompression.

Decompression is faster than compression, as you'd expect; on my computer it
takes about two seconds to decompress the 3.1M file.  Still a bit slower than
we'd like, but not *too* bad.

Conclusion
----------

Hopefully from this all, you can see *pipes* as a beautiful abstraction for
chaining together streaming computations in a pure, constant-space way.  I
hope that looking back on it all you can see everything as either a
transformation of pipes, or a chaining of pipes.

I recommend looking more into the great *pipes* documentation, joining the
[pipes mailing list][pml], and trying your own streaming data projects with
*pipes* to see what you can do with it.

[pml]: https://groups.google.com/forum/#!forum/haskell-pipes

You should also checkout *[conduit][]* and try to implement this streaming
logic in that framework.  Let me know how it turns out!

As always the great people of freenode's #haskell are always free to answer
any questions you might have, and also of course the [haskell tag][] on Stack
Overflow.  And I'll try to address as many questions as I can in the comments!

[haskell tag]: http://stackoverflow.com/questions/tagged/haskell

Keep in mind that I'm still a new user of *pipes* myself; if I've made any
huge mistakes in style or idiomaticness, I'm available here in the comments
and I'd appreciate any corrections y'all can offer.

So ends the "pipes tutorial" section of this series; tune in next time and
we'll try our best to optimize this baby! [^nexttime]

[^nexttime]: Hopefully you aren't holding your breath on this one :)  This
next part is not scheduled any ime soon and might not come for a while, as I'll
be pursuing some other things in the near future --- I apologize for any
disappointment/inconvenience this may cause.
