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

(All of the code in this article and the ones before can be downloaded [from
github][allcode], so you can download it and try it out yourself!)

[allcode]: https://github.com/mstksg/inCode/tree/master/code-samples/huffman

Pipes
-----

### Choosing Pipes

So we are searching for an abstraction to handle *constant-space* IO
streaming --- that is, we only ever have in memory exactly what we are
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

This picture is slightly simplified, but *conduit* focuses around safe
resource handling, and *pipes* focuses on equational reasoning and applied
mathematical abstractions.

I'm picking *pipes* for this tutorial, for no major reason.  All of this could
be written in *conduit* with little difference in code size or expressiveness,
I'm sure.  I mostly chose *pipes* because I wanted to demonstrate some of the
nice reasoning that pipes enables that Haskell is so famous for.  I also just
wanted to learn it, myself :)

### Before we go


<!-- We're going to use *pipes* for this tutorial, with some elements from -->
<!-- *[pipes-parse][]* for our limited requirements of leftover support.  Why not -->
<!-- *conduit*, which has built-in leftover/end-of-stream detection?  Well, no -->
<!-- major reason. You could actually translate much of what is described here to -->
<!-- conduit with little work.  But I wanted to use *pipes* to maybe display some -->
<!-- of the nice equational reasoning possible with mathematics-based abstractions -->
<!-- that Haskell is so famous for --- also, I wanted to learn it, myself :) -->

Before you proceed, it is recommended that you read over or are at least
somewhat familiar with the excellent [pipes tutorial][ptut], which is a part
of the actual pipes documentation.  This post does not attempt to be a
substitute for it, only a "what's next?".

[ptut]: http://hackage.haskell.org/package/pipes-4.1.2/docs/Pipes-Tutorial.html

Now, we are going to be using a bit more than just plain old *pipes* for our
program.  In addition to the libraries used in our previous parts, we're going
to be using:

1.  *[pipes-parse][]*, for leftover support.  We're going to be using limited
    leftover handling for this project in a couple of situations.
2.  *[pipes-bytestring][]*, which provides lenses for us to manipulate
    bytestring producers in efficient and expressive ways.

[pipes-parse]: http://hackage.haskell.org/package/pipes-parse
[pipes-bytestring]: http://hackage.haskell.org/package/pipes-bytestring

<!-- [lens]: http://lens.github.io/ -->

Today, our work with *pipes* will revolve around a couple of main concepts:

*   Taking two or more pipes and chaining them together to make new ones;
    hooking up input generators ("sources", or *Producers*) to pipes and to
    data consumers ("sinks", or *Consumers*)

*   Taking producers and pipes and chains of pipes (which are themselves just
    pipes, by the way) and *transforming* them into new producers and pipes.

If you've ever used bash/unix, the first concept is like using unix pipes to
"declare" a chain of tools.  You can do powerful things by just chaining
simple components.  The second concept relates to things to *sudo* or *time*;
they take normal bash commands and "transform" them into super user commands,
or "timeable" commands.

And without any further delay, let's write *encode.hs*!

Encoding
--------

(Remember that you can download [encode.hs][] from github and try it out
yourself; just remember to also grab [Huffman.hs][], [PQueue.hs][], and
[PreTree.hs][], and [Weighted.hs][] from the previous parts of this tutorial!)

!!![encode.hs]:huffman/encode.hs
!!![Huffman.hs]:huffman/Huffman.hs
!!![PQueue.hs]:huffman/PQueue.hs
!!![PreTree.hs]:huffman/PreTree.hs
!!![Weighted.hs]:huffman/Weighted.hs

### Design

Okay, so with the above in mind, let's sketch out a rough plan.  We'll talk
about the holes in the plan earlier, but it's useful to see exactly what won't
work, or what is a bad idea :)

We can envision this all as a big single giant pipeline of atomic components.

As a *Producer*, we have `fromHandle`, which emits `ByteString`s read from a
given file handle.  As a *Consumer*, we have `toHandle`, which takes in
`ByteString`s and writes them to the given file handle.

Those in hand, we'll need:

1.  A pipe that turns incoming `ByteString`s into bytes[^bs] (`Word8`s),
    emitting one at a time.
2.  A pipe that turns incoming `Word8`s into `Direction`s, by looking up each
    `Word8` in an encoding table to get a list of `Direction`s and emitting
    them one at a time.
3.  A pipe that takes in `Direction`s and "chunks them up" 8-at-a-time, and
    re-emits those chunks of 8 as bytes/`Word8`'s.
4.  A pipe that takes incoming `Word8`s and wraps them each back into
    `ByteString`s and emits them.

[^bs]: Remember, a `ByteString` is an efficiently packed "chunk"/"list" of
`Word8`/bytes; we can use functions like `unpack` and `pack` to turn a
`ByteString` into a list of `Word8`s or go backwards.

Sounds simple enough, right?  Basically like using unix pipes!

We'll be making two modifications to this plan before we go forward.

#### A Problem

The first hole: vanilla *pipes* do not have *leftover support*.  That is, the
stream terminates as soon as the producer terminates.

To put more technically: when a pipe is *awaiting* something, there is no
guarantee that it'll ever get anything --- if the producer it is awaiting from
terminates, then that's that; no chance to respond.

This is normally not a problem, and it won't be an issue for our decoding
program.  However, we run into a problem for pipe #3 above: we need to "clump
up" incoming `Direction`s and emit them in groups of 8.

This spells trouble for us, because our pipe will be merrily be waiting for
eight Directions before clumping them up --- until our producer terminates
mid-clump.  Then what?  That final in-progress clump will be lost...forever!

The problem is in the semantics of pipe composition with `(>->)`.

So it's clear that using normal pipe composition (`(>->)`) doesn't work.
We're going to have to transform our `Direction` producer in another way.

#### pipes-parse

This is precisely the problem that *pipes-parse* attempts to solve.

We'll go into more detail about how it solves this later.  At the high level,
instead of composing pipes with `(>->)`, we'll *transform* pipes by using pipe
transformers/functions.

So we'll modify our plan.  We'll have our "`Direction` producer", which
consists of:

1.  Our `ByteString` producer.
2.  Our `ByteString` to `Word8` pipe.
3.  Our `Word8` to `Direction` pipe.

And then we "transform" that `Direction` producer into a `Word8` producer,
which we'll call `dirsBytes`:

~~~haskell
dirsBytes :: Producer Direction m r -> Producer Word8 m r
~~~

which turns a `Direction` producer into a `Word8` producer that clumps up the
`Direction`s into groups of 8 --- and if the directions run out, pad the rest
of the byte with 0's.

*pipes-parse* gives us the ability to write `dirsBytes`.

#### In and Out

The next problem.

If you've ever worked with `ByteString`s, you might have noted an asymmetry to
what we are doing.  Look closely --- do you see it?

We *read* `ByteString`s from the file --- entire *big chunks* of bytes/`Word8`s.

We *write* individual bytes, one at a time.  That is, we emit individual
`Word8`s, which we each individually wrap into singleton `ByteString`s one at
a time, which we write to the file one at a time.

This is bad!

#### There's a lens for that

Luckily for us, there's a lens for just this job in *pipes-bytestring*:
`unpack`.

One thing that *lens* offers us is a way to, for some data structures,
effectively treat one thing like another thing.  Specific lenses provide
instructions on how to make that make sense.

This should be familiar to anyone who has ever used `fmap` on, say, `Identity a`:

~~~haskell
ghci> fmap (\x -> show (x + 3)) (Identity 5)
Identity "8"
~~~

(For those unfamiliar with `Identity`, it's basically `Maybe` with only
`Just`, and no `Nothing`: `data Identity a = Identity a`)

In this specific instance[^instance], `fmap` allows us to "treat a `Identity
Int` as if it were an `Int`" in the function that we pass it (in this case,
`\x -> show (x + 3)`).  In that function, we basically are "given" an `Int`
(which we named, endearingly, "*x*"), and we can do whatever we want with
it...and it'll update the entire `Identity` with the changes we make.

[^instance]: Pun intended.

So a lens literally contains the "instructions"/implementation on how to make
this happen.  If you have a lens, you can use the function `over` to
"retrieve" that implementation.

Say there was a lens called `_Identity` for `Identity a` that let us treat it
like an `a` in the same way as `fmap`, we would use it as:

~~~haskell
ghci> (over _Identity) (\x -> show (x + 3)) (Identity 5)
Identity "8"
~~~

<div class="note">
**Aside**

In simple terms, you can think of `over _Identity` as being exactly like
`fmap` for `Identity`.  In our case, `over` has type:

~~~haskell
over :: Lens (Identity a) (Identity b) a b
     -> (a -> b)
     -> Identity a
     -> Identity b
~~~

and, to take one step in generalization:

~~~haskell
over :: Lens s t a b
     -> (a -> b)
     -> s
     -> t
~~~

Also, `_Identity` isn't a `Lens` that comes in most packages; the one you'd
"actually" use is called `_Wrapped`, which works on (most) `newtype`s.
</aside>

Not all lenses "fit" this sort of semantic model (the "let me treat this thing
like this thing" model); the point is that *if you want this sort of
functionality, a lens can offer it to you*.  If you wish to provide this sort
of functionality for your type, a lens + `over` can be a way to offer it[^notfmap]; it
can be an expressive, common API[^api].

[^notfmap]: Why a lens + `over`, and not `fmap`?  Well, for one, `fmap` has to
follow many strict laws, and many times, you want to be able to do ad-hoc
things that might not necessarily be the `fmap` of a valid `Functor`.  Another
reason is that you can provide *many* lenses for a single type --- with each
one giving different "treat as this other thing" behaviors.

[^api]: ...one day in the future, when lens is common and idiomatic :)  One
day.  Yup.  Any day now...

Anyways, back to the problem at hand.  *pipes-bytestring* gives us the lens
*unpack*, which lets us treat any *bytestring producer* as a *byte producer*.

That is, you can do:

~~~haskell
(over unpack) ( \byteProducer -> stuffWithByteProducer ) bsProducer
~~~

As long as your function both takes and returns a byte producer, then the lens
unpack will basically handle all of the manual bytestring unpacking and "smart
repacking" for you.  The "smart chunking" problem that we discussed earlier
goes away, because we aren't handling it; *pipes-bytestring* handles it for
us, using the *unpack* lens.  Thank you Gabriel!

As it turns out, this is the last key to our puzzle.  Let's put it all
together.

### Down to it

Let's just get down to it!

First, our imports:

~~~haskell
!!!huffman/encode.hs "-- General imports" "-- Pipes imports" "-- Working with Binary" "-- Huffman imports"
~~~

It's a doozy, admittedly!

Now `main`:

~~~haskell
!!!huffman/encode.hs "main ::"
~~~

Just straight-forward, more or less.  The error handling is kind of not too
great, but we won't go into that too deeply here :)

#### File metadata

`analyzeFile` is going to be how we build the Huffman Tree for the encoding,
as discussed in part 1.  It'll go through an entire pass of the file and count
up the number of occurrences for each byte and build a Huffman encoding tree
out of it.  It'll also give us the length of the file in bytes; this is
actually necessary for *decoding* the file later, because it tells us where to
stop decoding (lest we begin decoding the leftover padding bits).

~~~haskell
!!!huffman/encode.hs "analyzeFile ::"
~~~

First, we use [`withFile`][withFile] from System.IO, which gives us a file
handler for a given filepath; we can pass this handler onto functions that
take file handlers.  `withFile` actually handles most of the IO-based error
handling and cleanup we would ever need.

[withFile]: http://hackage.haskell.org/package/base-4.7.0.0/docs/System-IO.html#v:withFile

Now we run into real *pipes* for the first time!

We'll assemble our producer of bytes by using `PB.fromHandle hIn` --- a
producer of `ByteString`s --- and chaining it to `bsToBytes`, a pipe that
takes incoming `ByteString`s and emits their constituent, unpacked `Word8`s
one-by-one:

~~~haskell
!!!huffman/encode.hs "bsToBytes ::"
~~~

Our implementation uses `B.unpack :: ByteString -> [Word8]`, which turns a
`ByteString` into a list of its constituent `Word8`s.  We use
`PP.mapFoldable`, which is sort of like `concatMap` --- it applies the given
function to every incoming element in the stream, and emits the items in the
resulting list[^foldable] one-by-one.  So `bsToBytes` is a Pipe that takes in
`ByteString`s and emits each contained `Word8` one-by-one.

[^foldable]: It actually works on all `Foldable`s, not just `[]`.

Then with our pipe ready, we "run"/"use" it, using `PP.fold`, from the pipes
Prelude.  This basically runs a giant "foldl" all over the incoming items of
the given producer.

The fold is identical in logic to `listFreq` from a [Part 2][part 2]:

~~~haskell
!!!huffman/Huffman.hs "listFreq ::"
~~~

Except instead of folding over a list, we fold over the elements of the
producer.  Note that the helper function has its arguments reversed.  This
whole thing, then, will fold over all of the items produced by the given
producer (all of the `Word8`s) with our frequency-table-building.

We then use `sum` from `Data.Foldable`, which sums up all the numbers in our
frequency map.  Then we use what we learned about the State monad in [Part
1][part 1] to build our tree (review [Part 1][part 1] if you do not understand
the declaration of `tree`).  `tree` is a `Maybe (PreTree Word8)`; we then tag
on the length to our `tree` using `fmap` and the TupleSections extension.
(That is, `(,y)` is sugar for `(\x -> (x,y))`).

#### The Encoding Pipeline

Once we have that, we can get onto the actual encoding process: the second
pass.

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
`len` and `tree` one after the other.  This is because `(a, b)` has a `Binary`
instance.  We'll read it back in later as a tuple, but it actually
doesn't matter, because the `Binary` instance for tuples is just
putting/getting each item one after the other.

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

<div class="note">
**Note**

Just as a warning, much of this section might be considered obsolute or out of
date on the next breaking release of *pipes-bytestring*, which replaces the
`PB.pack` iso with a pair of lenses.  Just be aware!  Once it's out, I'll
replace/augment this and possibly much of this post with new material
addressing the library redesign.
</div>

Now, the next step could have been to pipe in the stream of `Word8` into
`PP.map B.pack`, which takes each incoming `Word8` and "packs" them into a
singleton `ByteString`.

However, this is a bad idea: you are basically creating a full `ByteString`
and triggering a write on every incoming byte.  Instead, we use

~~~haskell
(view PB.pack) bytesOut
~~~

Where `view PB.pack` is a "producer transformer" (like `dirsBytes`).

The implementation of `view PB.pack` basically repeatedly takes chunks of
`Word8`s and then packs them into a big `ByteString`, and uses ["smart
chunking"][smartchunking] that allows us to maximize both space and time
usage.  Pretty cool! In a way it's a lot like our `dirsBytes` producer
transformer (chunking `Direction`s into `Word8`s), except with "smart chunk
sizes".

[smartchunking]: http://www.haskellforall.com/2013/09/perfect-streaming-using-pipes-bytestring.html

<div class="note">
**Aside**

`view PB.pack` is an idiom that comes from the *lens* library.

Basically, `PB.pack` from *pipes-bytestring* (and not the one from
*bytestring*), contains "producer transformers" ---  two, actually: a
`Producer Word8 m r -> Producer ByteString m r`, and a `Producer ByteString m
r -> Producer Word8 m r`.

In fact, in a way, you can think of `PB.pack` as simply a tupling of the two
transformer functions together.  Practically, the tupling/packaging them
together is useful because the two are inverses of each other.

Simply speaking, `view` is a function from *lens* that takes the "first" part
(kind of like `fst`) of that "tuple".

So *pipes-bytestring* gives us this tupling of two transformers (one going
from `Word8` to `ByteString` and one going from `ByteString` to `Word8`),
called `PB.pack`, and we use `view` to get the "forward" direction, the first
one.

There are many reasons why *pipes-bytestring* gives us these useful
functions "tied together" in `pack` instead of just them separately, and they
have to do with ways you can manipulate them with *lens*.  But that's another
story :)
</div>

<div class="note">
**Aside**

Perhaps more formally stated, `PB.pack` is an "isomorphism"; if `toBS` is your
forward transformer and `toW8` is your backwards transformer, `PB.pack` is
`iso toBS toW8`.

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

Ok, now that we have a stream of `ByteString`, all that's left to do is add on
a final pipe/consumer that writes it to disk.  For that, we have `PB.toHandle
out`, which is a consumer that takes in `ByteString` and writes each incoming
`ByteString` to the given file handler.

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
% 1.8M warandpeace.enc
% 3.1M warandpeace.txt
~~~

Cool, we compressed it to 58% of the original file size.  Not bad!  Using
`gzip` with default settings gives a compression of 39%, so it's not the best,
but it's something.  If we take out the encoding part of the script, we can
see that the metadata (the length and the dictionary) itself only takes 259
bytes (which is negligible) --- so 58% is pretty much the asymptotic
compression rate.

At this point it's not as snappy (performance wise) as we'd like; a
compressing a 3.1M file is not "super slow" (it takes about seven seconds on
my computer), but you probably won't be compressing a gigabyte.  We'll look
into performance in a later post!

Decoding
--------

(Remember again that download [decode.hs][] is also available online from
github!  Again, be sure to also grab [Huffman.hs][], [PQueue.hs][], and
[PreTree.hs][], and [Weighted.hs][] from posts past.)

!!![decode.hs]:huffman/decode.hs


### Design

Let's try to see the plan for our decoding script, applying what we learned
before.  What components do we need?

1.  First, a component producing decoded `Word8`s (that will be `view
    PB.pack`'d into a component producing decoded `ByteString`s with smart
    chunking)
    1.  A producer that reads in `ByteString`s from a file and sends them
        downstream.
    2.  A pipe that unpacks those `ByteString`s into `Word8`s and sends each
        one down.
    3.  A pipe that "unpacks" those `Word8`s into `Direction`s and sends
        *those* down.
    4.  A pipe that traverses down the Huffman encoding tree following the
        incoming `Direction`s, and emits a decoded `Word8` every time it
        decodes a value.
2.  A component consuming the incoming `ByteString`s, and writing them to our
    output file.


### Down to it

Luckily we can use most of the concepts we learned in writing the encoding
script to write the decoding script; we also have a less imports, so it's a
sign that decoding is going to be slightly simpler than encoding.

~~~haskell
!!!huffman/decode.hs "-- General imports" "-- Pipes imports" "-- Working with Binary" "-- Huffman imports"
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
decoding from the yielding/continuation/recursion/pipe-ness of it all.
Ideally we'd like to be able to separate the logic.  This isn't *too*
necessary, but doing this will expose us to some nice *pipes* idioms :)

One way we can do it is to turn `searchPT` into a `Consumer'` (a `Consumer`
with the ends not sealed off) that consumes `Direction`s and *returns*
resulting `Word8`s.

Then we use `(>~ cat)`, which turns a `Consumer'` into something that is
forever consuming and re-yielding --- in essence, it turns a `Consumer'`
returning values into a `Pipe` repeatedly yielding the returned values.

~~~haskell
!!!huffman/decode.hs "searchPT ::"
~~~

The logic is slightly cleaner; the gain isn't that much, but just being able
to have this separation is nice.  We also get rid of the "infinite loop";
`searchPT'` only "runs once".  In any case, this is also a good exposure to
`(>~)`!

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

~~~bash
$ ghc -O2 decode.hs
$ ./decode warandpeace.enc warandpeace.dec
$ md5sum warandpeace.txt
% 3c8168e48f49784ac3c2c25d15388e96  warandpeace.txt
$ md5sum warandpeace.dec
% 3c8168e48f49784ac3c2c25d15388e96  warandpeace.dec
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
