Streaming Huffman Compression in Haskell (Part 2: Binary, Searches, and Writer)
===============================================================================

Categories
:   Haskell
:   Tutorials
Tags
:   haskell
:   monads
CreateTime
:   2014/04/03 01:53:53
PostDate
:   2014/04/11 10:12:55
Series
:   Huffman Compression
:   Beginner/Intermediate Haskell Projects
Identifier
:   huffman-2

Continuing on this series of beginner/intermediate projects for newer Haskell
users, let's look back at our Huffman encoding project.

In our last post we went over two types of binary trees implemented as
algebraic data structures in Haskell, and also a scheme for assembling a
Huffman encoding tree using the State monad.

Now let's look at serializing and unserializing our prefix trees for easy
storage, and then at actually using them to encode and decode!  And maybe
learn about the Writer monad along the way!

Binary
------

There are a couple of serialization libraries in Haskell; the dominant one is
[binary][], but [cereal][] is also not uncommon.  The two diverge on several
design points, and you can read up on them in the documentation for *cereal*.
We'll be using *binary* for the this tutorial; among many reasons, for its
easy integration with the *pipes* library we will be working with later.

[binary]: http://hackage.haskell.org/package/binary
[cereal]: http://hackage.haskell.org/package/cereal

### The Easy Way

So let's make `PreTree` serialize/unserializable.

The easy way is to enable the `DeriveGeneric` language extension on GHC, use
`deriving (Generic)` when we define our `PreTree`, and then:

~~~haskell
instance Binary a => Binary (PreTree a)
~~~

And...that's it!  We just auto-generated functions to serialize and
deserialize our `PreTree`s (if what they contain is itself serializable).

In real life, we would do this.  However, for the sake of learning, let's dig
a bit more into the `Binary` typeclass.

### The *Other* Easy Way

So the big crux of *binary* is the `Binary` typeclass:

~~~haskell
class Binary t where
    put :: t -> Put
    get :: Get t
~~~

where `Put` and `Get` are sort of "instruction objects for putting/getting
binary".  `Get` is a monad, and `Put` is a wrapped `PutM`, which is a writer
monad.  (To be more specific, `Put` is `PutM ()`, because the final action has
no result and only "writes")

So `Binary` things are things that you can serialize (with the intructions in
`put`) and deserialize (with the instructions in `get`).

Luckily, because of Haskell's great composition tools, assembling these
instructions by hand are easy peasy!

#### Put

Let's define our own custom `Put` for our `PreTree`s:

~~~haskell
!!!huffman/PreTree.hs "putPT ::" huffman-encoding
~~~

This all should be fairly readable and self-explanatory.

*   "To put a `PTLeaf x`, first put a flag saying you have a leaf, then
    put the value of `x`."

*   "To put a `PTNode pt1 pt2`, first put a flag saying you have a node, then
    put both trees."

Due to how monads and pattern matching work, the whole thing is pretty
expressive, pleasant to read, and satisfying to write.

The only slightly annoying thing is that we subject ourselves to [boolean
blindness][] by using `True` or `False`; we have to keep track of what means
what.  Alternatively, we can create our own binary data types, `data PTType =
IsNode | IsLeaf`, and `put` *that*, instead...but in this case it might not be
so bad to live with boolean blindness for now.

[boolean blindness]: http://existentialtype.wordpress.com/2011/03/15/boolean-blindness/

#### Get

Now let's define our own custom `Get`:

~~~haskell
!!!huffman/PreTree.hs "getPT ::" huffman-encoding
~~~

This also shouldn't be too bad!

*   "Get" the boolean flag, to tell you if you have a leaf or a node.
*   If it's a leaf, then `get` the data inside the leaf, and wrap it in a
    `PTLeaf`.
*   If it's not, `get` the next two `PreTree a`'s, and put them both in a
    `PTNode`.

The neat thing here is that `get` is polymorphic in its return type.  We know
that the first `get` expects a `Bool`, so it knows to parse a `Bool`.  We know
that the second `get` expects an `a`, so it knows to parse an `a`.  We know
that the final two `get`s both expect `PreTree a`'s, so it nows what to parse
for that too.

Hooray for type inference!

If you're not familiar with the `f <$> x <*> y` idiom, you can consider it to
be the same thing as `f x y`, except that `x` and `y` are "inside" things:

~~~haskell
λ: (+) 1 4
5
λ: (+) <$> Just 1 <*> Just 4
Just 5
~~~

Where `(<$>)` and `(<*>)` come from `Control.Applicative`.  We call this style
"applicative style", in the biz.

#### Wrapping it up

And finally, to tie it all together:

~~~haskell
!!!huffman/PreTree.hs "instance Binary a => Binary (PreTree a)" huffman-encoding
~~~

### Testing it out

However way we decide to write our `Binary` instance, let's test it all out.

~~~haskell
λ: let (Just pt) = runBuildTree "hello world"
λ: let encoded = encode pt
λ: :t encoded
encoded :: ByteString       -- a string of bytes
λ: let decoded = decode encoded :: PreTree Char
λ: decoded
PQTNode (PTNode (PTNode (PTLeaf 'h')
                        (PTLeaf 'e')
                )
                (PTNode (PTLeaf 'w')
                        (PTLeaf 'r')
                )
        )
        (PTNode (PTLeaf 'l')
                (PTNode (PTNode (PTLeaf 'd')
                                (PTLeaf ' ')
                        )
                        (PTLeaf 'o')
                )
        )
λ: decoded == t
True
~~~

Neat!  We can also write it to a file and re-read:

~~~haskell
λ: encodeFile "test.dat" t
λ: t' <- decodeFile "test.dat" :: IO (PreTree Char)
λ: t'
PQTNode (PTNode (PTNode (PTLeaf 'h')
                        (PTLeaf 'e')
                )
                (PTNode (PTLeaf 'w')
                        (PTLeaf 'r')
                )
        )
        (PTNode (PTLeaf 'l')
                (PTNode (PTNode (PTLeaf 'd')
                                (PTLeaf ' ')
                        )
                        (PTLeaf 'o')
                )
        )
λ: t' == t
True
~~~

And this looks like it works pretty well!

Encoding
--------

Now that we've got that out of the way, let's work on actually encoding and
decoding.

So, basically, we encode a character in a huffman tree by path you take to
reach the character.

Let's represent this path as a list of `Direction`s:

~~~haskell
!!!huffman/PreTree.hs "data Direction =" "type Encoding =" huffman-encoding
~~~

Eventually, an `Encoding` will be turned into a `ByteString`, with `DLeft`
representing the 0 bit and `DRight` representing the 1 bit.  But we keep them
as their own data types now because everyone hates [boolean blindness][].
Instead of keeping a `True` or `False`, we keep data types that actually carry
semantic meaning :)  And we can't do silly things like use a boolean as a
direction...what the heck?  Why would you even want to do that?  How is "true"
a direction?

### Direct search

Here's a naive recursive direct (depth-first) search.

~~~haskell
!!!huffman/PreTree.hs "findPT ::" huffman-encoding
~~~

The algorithm goes:

1.  If you find a `PTLeaf`, if the data matches what you are looking for,
    return the current path in a `Just`.  If not, this is a dead-end; return
    `Nothing`.

2.  If you find a `PTNode`, search the left branch adding a `DLeft` to the
    current path, and the right branch adding a `DRight` to the current path.
    Use `(<|>)` to perform the search lazily (ie, stop after the first
    success).

~~~haskell
λ: let (Just pt) = runBuildTree "hello world"
λ: findPT t 'e'
Just [DLeft, DLeft, DRight]
λ: findPT t 'q'
Nothing
~~~


While it is clearly horribly inefficient, it does serve as a nice clean
example of a depth-first search (which exits as soon as it finds the goal),
and probably a nice reference implementation for us to reference later.

Its inefficiency lies in many things --- chiefly of those being the fact that
Huffman trees don't give you any real help as a search tree, and nothing short
of a full depth-first traversal would help you.  Also, you probably don't want
to do this every time you want to encode something; you'd want to have some
sort of memoing and cacheing, ideally.

### Pre-searching

We can sort of "solve" both of these problems this by traversing through our
`PreTree` and adding an entry to a `Map` at every leaf.  This fixes our
repetition problem by memozing all of our results into a map...and it fixes
our search problem because `Map`s are an ordered binary search tree with
efficient O(log n) lookups.

We can do this using the `State` monad as we learned about in [the previous
post][state], by using a `Map a Encoding` as an underlying state and adding to
it using `modify` at every step.

[state]: http://blog.jle.im/entry/streaming-huffman-compression-in-haskell-part-1-trees#the-state-monad

However, let's note something interesting --- all we ever "do" to the state is
"add" to it.  We never `get` it, and we *never* need to "branch" on the
results of our stateful actions.  Our stateful actions don't even return
anything ever!

We said earlier that, for things that don't require "branching", one can use a
fold instead of State.  But a fold in this case might be a little unweildy,
because each "write" depends on how deep in the tree you are.

Let's look at our next big general useful monad: Writer.

#### The Writer Monad

Here are some key flags that you might want a Writer:

1.  All you ever do is some sort of "append" or "adding" to your underlying
    state.
3.  You never need to decide anything based on the current state.
4.  Your entire computation revolves around building up a giant thing by
    continuously adding little things to it.
5.  Your "final computation" returns `()` in the end.

(Note that these things are signs you might want to use a Writer monad, but
actually, the Writer monad has more uses than just the ones described above.)

The Writer monad is actually a lot simpler than the State monad, so don't
worry!

Like `State`, `Writer` is also a newtype wrapper; let's compare them!

~~~haskell
newtype State  s a = State (s -> (a, s))
newtype Writer w a = Writer      (a, w)
~~~

And so it looks like `Writer` is just a "dumb `State`".  It's a `State` that
*doesn't have direct access to its state*.

Semantically, we say that `(a, w)` (or `Writer w a`) is a Writer with a
*result* `a` and an *accumulator* `w`.  `a` is the type of the result of the
Writer, and `w` is the type of our accumulator.  We can call them "data with
attached accumulators".

So what would `andThen` look like, for `(a, w)`?  How can we "sequence" two
"data with attached accumulators"?  Well...we can simply combine their
accumulators!

~~~haskell
andThen :: Monoid w => (a, w) -> (b, w) -> (a, w)
andThen (_, acc) (x, acc') = (x, acc <> acc')
~~~

I'm assuming you already know about the Monoid typeclass and `<>`.  If you
don't, then to put it briefly, a `Monoid` is something that implements `<>`
("`mappend`"):

~~~haskell
(<>) :: Monoid a => a -> a -> a
~~~

which is an associative "combining function".  "Give me two `a`'s and I'll
give you a combined `a`."  It is important that this combining operator is
associative, meaning that `x <> (y <> z)` is the same as `(x <> y) <> z`.

Monoids also implement `mempty :: Monoid a => a`, which is the element that,
when `<>`'d with something else, does not change that something else.

The canonical example of a Monoid is the list `[a]`, also known as the "free
monoid".  For lists, `<>` is `++` and `mempty` is `[]`:

~~~haskell
x ++ (y ++ z) == (x ++ y) ++ z
      x ++ [] == x
      [] ++ x == x
~~~

Basically, monoids represent "combinable things".

So, when we `andThen` two writer tuples, we combine the `w`.

I'll leave `andThenWith :: (a, w) -> (a -> (b, w)) -> (b, w)`  up to you to
try to implement yourself.

There's also `return`:

~~~haskell
return :: a -> (a, w)
return x = (x, mempty)
~~~

Where `return` just has the item with an empty/"fresh" accumulator.

Again, in real life, we can't define a typeclass instance on a type synonym,
and we actually can't even define an instance directly on `(a, w)` (can you
see why?).  So again, the canonical implementation comes in the
[transformers][] library, and wraps the `(a, w)` in a newtype as `Writer w a`,
just like for State.  It also offers a few nice primitives, but we will only
be using one:

[transformers]: http://hackage.haskell.org/package/transformers

~~~haskell
tell :: Monoid w => w -> Writer w a
tell acc = Writer ((), acc)
~~~

Basically, `tell` is just a simple `Writer` that returns no result, but adds
something that will be `<>`'d to the accumulator.

##### Sample Writer

Let's take a whack at an example computation using the Writer monad.

Let's re-write a simple fold that goes down a list and adds up every even
number in that list.

~~~haskell
addEven :: Integral a => a -> Writer (Sum a) ()
addEven x | even x    = tell (Sum x)
          | otherwise = return ()

addAllEvens :: Integral a => [a] -> Writer (Sum a) ()
addAllEvens []     = return ()
addAllEvens (x:xs) = do
    addEven x
    addAllEvens xs

-- or, using higher order functions
addAllEvens' :: Integral a => [a] -> Writer (Sum a) ()
addAllEvens' = mapM_ addEven

runAddAllEvens :: Integral a => [a] -> Sum a
runAddAllEvens = execWriter addAllEvens
~~~

~~~haskell
λ: runAddAllEvens [2,7,3,5,4,7,8]
Sum 14
~~~

`Num a => Sum a` is a Monoid, whose "combining"/"merging" function is simply
to add the values inside.  `Sum 1 <> Sum 2 == Sum 3`.  `mempty` is, of course,
`Sum 0`.

`execWriter` unwraps the `Writer` newtype, and returns only the `w`.

#### Pre-searching, with Writer

With this in mind, let's build up our memoized lookup tree.

~~~
!!!huffman/PreTree.hs "ptTable ::" huffman-encoding
~~~

We take advantage of the fact that `Map k v` is in fact a monoid, and that
`map1 <> map 2` means "adding" the two maps together.  We use the `singleton`
function, where `singleton k v` means making a new map with only one entry ---
with key `k` of value `v`.  So when we "append" a singleton to a map, it's
like simply adding a key/value pair.

Notice that this has pretty much the exact same structure as our previous
depth-first search:

~~~haskell
!!!huffman/PreTree.hs "findPT ::" huffman-encoding
~~~

Except instead of returning a value based on equality at the leaves, we
"write" it.  And instead of "choosing between" the two branches of a node, we
"sequence"/do them both.

### Lookup, Act 2

So now that we have our lookup table, our new lookup/find function is both
simple and performant:

~~~haskell
!!!huffman/PreTree.hs "lookupPTTable ::" huffman-encoding
~~~

given, of course, that we generate our table first.

~~~haskell
λ: let (Just pt) = runBuildTree "hello world"
λ: let tb = ptTable pt
λ: lookupPTTable tb 'e'
Just [DLeft, DLeft, DRight]
λ: lookupPTTable tb 'q'
Nothing
~~~

### Encoding many

Now, we'd like to be able to decode an entire stream of `a`'s, returning a
list of the encodings.

~~~
!!!huffman/PreTree.hs "encodeAll ::" huffman-encoding
~~~

This is a bit dense!  But I'm sure that you are up for it.

1.  First, we build the lookup table and call it `tb`.

2.  Then, we map `lookupPTTable tb` over our list `xs`, to get a list of type
    `[Maybe Encoding]`.

3.  Then, we use `sequence`, which in our case is `[Maybe a] -> Maybe [a]`.
    It turns a list of Maybe's into a list inside a Maybe.  Recall the
    semantics of the Maybe monad: If you ever encounter a `Nothing`, the
    *whole thing* is a `Nothing`.  So in this case, if *any* of the inputs are
    not decodable, *the entire thing is Nothing*.

    ~~~haskell
    λ: sequence [Just 5, Just 4]
    Just [5,4]
    λ: sequence [Just 6, Nothing]
    Nothing
    ~~~

    Note that the standard libraries provide a synonym for `sequence . map`
    --- `mapM`.  So we could have written it as `mapM (lookupPTTable t)
    xs`...but that is significantly less clear/immediately understandable.

4.  Recall that our `sequence` left us with a `Maybe [Encoding]`...but we only
    want `Maybe Encoding`.  So we can use `(<$>)` to `concat` all of the
    `Encoding`s inside the Maybe.

~~~haskell
λ: let (Just pt) = runBuildTree "hello world"
λ: encodeAll pt "hello world"
Just [DLeft, DLeft, DLeft, DLeft, DLeft, DRight, DRight, DLeft, DRight, DLeft,
DRight, DRight, DRight, DRight, DRight, DLeft, DRight, DLeft, DRight, DLeft,
DRight, DRight, DRight, DLeft, DRight, DRight, DRight, DLeft, DRight, DRight,
DLeft, DLeft]
λ: encodeAll pt "hello worldq"
Nothing
~~~

Welp, that's half the battle!

Decoding
--------

For huffman trees, decoding is the much simpler process.  Simply traverse down
the tree using the given encoding and return a value whenever you reach a
leaf.

~~~haskell
!!!huffman/PreTree.hs "decodePT ::" huffman-encoding
~~~

The logic should seem pretty familiar.  The main algorithm involves going down
the tree, "following" the direction list.  If you reach a leaf, then you have
found something (and return the directions you haven't followed yet).  If you
run out of directions while on a node...something has gone wrong.

~~~haskell
λ: let (Just pt)  = runBuildTree "hello world"
λ: let (Just enc) = encodeAll pt "hello world"
λ: decodePT pt enc
Just ('h', [DLeft, DLeft ...])
~~~


### Decoding many

We'd like to repeatedly iterate this until we have consumed our entire
encoding.

Basically, starting with a list of encodings, we want to continually chop it
up and build a list from it.

This sounds a lot like the `Data.List` function `unfoldr`:

~~~
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
~~~

`unfoldr` makes a list by applying your function repeatedly to a
"de-cumulator", carrying the state of the decumulator, and stopping when your
function returns `Nothing`.  You can think of it as the "opposite" of `foldr`.

Using `unfoldr`, we can write a `decodeAll`:

~~~haskell
!!!huffman/PreTree.hs "decodeAll ::" huffman-encoding
~~~

~~~haskell
λ: let (Just pt)  = runBuildTree "hello world"
λ: let (Just enc) = encodeAll pt "hello world"
λ: decodeAll t enc
"hello world"
~~~

Which works exactly as we'd like!

Testing
-------

We can write a utility to test our building:

~~~haskell
!!!huffman/Huffman.hs "testTree ::" huffman-encoding
~~~

`testTree` should be an identity; that is, `testTree xs === xs`.

~~~haskell
λ: testTree "hello world"
"hello world"
λ: testTree "the quick brown fox jumps over the lazy dog"
"the quick brown fox jumps over the lazy dog"
~~~

### QuickCheck

Now that we have a neat proposition, we can use `quickcheck` on it, from the
great [QuickCheck][] library.  `quickcheck` will basically test our
proposition `testTree xs == xs` by generating several random `xs`'s.

[QuickCheck]: http://hackage.haskell.org/package/QuickCheck

~~~haskell
λ: import Test.QuickCheck
λ: :set -XScopedTypeVariables
λ: quickCheck (\(xs :: String) -> testTree xs == xs)
*** Failed! Falsifiable (after 3 tests and 2 shrinks):
"a"
~~~

#### Failure!

Oh!  We failed?  And on such a simple case?  What happened?

If we look at how `"a"` is encoded, it'll become apparent:

~~~haskell
λ: let (Just pt) = runBuildTree "aaa"
λ: pt
PTLeaf 'a'
λ: findPT pt 'a'
Just []
λ: encodeAll pt "aaaaaaaaaaa"
Just []
~~~

Ah.  Well, that's a problem.  Basically, our input string has
["zero" entropy][entropy], according to typical measurements.  So we cannot
naively huffman encode it.

[entropy]: http://en.wikipedia.org/wiki/Entropy_(information_theory)

#### Success!

There are a few ways to deal with this.  The most "immediate" way would be to
realize that `decodeAll` is partial, and will actually never terminate if the
given tree is a singleton tree.  We can write a "safe" `decodeAll`:

~~~haskell
!!!huffman/PreTree.hs "decodeAll' ::" huffman-encoding
~~~

And also a "safe" `testTree`, taking advantage of the Monad instance for
Maybe.

~~~haskell
!!!huffman/Huffman.hs "testTree' ::" huffman-encoding
~~~

So we can now quickcheck:

~~~haskell
λ: quickCheck (\(xs :: String) -> testTree' xs `elem` [Nothing, Just xs])
+++ OK, passed 100 tests.
~~~

Hooray!

### Re: Testing

All I'll admit that I didn't even anticipate the degenerate singleton tree
case until I decided to add a quickcheck section to this post.  It just goes
to show that you should always test!  And it also shows how easy it is to
write tests in quickcheck.  One line could mean five unit tests, and you might
even test edge/corner cases that you might have never even thought about!

For example, we probably should have tested `lookupPTTable` against `findPT`,
our reference implementation :)

Next Time
---------

We're almost there!

For our last section, we are going to be focusing on pulling it all together
to make a streaming compression/decompression interface that will be able to
read a file and encode/decode into a new file as it goes, in constant memory.
We will also be looking at some optimization tricks we can do to get things
just right, and other things to wrap up.

