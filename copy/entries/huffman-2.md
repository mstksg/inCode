Streaming Huffman Compression in Haskell (Part 2: Binary and Pipes)
===================================================================

Categories
:   Haskell
:   Tutorials
Tags
:   haskell
:   monads
CreateTime
:   2014/04/03 01:53:53
PostDate
:   Never
Series
:   Huffman Compression
:   Beginner/Intermediate Haskell Projects
Identifier
:   huffman-2

Continuing on this series of beginner/intermediate projects for newer haskell
users, let's look back at our huffman encoding project.

In our last post we went over two types of binary trees implemented as
algebraic data structures in Haskell, and also a scheme for assembling a
huffman encoding tree using the State monad.

Now let's look at serializing and unserializing our prefix trees for easy
storage, and then at actually using them to encode and decode!

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

#### Wrapping it up

And finally, to tie it all together:

~~~haskell
!!!huffman/PreTree.hs "instance Binary a => Binary (PreTree a)" huffman-encoding
~~~

### Testing it out

However way we decide to write our `Binary` instance, let's test it all out.

~~~haskell
λ: let t = fromJust $ runBuildTree "hello world"
λ: let encoded = encode t
λ: :t encoded
encoded :: ByteString       -- a string of bytes
λ: let decoded = decode t :: PreTree Char
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

Encoding and Decoding
---------------------

Now that we've got that out of the way, let's work on actually encoding and
decoding.

### Encoding

So, basically, we encode a character in a huffman tree by path you take to
reach the character.

Let's represent this path as a list of `Direction`s:

~~~haskell
data Direction = DLeft
               | DRight
               deriving (Show, Eq)

type Encoding = [Direction]
~~~

Eventually, an `Encoding` will be turned into a `ByteString`, with `DLeft`
representing the 0 bit and `DRight` representing the 1 bit.  But we keep them
as their own data types now because everyone hates [boolean blindness][].
Instead of keeping a `True` or `False`, we keep data types that actually carry
semantic meaning :)  And we can't do silly things like use a boolean as a
direction...what the heck?  Why would you even want to do that?

#### Direct search

Here's a direct search.  It's horribly inefficient.

~~~haskell
findPT :: Eq a => a -> PreTree a -> Maybe Encoding
findPT x t0 = reverse <$> go t0 []
  where
    go (PTLeaf y      ) enc | x == y    = Just enc
                            | otherwise = Nothing
    go (PTNode pt1 pt2) enc = go h1 (DLeft:ds) <|> go h2 (DRight:ds)
~~~







