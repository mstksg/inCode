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

### Binary duals

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
putPT :: Binary a => PreTree a -> Put
putPT (PTLeaf x) = do
    put True                    -- signify we have a leaf
    put x
putPT (PTNode pt1 pt2) = do
    put False                   -- signify we have a node
    put pt1
    put pt2
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

~~~
getPT :: Binary a => Get (PreTree a)
getPT = do
    isLeaf <- get
    if isLeaf
      then PTLeaf <$> get
      else PTNode <$> get <*> get
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




