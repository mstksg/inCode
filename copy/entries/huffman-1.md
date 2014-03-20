Implementing Huffman compression/encoding in Haskell
===================================================================

Categories
:   Haskell
:   Tutorials
Tags
:   haskell
CreateTime
:   2014/03/19 01:25:50
PostDate
:   Never
Series
:   Huffman Compression
Identifier
:   huffman-1

So you're learning Haskell and are looking for some projects that aren't super
trivial, are familiar enough that you can use what you already know, and are
difficult enough to maybe help you learn new things.

Let's take a go at [Huffman encoding][] in Haskell.  We will look at two types
of binary trees, which we use to implement immutable/persistent priority
queues and prefix trees.  We'll play around with the State monad a bit,
explore some useful typeclasses (like Semigroup, Monoid, and Ord), learn how
to serialize, marshall, and unmarshall data structures, and also look at how
to load data from a file and write to another in a pure way, avoiding lazy IO
using the ever-more-popular *[pipes][]* library.  And hopefully we learn some
neat haskell idioms!

[Huffman encoding]: http://en.wikipedia.org/wiki/Huffman_coding
[pipes]: http://hackage.haskell.org/package/pipes

Prefix trees & Huffman coding
-----------------------------

You might have encountered this in the past, but Huffman encoding solves the
problem of finding the optimal binary prefix code to encode a string.

I'll leave you to read [the wikipedia article][Huffman encoding], which
explains it much better than I could.  Basically, binary prefix codes are nice
because you don't have to encode any "stop" symbol --- as soon as you reach a
leaf of the tree, you know that you have found a letter, and can move on.

Huffman trees are built from the bottom-up using priority queues.  The two
lowest-frequency nodes are continually "popped" from the queue, combined into
a new node, and placed back in the queue.

Our first challenge --- representing a Huffman tree as a data structure!

### The Tree

So some properties about prefix trees that might be useful to us --- all data
is stored in the leaves, and all internal nodes have exactly two children.
This sounds like the perfect candidate for an Algebraic Data Structure.

~~~haskell
data PreTree a = PTLeaf a
               | PTNode (PreTree a) (PreTree a)
               deriving (Show, Eq)
~~~

We leave the type parameterized on `a` (which is like a template/generic in
C++/Java) so we can decide what to put into it later.  Also, leaving it
generic means that we might be able to play around with it as a functor,
applicative, or monad, and who knows how useful those would end up being?  (If
you didn't understand that, don't worry!)

#### `PreTree` operations

So, what sort of things are we going to want to do with our `PreTree`?

Well..first of all, we might want a way to put something into an empty tree
--- create a leaf containing just that data.

That function is sort of embarrassingly easy:

~~~haskell
makePT' :: a -> PreTree a
makePT' x = PLeaf x
~~~

Remember, that's `PLeaf` is a data constructor that "creates" a `PreTree` when
you use `PLeaf x`.

However, something like this is just begging to be eta-reduced, and we can
simplify it as:

~~~haskell
makePT :: a -> PreTree a
makePT = PLeaf
~~~

Which does the same thing.  Basically, `PLeaf` is already a function `a ->
PreTree a`...so `makePT` is literally just `PLeaf`.


~~~haskell
λ: let pt = makePT 'c'
λ: :t pt
PreTree Char
λ: pt
PLeaf 'c'
~~~

Now, we might also want a way to "merge" two `PreTree a`'s.  This is at the
heart of building the tree in the first place...successively merge two trees
until everything is in one giant tree.

This isn't too bad either:

~~~haskell
mergePT' :: PreTree a -> PreTree a -> PreTree a
mergePT' t1 t2 = PNode t1 t2
~~~

Which, from what we saw before, can just be written as:

~~~haskell
mergePT :: PreTree a -> PreTree a -> PreTree a
mergePT = PNode
~~~

~~~haskell
λ: let pt1 = makePT 'c'
λ: let pt2 = makePT 't'
λ: let pt3 = pt1 `mergePT` pt2
λ: :t pt3
PreTree Char
λ: pt3
PNode (PLeaf 'c') (PLeaf 't')
~~~

Hm.  Maybe that's a bit too easy.  Feels a little unsettling, isn't it?

Welcome to Haskell!

### Weighting things

We're going to need some way of comparing the weights/priorities of two
`PreTree`s when we are assembling the tree.  Let's introduce a data type that
includes both a `PreTree` and a weight.

~~~haskell
data Weighted w a = Weighted w a
~~~

We will say that a `Weighted w a` is some `a` associated with a weight `w`.

We can create, say, a `PreTree` containing the character 'a', weighted with
integer 1:

~~~haskell
λ: Weighted 1 (makePLeaf 'a')
Weighted 1 (makePLeaf 'a') :: Weighted Int (PreTree Char)
~~~

This weighted `PreTree` is pretty useful, let's give it an alias/typedef:

~~~haskell
type WPreTree = Weighted Int
~~~

Let's make the same functions for `WPreTree` as we did for `PreTree`:

~~~haskell
makeWPT :: Int -> a -> WPreTree a
makeWPT w = Weighted w . makePT
~~~

The above basically says "to make a `WPreTree` with weight `w`, first
`makePT` it, and then add it to a `Weighted w`.

~~~haskell
λ: let pt = makeWPT 1 'w'
λ: :t pt
Weighted Int (PreTree Char)
λ: pt
Weighted 1 (PLeaf 'w')
~~~

We will also want to merge two `WPreTree`s:

~~~haskell
mergeWPT :: WPreTree a -> WPreTree a -> WPreTree a
mergeWPT (Weighted w1 pt1) (Weighted w2 pt2)
    = Weighted (w1 + w2) (mergePT pt1 p2)
~~~

so that the total weight is the sum of the weights of the two subtrees.

Finally, the entire point of having weighted things is so that we can compare
them and impose some total ordering.  Haskell has a typeclass that abstracts
these comparing operations, `Ord`:

~~~haskell
instance Ord w => Ord (Weighted w a) where
    compare (Weighted w1 _) (Weighted w2 _) = compare w1 w2
~~~

Which says that `Weighted w a` is an `Ord` (is orderable/comparable) if `w` is
an `Ord`...and to compare two `Weighted w a`'s, you compare the `w`'s.

~~~haskell
λ: makeWPT 2 'a' > makeWPT 3 'b'
False
λ: makeWPT 4 't' == makeWPT 4 'k'
True
~~~

Priority Queues
---------------

There are some great priority queue libraries on Hackage, like [PSQueue][].
However, for fun, we're going to be making our own!  Yay!

[PSQueue]: http://hackage.haskell.org/package/PSQueue

### Skew heaps

A traditional approach to making efficient priority queues is to use a
[heap][], a tree with insertion algorithms that make sure the root of the tree
is the most prioritized element and that the tree stays balanced.  Heaps make
heavy use of stateful mutation to do this, and while it's not so hard to do
this in Haskell, we might consider a 'pure' version of a heap: a [skew
heap][].

[heap]: http://en.wikipedia.org/wiki/Heap_(data_structure)
[skew heap]: http://en.wikipedia.org/wiki/Skew_heap

A skew heap is a heap that doesn't explicitly maintain its balance, but
maintains "heap ordering" (parents are always higher priority than their
children).

I'll leave it to the wikipedia article to do most of the explaining because
they have pretty pictures, but here is the gist of it --- skew heaps have only
three operations: making new (singleton) one, merging two skew heaps, and
popping off the root.  Traditional "insert" is done by making a new skew heap
with one element, and merging it with the main heap.

Merging is embarrassingly simple: The higher-priority root becomes the new
root, and the lower-priority root is merged with the child tree of that new
root.  Some left-right flipping of branches is done to make sure things tend
to stay balanced.  Pictures provided in the wikipedia article.

Popping the root is simple too; just take the root, and merge its two
sub-trees to make the new tree.

This is a new type of binary tree, so let's define a new data type:

~~~haskell
data SkewHeap a = SEmpty
                | SNode a (SkewHeap a) (SkewHeap a)
                deriving (Show, Eq)
~~~

Creating a new `SkewHeap` with one item:

~~~haskell
makeSH :: a -> SkewHeap a
makeSH x = SNode x SEmpty SEmpty
~~~

Popping the root off of a skew tree:

~~~haskell
popSH :: Ord a => SkewHeap a -> (Maybe a, SkewHeap a)
popSH SEmpty          = (Nothing, SEmpty)
popSH (SNode r h1 h2) = (Just r , mergeSH h1 h2)
~~~

We make it return a potential result (`Maybe a`), and the resulting new popped
tree.  The result is `Maybe a` because we might potentially not be able to pop
anything!  We also require an `Ord` constraint because in order to merge two
skew heaps, the data must be comparable.

Finally, the hardest piece of code so far: merging two skew heaps:

~~~haskell
mergeSH :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
mergeSH SEmpty h = h
mergeSH h SEmpty = h
mergeSH l@(SNode nl ll rl) r@(SNode nr lr rr)
    | nl < nr    = SNode nl (mergeSH lr r) ll
    | otherwise  = SNode nr (mergeSH rr l) lr
~~~

Hopefully this is very pleasing to read --- it reads a lot like a
specification, or a math formula:

1.  Merging any skew heap with an empty heap is that same skew heap.
2.  When merging two heaps, the new heap is an `SNode` with the smaller root,
    whose children are the merge of the smaller tree and the original
    children. (Admittedly, the math/code is a bit more expressive than English
    in this case)

(Remember that in our case, the *lower* value/weight is the *higher*
priority.)

We require an `Ord` constraint because we compare the node element on the
third case.


### Priority Queue interface

Ok, neat!

Let's wrap this up in a tidy interface/API for a `PQueue` type:

~~~haskell
newtype PQueue a = PQ (SkewHeap a)

emptyPQ :: PQueue a
emptyPQ = PQ SEmpty

insertPQ :: Ord a => a -> PQueue a -> PQueue a
insertPQ x (PQ h) = PQ (mergeSH h (makeSH x))

popPQ :: Ord a => PQueue a -> (Maybe a, PQeueue a)
popPQ (PQ h) = (res, PQ h')
  where
    (res, h') = popSH h
~~~

We do this so that we hide our low-level skew heap implementation over a
"high-level" priority queue interface.  In this case, the high level isn't
much higher of a level, but it's good practice to hide away the implementation
details when you can in Haskell, a language whose power lies so much in
abstraction.

Building our Huffman encoding tree
----------------------------------

Now that we have what we need in place, let's get to 


