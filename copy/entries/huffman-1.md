Streaming Huffman Compression in Haskell (Part 1)
=================================================

Categories
:   Haskell
:   Tutorials
Tags
:   haskell
:   monads
CreateTime
:   2014/03/19 01:25:50
PostDate
:   2014/03/31 10:13:44
Series
:   Huffman Compression
:   Beginner/Intermediate Haskell Projects
Identifier
:   huffman-1

So you're learning Haskell and are looking for some projects that aren't super
trivial, are familiar enough that you can use what you already know, and are
difficult enough to maybe help you learn new things.  Hey, maybe this is for
you :)

Let's take a go at [Huffman encoding][] in Haskell.  We will look at two types
of binary trees, which we use to implement immutable/persistent priority
queues and prefix trees.  We'll play around with the State monad a bit,
explore some useful typeclasses, learn how to serialize, marshal, and
unmarshal data structures, and also look at how to load data from a file and
write to another in a pure way, avoiding lazy IO using the ever-more-popular
*[pipes][]* library.  And hopefully we learn some neat Haskell idioms!

[Huffman encoding]: http://en.wikipedia.org/wiki/Huffman_coding
[pipes]: http://hackage.haskell.org/package/pipes

We're going to be assuming some basic Haskell knowledge, like algebraic data
types, higher order functions, basic monad usage, and some basic familiarity
with the functions in Prelude/base, the standard library.  If you have any
questions, feel free to leave a comment, drop by on #haskell on freenode,
throw me a [tweet][twitter], or give the great [Learn You A Haskell][lyah] a
quick read!

[twitter]: https://twitter.com/mstk
[lyah]: http://learnyouahaskell.com

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

(All the code in this section on is [available for download][sourcept] for
you to try it out yourself!)

!!![sourcept]:huffman/PreTree.hs

So some properties about prefix trees that might be useful to us --- all data
is stored in the leaves, and all internal nodes have exactly two children.
This sounds like the perfect candidate for an Algebraic Data Structure.

~~~haskell
!!!huffman/PreTree.hs "data PreTree" huffman-encoding
~~~

We leave the type parameterized on `a` (which is like a template/generic in
C++/Java) so we can decide what to put into it later.

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
!!!huffman/PreTree.hs "makePT ::" huffman-encoding
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
mergePT' t1 t2 = PTNode t1 t2
~~~

Which, from what we saw before, can just be written as:

~~~haskell
!!!huffman/PreTree.hs "mergePT ::" huffman-encoding
~~~

~~~haskell
λ: let pt1 = makePT 'c'
λ: let pt2 = makePT 't'
λ: let pt3 = pt1 `mergePT` pt2
λ: :t pt3
PreTree Char
λ: pt3
PTNode (PTLeaf 'c') (PTLeaf 't')
~~~

Hm.  Maybe that's a bit too easy.  Feels a little unsettling, isn't it?

Welcome to Haskell!

### Weighting things

We're going to need some way of comparing the weights/priorities of two
`PreTree`s when we are assembling the tree.  Let's introduce a data type that
includes both a `PreTree` and an (integer) weight.

~~~haskell
!!!huffman/Weighted.hs "data Weighted" huffman-encoding
~~~

(Code for the Weighted module is [available for download][sourcew])

!!![sourcew]:huffman/Weighted.hs


We will say that a `Weighted a` is some `a` associated with an integer weight.

We can create, say, a `PreTree` containing the character 'a', weighted with
integer 1:

~~~haskell
λ: WPair 1 (makePTLeaf 'a')
WPair 1 (makePTLeaf 'a') :: Weighted (PreTree Char)
~~~

This weighted `PreTree` is pretty useful, let's give it an alias/typedef:

~~~haskell
!!!huffman/PreTree.hs "type WPreTree" huffman-encoding
~~~

Let's make the same functions for `WPreTree` as we did for `PreTree`:

~~~haskell
!!!huffman/PreTree.hs "makeWPT ::" huffman-encoding
~~~

The above basically says "to make a `WPreTree` with weight `w`, first
`makePT` it, and then add that result it to a `WPair w`.

~~~haskell
λ: let pt = makeWPT 1 'w'
λ: :t pt
WPreTree Char
λ: pt
WPair 1 (PLeaf 'w')
~~~

We will also want to merge two `WPreTree`s:

~~~haskell
!!!huffman/PreTree.hs "mergeWPT ::" huffman-encoding
~~~

so that the total weight is the sum of the weights of the two subtrees.

Finally, the entire point of having weighted things is so that we can compare
them and impose some total ordering.  Haskell has a typeclass that abstracts
these comparing operations, `Ord`:

~~~haskell
!!!huffman/Weighted.hs "instance Eq (Weighted a)" "instance Ord (Weighted a)" huffman-encoding
~~~

Which says that `Weighted a` is an `Ord` (is orderable/comparable), and to
compare two `WPair w x`'s, you compare the `w`'s.

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

Our Priority Queue code module is [available for download][sourcepq] to try
out!

!!![sourcepq]:huffman/PQueue.hs

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

Merging is simple enough: The higher-priority root becomes the new
root, and the lower-priority root is merged with the child tree of that new
root.  (Some left-right flipping of branches is done to make sure things tend
to stay balanced.  Pictures provided in the wikipedia article.)

Popping the root is simple too; just take the root, and merge its two
sub-trees to make the new tree.

This is a new type of binary tree, so let's define a new data type:

~~~haskell
!!!huffman/PQueue.hs "data SkewHeap" huffman-encoding
~~~

Creating a new `SkewHeap` with one item:

~~~haskell
!!!huffman/PQueue.hs "makeSH ::" huffman-encoding
~~~

Popping the root off of a skew tree:

~~~haskell
!!!huffman/PQueue.hs "popSH ::" huffman-encoding
~~~

We make it return a potential result (`Maybe a`), and the resulting new popped
tree.  The result is `Maybe a` because we might potentially not be able to pop
anything!  We also require an `Ord` constraint because in order to merge two
skew heaps, the data must be comparable.

Finally, the hardest piece of code so far: merging two skew heaps:

~~~haskell
!!!huffman/PQueue.hs "mergeSH ::" huffman-encoding
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
!!!huffman/PQueue.hs "newtype PQueue" "emptyPQ ::" "insertPQ ::" "popPQ ::" "sizePQ ::" huffman-encoding
~~~

We do this so that we hide our low-level skew heap implementation over a
"high-level" priority queue interface.  We do not export the `PQ` constructor,
so users cannot ever directly access the underlying skew heap.  In this case,
the high level isn't much higher of a level, but it's good practice to hide
away the implementation details when you can in Haskell, a language whose
power lies so much in abstraction.

Building our Huffman encoding tree
----------------------------------

Now that we have what we need in place, let's get to doing building our tree.
(Again, all available [for download][sourcehuff].)

!!![sourcehuff]:huffman/Huffman.hs

### Frequency Tables

First, we need to have some sort of frequency table.  We will use
`Data.Map.Strict`'s `Map` type:

~~~haskell
!!!huffman/Huffman.hs "type FreqTable" huffman-encoding
~~~

and we'll import the operations from `Data.Map.Strict` qualified:

~~~haskell
import qualified Data.Map.Strict as M
~~~

Just to work with things now, let's make a way to generate a `FreqTable` from
an arbitrary string:

~~~haskell
!!!huffman/Huffman.hs "listFreq ::" huffman-encoding
~~~

This says that `listFreq` is a fold, where you start with `M.empty` (an empty
`FreqTable`) and for every element, you insert it into the map as a key with
value 1.  If the key already exists, add one to its current value instead.

~~~haskell
λ: listFreq "hello world"
fromList [(' ',1),('d',1),('e',1),('h',1),('l',3),('o',2),('r',1),('w',1)]
~~~

### Building the queue

Next, we would like to create Huffman leaves out of all of these elements,
with associated weights, and insert them all into a `PQueue`.  We can do this
by using `M.foldrWithKey`, which is a `foldr` over the map, giving the folding
function both the key and the value.

~~~haskell
!!!huffman/Huffman.hs "listQueue ::" huffman-encoding
~~~

~~~haskell
λ: let pq = listQueue "hello world"
λ: :t pq
pq :: PQueue (WPair Int (PreTree Char))
λ: sizePQ pq
8
λ: let (popped1, pq') = popPQ pq
λ: popped1
Just (WPair 1 ' ')
λ: let (popped2, pq'') = popPQ pq'
λ: popped2
Just (WPair 1 'd')
λ: sizePQ pq''
6
~~~

### Building the tree

Building the tree is going to be a bit harder than a simple fold over the
queue, because we have to "branch" based on the state of the queue.  Depending
on the state of the queue, we make decisions on "control flow".

The experienced Haskelleur will recognize that this language is very evocative
of the Monad design pattern.

#### The State monad

In particular, we will be using the State monad, which is basically a
plain ol' newtype wrapper around functions `s -> (a, s)`.  Basically,
functions that act on a state and return a value with a modified state.

These functions are actually surprisingly useful, and as it turns out, all
stateful computations can be described as "compositions" of these functions.

What do I mean by "compositions"?

Let's say I have two functions:

~~~haskell
f1 :: s -> (a, s)
f2 :: s -> (b, s)
~~~

And I wanted to sequence them:

~~~haskell
f1 `andThen` f2
~~~

What would that even "look like"?

Well, I expect that sequencing two state functions will return a new, "giant"
state function that does both functions "one after the other".  That is:

~~~haskell
f1 `andThen` f2 :: s -> (b, s)
~~~

This new function will first run the input state on `f1`, and take that
resulting state and pass it into `f2`, and then return the result of `f2` and
the resulting modified state of `f2`.

So we have something like

~~~haskell
andThen :: (s -> (a,s)) -> (s -> (b,s)) -> (s -> (b,s))
andThen f1 f2 = \s -> let (_,s') = f1 s
                      in  f2 s'
~~~

Think of `andThen` like a semicolon, of sorts.

Notice that we "lose" the result of `f1` with `andThen`.  What if we wanted to
use it?  We might write a combinator:

~~~
andThenWith ::       (s -> (a, s))
            -> (a -> (s -> (b, s)))
            ->       (s -> (b, s))
~~~

Which you would use like

~~~haskell
f1 `andThenWith` (\x -> f2 x)
~~~

where `f2` is a function that takes an `a` and returns a `s -> (a,s)`.

Basically, it would be exactly the same as `andThen`, except the second
argument gets access to the result of the first.  Writing it is almost as
simple, actually ---

~~~haskell
andThenWith :: (s -> (a,s)) -> (a -> (s -> (b, s))) -> (s -> (b, s))
andThenWith f1 f2 = \s -> let (x,s') = f1 s
                          in  (f2 x) s'
~~~

As it turns out...if you squint hard enough, the type signature `andThenWith`
looks a lot like the type signature for `(>>=)`:

~~~haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
~~~

Hm.  Let's create a type synonym for our `s -> (a, s)`, to make things more
clear.

~~~haskell
type State s a = s -> (a, s)
~~~

So now our `andThenWith` looks like:

~~~haskell
andThenWith :: State s a -> (a -> State s b) -> State s b
~~~

If we let `m ~ State s`:

~~~haskell
andThenWith :: m a -> (a -> m b) -> m b
~~~

Neat!

As it turns out, we can turn our state functions into a Monad, which
encapsulates "sequencing" state functions one after another.

We just need `return`:

~~~haskell
returnState :: a -> State s a
returnState x = \s -> (x, s)
~~~

And `return` is `returnState`, `(>>)` is `andThen`, and `(>>=)` is
`andThenWith`.

In real life, we can't define typeclass instances on type synonyms,
so we actually use a `newtype`.  The standard implementation comes from the
[transformers][] library.  Because `State s` is a member of the `Monad`
typeclass, we can use normal monad combinators, operators, and do notation.
The transformers implementation comes with a few useful primitives:

[transformers]: http://hackage.haskell.org/package/transformers

~~~haskell
-- wrap a normal state function into the State wrapper
state :: (s -> (a, s)) -> State s a

-- get grabs the state as the result.
get :: State s s
get = state (\s -> (s,s))

-- put sets the state to the input
put :: s -> State s ()
put s = state (\_ -> ((), s))

-- modifies the state with the given function
modify :: (s -> s) -> State s ()
modify f = state (\s -> ((), f s))

-- alternative implementation of `modify`
modify' f :: (s -> s) -> State s ()
modify' f = do
    s <- get
    put (f s)
~~~

If you're still lost, check out Brandon Simmon's [state monad
tutorial][statetut], which was the article that eventually cleared it all up
for myself.  And feel free to ask questions!

[statetut]: http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/

The *big* usefulness for this "composing stateful functions" business, instead
of manually unwrapping and re-wrapping the state, is that now `State` actions
are first-class, and you can freely compose them and pass them around as
objects, and you can write individual "sub-routines", are little packets of
commands that modify state, and then "call them" and compose them from other
stateful computations.

#### Why monads?

One might pause to wonder why we would want to instance our `s -> (a, s)`
functions as a Monad.  Why can't we just always sequence our state functions
using `andThen` and `andThenWith`?

1.  Using monads, we can now use `do` notation, which is pretty nice sugar.

2.  We now have access to the wide library of useful Haskell [monad
    combinators][controlmonad].  And boy are there a lot --- `sequence`,
    `mapM`, `when`, `filterM`, etc.

3.  We also get an Applicative instance for free, so we can do arbitrary-arity
    lifting with things like `f <$> x <*> y`, where `f` is a pure function
    like `(+)` and `x` and `y` are stateful functions.  We also get a free
    Functor instance as well, so we can `fmap`.

4.  We can now reason with our stateful functions with all of the powerful
    equational reasoning tools that the monad laws offer.

[controlmonad]: http://hackage.haskell.org/package/base-4.6.0.1/docs/Control-Monad.html

As you can see, monads are not just a curiosity --- they are a powerful and
expressive tool!

#### A quick look back

Now note that we could have actually done our previous `fold`s as `State`
monad operations:

~~~haskell
!!!huffman/Huffman.hs "runListFreq ::" huffman-encoding
~~~

`execState` runs the given `State` computation with the given initial state,
and returns the final state `s` at the end of it all.

Remember that the best way to read `State s a` is just "a type synonym for
`s -> (a,s)`".  So when we say `listFreqState :: State (FreqTable a) ()`, we
mean that `listFreqState` is a function from a `FreqTable a` to `((),
FreqTable a)`.

~~~haskell
!!!huffman/Huffman.hs "listQueueState ::" "runListQueue ::" huffman-encoding
~~~

Note that in these cases, the monadic usage isn't actually necessary --- nor
is it very useful (a fold would be just as expressive and probably easier to
read).  However, what if we wanted to make decisions and branch based on the
current state?  That's when the State monad shines as a monad.

(We're not going to use `listFreq'` and `listQueue'`...but let's hang onto
`listQueueState` for a bit.)


### Building with State

So let's remember how the building process works:

1.  Pop an item from the queue.
2.  Pop another item.  If the queue was actually empty, and nothing was
    poppable, you are done; return the result of step 1.
3.  Merge the two popped items, and push them back into the queue.  Go back to
    step 1.

Sounds simple enough.  We should take into account that we would fail to build
a tree if the queue was empty to begin with, by returning a `Maybe (PreTree
a)` instead of a `PreTree a`.

~~~haskell
!!!huffman/Huffman.hs "buildTree ::" "runBuildTree ::" huffman-encoding
~~~

Note that due to our uncanny foresight, `popPQ :: PQueue a -> (Maybe a, PQueue
a)` is already a state function `s -> (a, s)`, where the state is `PSQueue a`
and the return value is `Maybe a`.  So all we need to do is say `state popPQ`
to wrap it in the `State s a` newtype wrapper/container, and it becomes an
"official" `State (PQueue a) (Maybe a)`.

Remember that `State s a` is *just a thin wrapper/container* over a function
`s -> (a, s)`, anyway, so the two should be somewhat equivalent in your mind;
the requirement to wrap it in `State` using `state` is only because of
Haskell's own language limitations (namely, that you can't define a Monad
instance for `s -> (a, s)` in a clean way).  When you read `State s a`, you
*should really read* `s -> (a, s)`, because they are for the most part
*completely equivalent*.

Remember also that `(>>)` is Monad-speak for our `andThen` function we
defined earlier, so for `buildTree`, we do "`listQueueState xs` *and then*
`buildTree`".  `(>>)` joins two `s -> (a, s)` functions into one giant `s
-> (a,s)`, by feeding the resulting state of the first action into the next
one.  `listQueueState` takes an empty priority queue and 'fills' it with nodes
generated from `xs`, leaving a filled priority queue.  `buildTree` then
takes that filled queue and performs our building operations on it, modifying
it as it goes along, and ends up with an empty queue as a state and returning
the finished tree as a result.

`evalState` is like the opposite of `execState` --- it runs the state
operation on the given starting state, and outputs the final result (instead
of the final state).

~~~haskell
λ: fromJust $ runBuildTree "hello world"
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
~~~

Congrats, we built a Huffman encoding tree!  Notice that the most commonly
used letter (`'l'`, occurring 3 times) is only at depth 2 (and is most
accessible), while the others are at depths 3 and 4.

Next steps
----------

That's it for this post, it's already long enough!

In the next posts we will look at how we would use this Huffman tree to encode
and decode text, and general bytes (`Word8`s), and then hook it all up to make
a "streaming" compressor and uncompressor that reads a file byte-by-byte and
outputs a compressed file as it goes.  We'll then figure out how to store this
huffman tree in a compact, serialized binary way, and load it cleanly.

In the mean time, try [downloading the source][sourceall], or [playing with it
online][sourcefp] on [fpcomplete]!

[sourceall]: https://github.com/mstksg/inCode/tree/master/code-samples/huffman
[sourcefp]: https://www.fpcomplete.com/user/jle/huffman-encoding
[fpcomplete]: http://www.fpcomplete.com
