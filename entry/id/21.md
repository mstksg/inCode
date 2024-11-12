Streaming Huffman Compression in Haskell (Part 1: Trees and State)

===================================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on March 31, 2014.
> [Read online!](https://blog.jle.im/entry/streaming-huffman-compression-in-haskell-part-1-trees.html)

So you're learning Haskell and are looking for some projects that aren't super
trivial, are familiar enough that you can use what you already know, and are
difficult enough to maybe help you learn new things. Hey, maybe this is for you
:)

Let's take a go at [Huffman
encoding](http://en.wikipedia.org/wiki/Huffman_coding) in Haskell. We will look
at two types of binary trees, which we use to implement immutable/persistent
priority queues and prefix trees. We'll play around with the State monad a bit,
explore some useful typeclasses, learn how to serialize, marshal, and unmarshal
data structures using the [binary](http://hackage.haskell.org/package/binary)
library, and also look at how to load data from a file and write to another in a
pure way, avoiding lazy IO using the ever-more-popular
*[pipes](http://hackage.haskell.org/package/pipes)* library. And hopefully we
learn some neat Haskell idioms!

We're going to be assuming some basic Haskell knowledge, like algebraic data
types, higher order functions, basic monad usage, and some basic familiarity
with the functions in Prelude/base, the standard library. If you have any
questions, feel free to leave a comment, drop by on #haskell on freenode, throw
me a [tweet](https://twitter.com/mstk), or give the great [Learn You A
Haskell](http://learnyouahaskell.com) a quick read!

## Prefix trees & Huffman coding

You might have encountered this in the past, but Huffman encoding solves the
problem of finding the optimal binary prefix code to encode a string.

I'll leave you to read [the wikipedia
article](http://en.wikipedia.org/wiki/Huffman_coding), which explains it much
better than I could. Basically, binary prefix codes are nice because you don't
have to encode any "stop" symbol --- as soon as you reach a leaf of the tree,
you know that you have found a letter, and can move on.

Huffman trees are built from the bottom-up using priority queues. The two
lowest-frequency nodes are continually "popped" from the queue, combined into a
new node, and placed back in the queue.

Our first challenge --- representing a Huffman tree as a data structure!

### The Tree

(All the code in this section on is [available for
download](https://github.com/mstksg/inCode/tree/master/code-samples/huffman/PreTree.hs)
for you to try it out yourself!)

So some properties about prefix trees that might be useful to us --- all data is
stored in the leaves, and all internal nodes have exactly two children. This
sounds like the perfect candidate for an Algebraic Data Structure.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/PreTree.hs#L19-L21
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

data PreTree a = PTLeaf a
               | PTNode (PreTree a) (PreTree a)
               deriving (Show, Eq, Generic)
```

We leave the type parameterized on `a` (which is like a template/generic in
C++/Java) so we can decide what to put into it later.

#### `PreTree` operations

So, what sort of things are we going to want to do with our `PreTree`?

Well..first of all, we might want a way to put something into an empty tree ---
create a leaf containing just that data.

That function is sort of embarrassingly easy:

``` haskell
makePT' :: a -> PreTree a
makePT' x = PTLeaf x
```

Remember, that's `PTLeaf` is a data constructor that "creates" a `PreTree` when
you use `PTLeaf x`.

However, something like this is just begging to be eta-reduced, and we can
simplify it as:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/PreTree.hs#L46-L47
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

makePT :: a -> PreTree a
makePT = PTLeaf
```

Which does the same thing. Basically, `PTLeaf` is already a function
`a -> PreTree a`...so `makePT` is literally just `PTLeaf`.

``` haskell
ghci> let pt = makePT 'c'
ghci> :t pt
PreTree Char
ghci> pt
PTLeaf 'c'
```

Now, we might also want a way to "merge" two `PreTree a`'s. This is at the heart
of building the tree in the first place...successively merge two trees until
everything is in one giant tree.

This isn't too bad either:

``` haskell
mergePT' :: PreTree a -> PreTree a -> PreTree a
mergePT' t1 t2 = PTNode t1 t2
```

Which, from what we saw before, can just be written as:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/PreTree.hs#L50-L51
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

mergePT :: PreTree a -> PreTree a -> PreTree a
mergePT = PTNode
```

``` haskell
ghci> let pt1 = makePT 'c'
ghci> let pt2 = makePT 't'
ghci> let pt3 = pt1 `mergePT` pt2
ghci> :t pt3
PreTree Char
ghci> pt3
PTNode (PTLeaf 'c') (PTLeaf 't')
```

Hm. Maybe that's a bit too easy. Feels a little unsettling, isn't it?

Welcome to Haskell!

### Weighting things

We're going to need some way of comparing the weights/priorities of two
`PreTree`s when we are assembling the tree. Let's introduce a data type that
includes both a `PreTree` and an (integer) weight.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/Weighted.hs#L13-L15
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

data Weighted a = WPair { _wWeight :: Int
                        , _wItem   :: a
                        } deriving (Show, Functor)
```

(Code for the Weighted module is [available for
download](https://github.com/mstksg/inCode/tree/master/code-samples/huffman/Weighted.hs))

We will say that a `Weighted a` is some `a` associated with an integer weight.

We can create, say, a `PreTree` containing the character 'a', weighted with
integer 1:

``` haskell
ghci> WPair 1 (makePTLeaf 'a')
WPair 1 (makePTLeaf 'a') :: Weighted (PreTree Char)
```

This weighted `PreTree` is pretty useful, let's give it an alias/typedef:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/PreTree.hs#L54-L54
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

type WeightedPT a = Weighted (PreTree a)
```

Let's make the same functions for `WeightedPT` as we did for `PreTree`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/PreTree.hs#L58-L59
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

makeWPT :: Int -> a -> WeightedPT a
makeWPT w = WPair w . makePT
```

The above basically says "to make a `WeightedPT` with weight `w`, first `makePT`
it, and then add that result it to a `WPair w`.

``` haskell
ghci> let pt = makeWPT 1 'w'
ghci> :t pt
WeightedPT Char
ghci> pt
WPair 1 (PTLeaf 'w')
```

We will also want to merge two `WeightedPT`s:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/PreTree.hs#L62-L64
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

mergeWPT :: WeightedPT a -> WeightedPT a -> WeightedPT a
mergeWPT (WPair w1 pt1) (WPair w2 pt2)
    = WPair (w1 + w2) (mergePT pt1 pt2)
```

so that the total weight is the sum of the weights of the two subtrees.

Finally, the entire point of having weighted things is so that we can compare
them and impose some total ordering. Haskell has a typeclass that abstracts
these comparing operations, `Ord`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/Weighted.hs#L17-L21
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

instance Eq (Weighted a) where
    WPair w1 _ == WPair w2 _ = w1 == w2

instance Ord (Weighted a) where
    compare (WPair w1 _) (WPair w2 _) = compare w1 w2
```

Which says that `Weighted a` is an `Ord` (is orderable/comparable), and to
compare two `WPair w x`'s, you compare the `w`'s.

``` haskell
ghci> makeWPT 2 'a' > makeWPT 3 'b'
False
ghci> makeWPT 4 't' == makeWPT 4 'k'
True
```

## Priority Queues

There are some great priority queue libraries on Hackage, like
[PSQueue](http://hackage.haskell.org/package/PSQueue). However, for fun, we're
going to be making our own! Yay!

Our Priority Queue code module is [available for
download](https://github.com/mstksg/inCode/tree/master/code-samples/huffman/PQueue.hs)
to try out!

### Skew heaps

A traditional approach to making efficient priority queues is to use a
[heap](http://en.wikipedia.org/wiki/Heap_(data_structure)), a tree with
insertion algorithms that make sure the root of the tree is the most prioritized
element and that the tree stays balanced. Heaps make heavy use of stateful
mutation to do this, and while it's not so hard to do this in Haskell, we might
consider a 'pure' version of a heap: a [skew
heap](http://en.wikipedia.org/wiki/Skew_heap).

A skew heap is a heap that doesn't explicitly maintain its balance, but
maintains "heap ordering" (parents are always higher priority than their
children).

I'll leave it to the wikipedia article to do most of the explaining because they
have pretty pictures, but here is the gist of it --- skew heaps have only three
operations: making new (singleton) one, merging two skew heaps, and popping off
the root. Traditional "insert" is done by making a new skew heap with one
element, and merging it with the main heap.

Merging is simple enough: The higher-priority root becomes the new root, and the
lower-priority root is merged with the child tree of that new root. (Some
left-right flipping of branches is done to make sure things tend to stay
balanced. Pictures provided in the wikipedia article.)

Popping the root is simple too; just take the root, and merge its two sub-trees
to make the new tree.

This is a new type of binary tree, so let's define a new data type:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/PQueue.hs#L20-L22
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

data SkewHeap a = SEmpty
                | SNode a (SkewHeap a) (SkewHeap a)
                deriving (Show, Eq, Foldable)
```

Creating a new `SkewHeap` with one item:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/PQueue.hs#L25-L26
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

makeSH :: a -> SkewHeap a
makeSH x = SNode x SEmpty SEmpty
```

Popping the root off of a skew tree:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/PQueue.hs#L31-L33
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

popSH :: Ord a => SkewHeap a -> (Maybe a, SkewHeap a)
popSH SEmpty          = (Nothing, SEmpty)
popSH (SNode r h1 h2) = (Just r , mergeSH h1 h2)
```

We make it return a potential result (`Maybe a`), and the resulting new popped
tree. The result is `Maybe a` because we might potentially not be able to pop
anything! We also require an `Ord` constraint because in order to merge two skew
heaps, the data must be comparable.

Finally, the hardest piece of code so far: merging two skew heaps:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/PQueue.hs#L37-L42
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

mergeSH :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
mergeSH SEmpty h = h
mergeSH h SEmpty = h
mergeSH hA@(SNode xA lA rA) hB@(SNode xB lB rB)
    | xA < xB    = SNode xA (mergeSH rA hB) lA
    | otherwise  = SNode xB (mergeSH rB hA) lB
```

Hopefully this is very pleasing to read --- it reads a lot like a specification,
or a math formula:

1.  Merging any skew heap with an empty heap is that same skew heap.
2.  When merging two heaps, the new heap is an `SNode` with the smaller root,
    whose children are the merge of the smaller tree and the original children.
    (Admittedly, the math/code is a bit more expressive than English in this
    case)

(Remember that in our case, the *lower* value/weight is the *higher* priority.)

We require an `Ord` constraint because we compare the node element on the third
case.

### Priority Queue interface

Ok, neat!

Let's wrap this up in a tidy interface/API for a `PQueue` type:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/PQueue.hs#L48-L71
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

newtype PQueue a = PQ (SkewHeap a) deriving Show

emptyPQ :: PQueue a
emptyPQ = PQ SEmpty

insertPQ :: Ord a => a -> PQueue a -> PQueue a
insertPQ x (PQ h) = PQ (mergeSH h (makeSH x))

popPQ :: Ord a => PQueue a -> (Maybe a, PQueue a)
popPQ (PQ h) = (res, PQ h')
  where
    (res, h') = popSH h

sizePQ :: PQueue a -> Int
sizePQ (PQ h) = length (toList h)
```

(Notice `toList`, from the
[Foldable](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Foldable.html)
module; we derived `Foldable` so that we can use `toList` on our `SkewHeap`s. If
your Haskell implementation cannot derive foldable (if you are not using GHC,
for example) --- and even if your implementation can --- it might be fun to
think about how to implement `sizePQ` without it!)

We do this so that we hide our low-level skew heap implementation over a
"high-level" priority queue interface. We do not export the `PQ` constructor, so
users cannot ever directly access the underlying skew heap. In this case, the
high level isn't much higher of a level, but it's good practice to hide away the
implementation details when you can in Haskell, a language whose power lies so
much in abstraction.

## Building our Huffman encoding tree

Now that we have what we need in place, let's get to doing building our tree.
(Again, all available [for
download](https://github.com/mstksg/inCode/tree/master/code-samples/huffman/Huffman.hs).)

### Frequency Tables

First, we need to have some sort of frequency table. We will use
`Data.Map.Strict`'s `Map` type:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/Huffman.hs#L19-L19
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

type FreqTable a = Map a Int
```

and we'll import the operations from `Data.Map.Strict` qualified:

``` haskell
import qualified Data.Map.Strict as M
```

Just to work with things now, let's make a way to generate a `FreqTable` from an
arbitrary string:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/Huffman.hs#L22-L25
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

listFreq :: Ord a => [a] -> FreqTable a
listFreq = foldr f M.empty
  where
    f x m = M.insertWith (+) x 1 m
```

This says that `listFreq` is a fold, where you start with `M.empty` (an empty
`FreqTable`) and for every element, you insert it into the map as a key with
value 1. If the key already exists, add one to its current value instead.

``` haskell
ghci> listFreq "hello world"
fromList [(' ',1),('d',1),('e',1),('h',1),('l',3),('o',2),('r',1),('w',1)]
```

### Building the queue

Next, we would like to create Huffman leaves out of all of these elements, with
associated weights, and insert them all into a `PQueue`. We can do this by using
`M.foldrWithKey`, which is a `foldr` over the map, giving the folding function
both the key and the value.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/Huffman.hs#L43-L46
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

listQueue :: Ord a => [a] -> PQueue (Weighted a)
listQueue = M.foldrWithKey f emptyPQ . listFreq
  where
    f k v pq = insertPQ (WPair v k) pq
```

``` haskell
ghci> let pq = listQueue "hello world"
ghci> :t pq
pq :: PQueue (WPair Int (PreTree Char))
ghci> sizePQ pq
8
ghci> let (popped1, pq') = popPQ pq
ghci> popped1
Just (WPair 1 ' ')
ghci> let (popped2, pq'') = popPQ pq'
ghci> popped2
Just (WPair 1 'd')
ghci> sizePQ pq''
6
```

### Building the tree

Building the tree is going to be a bit harder than a simple fold over the queue,
because we have to "branch" based on the state of the queue. Depending on the
state of the queue, we make decisions on "control flow".

The experienced Haskelleur will recognize that this language is very evocative
of the Monad design pattern.

#### The State monad

In particular, we will be using the State monad, which is basically a plain ol'
newtype wrapper around functions `s -> (a, s)`. Basically, functions that act on
a state and return a value with a modified state.

These functions are actually surprisingly useful, and as it turns out, all
stateful computations can be described as "compositions" of these functions.

What do I mean by "compositions"?

Let's say I have two functions:

``` haskell
f1 :: s -> (a, s)
f2 :: s -> (b, s)
```

And I wanted to sequence them:

``` haskell
f1 `andThen` f2
```

What would that even "look like"?

Well, I expect that sequencing two state functions will return a new, "giant"
state function that does both functions "one after the other". That is:

``` haskell
f1 `andThen` f2 :: s -> (b, s)
```

This new function will first run the input state on `f1`, and take that
resulting state and pass it into `f2`, and then return the result of `f2` and
the resulting modified state of `f2`.

So we have something like

``` haskell
andThen :: (s -> (a,s)) -> (s -> (b,s)) -> (s -> (b,s))
andThen f1 f2 = \st -> let (_,st') = f1 s
                       in  f2 st'
```

Think of `andThen` like a semicolon, of sorts.

Notice that we "lose" the result of `f1` with `andThen`. What if we wanted to
use it? We might write a combinator:

    andThenWith ::       (s -> (a, s))
                -> (a -> (s -> (b, s)))
                ->       (s -> (b, s))

Which you would use like

``` haskell
f1 `andThenWith` (\x -> f2 x)
```

where `f2` is a function that takes an `a` and returns a `s -> (a,s)`.

Basically, it would be exactly the same as `andThen`, except the second argument
gets access to the result of the first. Writing it is almost as simple, actually
---

``` haskell
andThenWith :: (s -> (a,s)) -> (a -> (s -> (b, s))) -> (s -> (b, s))
andThenWith f1 f2 = \st -> let (x,st') = f1 s
                           in  (f2 x) st'
```

As it turns out...if you squint hard enough, the type signature `andThenWith`
looks a lot like the type signature for `(>>=)`:

``` haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

Hm. Let's create a type synonym for our `s -> (a, s)`, to make things more
clear.

``` haskell
type State s a = s -> (a, s)
```

So now our `andThenWith` looks like:

``` haskell
andThenWith :: State s a -> (a -> State s b) -> State s b
```

If we let `m ~ State s`:

``` haskell
andThenWith :: m a -> (a -> m b) -> m b
```

Neat!

As it turns out, we can turn our state functions into a Monad, which
encapsulates "sequencing" state functions one after another.

We just need `return`:

``` haskell
returnState :: a -> State s a
returnState x = \st -> (x, st)
```

And `return` is `returnState`, `(>>)` is `andThen`, and `(>>=)` is
`andThenWith`.

In real life, we can't define typeclass instances on type synonyms, so we
actually use a `newtype`. The standard implementation comes from the
[transformers](http://hackage.haskell.org/package/transformers) library. Because
`State s` is a member of the `Monad` typeclass, we can use normal monad
combinators, operators, and do notation. The transformers implementation comes
with a few useful primitives:

``` haskell
-- wrap a normal state function into the State wrapper
state :: (s -> (a, s)) -> State s a

-- get grabs the state as the result.
get :: State s s
get = state (\st -> (st, st))

-- put sets the state to the input
put :: s -> State s ()
put s = state (\_ -> ((), st))

-- modifies the state with the given function
modify :: (s -> s) -> State s ()
modify f = state (\st -> ((), f st))

-- alternative implementation of `modify`
modify' f :: (s -> s) -> State s ()
modify' f = do
    st <- get
    put (f st)
```

If you're still lost, check out Brandon Simmon's [state monad
tutorial](http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/),
which was the article that eventually cleared it all up for myself. And feel
free to ask questions!

The *big* usefulness for this "composing stateful functions" business, instead
of manually unwrapping and re-wrapping the state, is that now `State` actions
are first-class, and you can freely compose them and pass them around as
objects, and you can write individual "sub-routines", are little packets of
commands that modify state, and then "call them" and compose them from other
stateful computations.

#### Why monads?

One might pause to wonder why we would want to instance our `s -> (a, s)`
functions as a Monad. Why can't we just always sequence our state functions
using `andThen` and `andThenWith`?

1.  Using monads, we can now use `do` notation, which is pretty nice sugar.

2.  We now have access to the wide library of useful Haskell [monad
    combinators](http://hackage.haskell.org/package/base-4.6.0.1/docs/Control-Monad.html).
    And boy are there a lot --- `sequence`, `mapM`, `when`, `filterM`, etc.

3.  We also get an Applicative instance for free, so we can do arbitrary-arity
    lifting with things like `f <$> x <*> y`, where `f` is a pure function like
    `(+)` and `x` and `y` are stateful functions. We also get a free Functor
    instance as well, so we can `fmap`.

4.  We can now reason with our stateful functions with all of the powerful
    equational reasoning tools that the monad laws offer.

As you can see, monads are not just a curiosity --- they are a powerful and
expressive tool!

#### A quick look back

This is a bit of an unrelated aside...but notice that we could have actually
done our previous `fold`s as state monad operations; like `listFreq`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/Huffman.hs#L29-L36
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

runListFreq :: forall a. Ord a => [a] -> FreqTable a
runListFreq xs = execState listFreqState M.empty
  where
    listFreqState :: State (FreqTable a) ()
    listFreqState = mapM_ addFreq xs

    addFreq :: a -> State (FreqTable a) ()
    addFreq x = modify (M.insertWith (+) x 1)
```

`execState` runs the given `State` computation with the given initial state, and
returns the final state `s` at the end of it all. Basically, it takes an
`s -> (a, s)` (the `State s a`), an `s`, applies the function to it, and returns
just the `s` in the tuple.

Remember that the best way to read `State s a` is just "a type synonym for
`s -> (a,s)`". So when we say `listFreqState :: State (FreqTable a) ()`, we mean
that `listFreqState` is a function from a `FreqTable a` to `((), FreqTable a)`.

How about `listQueue`? We could do it with the state monad too, if we wanted to.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/Huffman.hs#L50-L59
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

listQueueState :: Ord a => [a] -> State (PQueue (WeightedPT a)) ()
listQueueState xs = M.traverseWithKey addNode (listFreq xs) >> return ()
  where
    addNode :: a -> Int -> State (PQueue (WeightedPT a)) ()
    addNode x i = modify (insertPQ (WPair i (makePT x)))

runListQueue :: Ord a => [a] -> PQueue (WeightedPT a)
runListQueue xs = execState (listQueueState xs) emptyPQ
```

In these cases, the monadic usage isn't quite necessary or useful on its own. A
fold would have probably been more expressive and easier to read. The above
examples were just for demonstrations/exercises.

But when do we "need" the state monad? (Or rather, when is a fold not powerful
enough or much messier?)

It's when we want to make decisions or "branch" based on the current state, or
the results of our state actions. "Fold for three items; if the next list item
is even then do this fold afterwards, otherwise do that fold". This is when the
state monad shines as a monad.

Another case where we might want to use a state monad over a fold is if we
forsee us wanting to "compose" our folds into bigger stateful computations. For
example, in `listQueueState`, we "process" a state, and leave it modified for
*another state monad action* to use.

For example:

``` haskell
prepareQueue :: State (PQueue (WeightedPT a)) ()
useQueue     :: State (PQueue (WeightedPT a)) a

doAllTogether :: Ord a => [a] -> State (PQueue (WeightedPT a)) a
doAllTogether xs = prepareQueue >> listQueueState xs >> useQueue

-- alternatively, the same thing but in do notation
doAllTogether' :: Ord a => [a] -> State (PQueue (WeightedPT a)) a
doAllTogether' xs = do
    prepareQueue
    listQueueState xs
    useQueue

runDoAllTogether :: Ord a => [a] -> a
runDoAllTogether xs = evalState (doAllTogether xs) emptyPQ
```

(Remember that `(>>)` is just our `andThen`, and when we sequence using `(>>)`
we mean "combine these two actions into one big action that feeds the resulting
state of the left side into the beginning state of the right side.")

Anyways, see that we can just plop a call to `listQueueState` inside a sequence
of stateful actions, and it'll just process the queue and leave it for the next
action to use.

If we had used `listQueue` as a "pure" fold...this is a bit harder to do. You'd
have to rewrite `listQueue` to take in any arbitrary "starting queue"...extract
the starting queue using `get` after `prepareQueue`, use a `let` to bind it as a
pure function, then use `put` to pop the result back into the state for
`useQueue` to use. Or use `modify` in a just-as-convoluted way.

Moving on, we actually won't be using `runListFreq` in the future (it was mostly
for fun), but (spoilers) we might want to hold onto `listQueueState` :)

### Building with State

So let's remember how the building process works:

1.  Pop an item from the queue.
2.  Pop another item. If the queue was actually empty, and nothing was poppable,
    you are done; return the result of step 1.
3.  Merge the two popped items, and push them back into the queue. Go back to
    step 1.

Sounds simple enough. We should take into account that we would fail to build a
tree if the queue was empty to begin with, by returning a `Maybe (PreTree a)`
instead of a `PreTree a`.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/huffman/Huffman.hs#L75-L98
-- interactive: https://www.fpcomplete.com/user/jle/huffman-encoding

buildTree :: State (PQueue (WeightedPT a)) (Maybe (PreTree a))
buildTree = do
    t1' <- state popPQ
    case t1' of
      Nothing ->
        -- queue was empty to begin with, so this fails.
        return Nothing
      Just t1 -> do
        t2' <- state popPQ
        case t2' of
          Nothing  ->
            -- We're done, there was only one item!  Return a `Just` to
            -- indicate success.
            return (Just (_wItem t1))     -- break out of the loop
          Just t2 -> do
            -- merge and push
            let combined = mergeWPT t1 t2
            modify (insertPQ combined)
            buildTree                     -- recursive call

runBuildTree :: Ord a => [a] -> (Maybe (PreTree a))
runBuildTree xs = evalState (listQueueState xs >> buildTree) emptyPQ
```

Note that due to our uncanny foresight,
`popPQ :: PQueue a -> (Maybe a, PQueue a)` is already a state function
`s -> (a, s)`, where the state is `PSQueue a` and the return value is `Maybe a`.
So all we need to do is say `state popPQ` to wrap it in the `State s a` newtype
wrapper/container, and it becomes an "official" `State (PQueue a) (Maybe a)`.

Remember that `State s a` is *just a thin wrapper/container* over a function
`s -> (a, s)`, anyway, so the two should be somewhat equivalent in your mind;
the requirement to wrap it in `State` using `state` is only because of Haskell's
own language limitations (namely, that you can't define a Monad instance for
`s -> (a, s)` in a clean way). When you read `State s a`, you *should really
read* `s -> (a, s)`, because they are for the most part *completely equivalent*.

Again, `(>>)` is Monad-speak for our `andThen` function we defined earlier, so
for `buildTree`, we do "`listQueueState xs` *and then* `buildTree`". `(>>)`
joins two `s -> (a, s)` functions into one giant `s -> (a,s)`, by feeding the
resulting state of the first action into the next one. `listQueueState` takes an
empty priority queue and 'fills' it with nodes generated from `xs`, leaving a
filled priority queue. `buildTree` then takes that filled queue and performs our
building operations on it, modifying it as it goes along, and ends up with an
empty queue as a state and returning the finished tree as a result.

`evalState` is like partner of `execState` --- it runs the state operation on
the given starting state, and outputs the final result (instead of the final
state). It takes an `s -> (a, s)`, an `s`, and applies the function to it and
gives the resulting `a` of the tuple.

### Putting it all together

Let's try it out, shall we?

``` haskell
ghci> fromJust $ runBuildTree "hello world"
PTNode (PTNode (PTNode (PTLeaf 'h')
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
```

Congrats, we built a Huffman encoding tree! Notice that the most commonly used
letter (`'l'`, occurring 3 times) is only at depth 2 (and is most accessible),
while the others are at depths 3 and 4.

## Next steps

That's it for this post, it's already long enough!

In the next posts we will look at how we would use this Huffman tree to encode
and decode text, and general bytes (`Word8`s), and then hook it all up to make a
"streaming" compressor and uncompressor that reads a file byte-by-byte and
outputs a compressed file as it goes. We'll then figure out how to store this
huffman tree in a compact, serialized binary way, and load it cleanly.

In the mean time, try [downloading the
source](https://github.com/mstksg/inCode/tree/master/code-samples/huffman), or
[playing with it online](https://www.fpcomplete.com/user/jle/huffman-encoding)
on [fpcomplete](http://www.fpcomplete.com)!

--------------------------------------------------------------------------------

Hi, thanks for reading! You can reach me via email at <justin@jle.im>, or at
twitter at [\@mstk](https://twitter.com/mstk)! This post and all others are
published under the [CC-BY-NC-ND
3.0](https://creativecommons.org/licenses/by-nc-nd/3.0/) license. Corrections
and edits via pull request are welcome and encouraged at [the source
repository](https://github.com/mstksg/inCode).

If you feel inclined, or this post was particularly helpful for you, why not
consider [supporting me on Patreon](https://www.patreon.com/justinle/overview),
or a [BTC donation](bitcoin:3D7rmAYgbDnp4gp4rf22THsGt74fNucPDU)? :)

