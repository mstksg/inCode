---
title: Tries with Recursion Schemes
categories: Haskell, Tutorials
tags: haskell, recursion schemes
create-time: 2018/11/29 17:45:04
series: Beginner/Intermediate Haskell Projects
identifier: trie
slug: tries-with-recursion-schemes
---

Not too long ago, I was browsing the [prequel memes subreddit][r/prequelmemes]
--- a community built around creative ways of remixing and re-contextualizing
quotes from the cinematic corpus of the three Star Wars prequel movies --- when
I noticed that a fad was in progress [constructing tries based on quotes as
keys][meme] indexing stills from the movie corresponding to those quotes.

[r/prequelmemes]: https://www.reddit.com/r/PrequelMemes
[meme]: https://www.reddit.com/r/PrequelMemes/comments/9w59t4/i_expanded_it/

This inspired me to try playing around with some tries myself, and it gave me
an excuse to play around with *[recursion-schemes][]* (one of my favorite
Haskell libraries).  If you haven't heard about it yet, *recursion-schemes*
(and the similar library *[data-fix][]*) abstracts over common recursive
functions written on recursive data types.  It exploits the fact that a lot of
recursive functions for different recursive data types all really follow the
same pattern and gives us powerful tools for writing cleaner and safer code.

[recursion-schemes]: https://hackage.haskell.org/package/recursion-schemes
[data-fix]: https://hackage.haskell.org/package/data-fix

Recursion schemes is a perfect example of those amazing accidents that happen
throughout the Haskell ecosystem: an extremely "theoretically beautiful"
abstraction that also happens to be extremely useful for writing industrially
rigorous code.

Tries are a common intermediate-level recursive data type, and
recursion-schemes is a common intermediate-level library.  So, as a fun
intermediate-level Haskell project, let's build a trie data type in Haskell
based on recursion-schemes, to see what it has to offer!  The resulting data
type will definitely not be a "toy" --- it'll be something you can actually use
to build meme diagrams of your own!

Trie
----

A [trie][] (prefix tree) is a classic example of a simple yet powerful data
type most people encounter in school (I remember being introduced to it through
a project implementing a boggle solver).

[trie]: https://en.wikipedia.org/wiki/Trie

Wikipedia has a nice picture:

![Sample Trie from Wikipedia, indexing lists of Char to Ints](/img/entries/trie/wiki-trie.png "An example Trie")

API-wise, it is very similar to an *associative map*, like the `Map` type from
*[containers][Data.Map]*.  It stores "keys" to "values", and you can insert a
value at a given key, lookup the value stored at a given key, or delete the value
at a given key.

[Data.Map]: https://hackage.haskell.org/package/containers/docs/Data-Map-Lazy.html

The main difference is in implementation: the keys are *strings of tokens*, and
it is internally represented as a tree: if your keys are words, then the first
level is the first letter, the second level is the letter, etc.  In the example
above, the trie stores the keys `to`, `tea`, `ted`, `ten`, `A`, `i`, `in`, and
`inn`, to the values 7, 3, 4, 12, 15, 11, 5, and 9, respectively.  Note that it
is possible for one key to completely overlap another (like `in` storing 5, and
`inn` storing 9).  In the usual case, however, we have partial overlaps (like
`tea`, storing 3, and `ted` storing 4), whose common prefix (`te`) has no
value stored under it.

Haskell Tries
-------------

We can represent this in Haskell by representing each layer as a `Map` of a token
to the next "level" of the trie:

```haskell
!!!trie/trie.hs "data Trie k v"
```

A `Trie k v` will have keys of type `[k]`, where `k` is the key token type, and
values of type `v`.  Each layer might have a value (`Maybe v`), and branches
out to each new layer.

We could write the trie storing `(to, 9)`, `(ton, 3)`, and `(tax, 2)` as:

```haskell
!!!trie/trie.hs "testTrie ::"
```

Note that this implementation isn't particularly structurally sound, since it's
possible to represent invalid keys that have branches that lead to nothing.
This mostly becomes troublesome when we implement `delete`, but we won't be
worrying about that for now.  The nice thing about Haskell is that we can be as
safe as we want or need, as a judgement call on a case-by-case basis.  However,
a "correct-by-construction" trie is in the next part of this series :)

### Enter Recursion Schemes

Now, `Trie` as we write it up there is an explicitly recursive data type.
While this is common practice, it's not a particularly ideal one.  The problem
with explicitly recursive data types is that to work with them, you often rely
on explicitly recursive functions.

Explicitly recursive functions are notoriously difficult to write, understand,
and maintain.  It's extremely easy to accidentally write an infinite loop, and
is often called "the GOTO of functional programming".

So, there's a trick we can use to "factor out" the recursion in our data type.
The trick is to replace the recursive occurrence of `Trie a` (in the `Cons`
constructor) with a "placeholder" variable:

```haskell
!!!trie/trie.hs "data TrieF k v"
```

`TrieF` now represents, essentially, "one layer" of a `Trie`.

There are now two paths we can go down: we can re-implement `Trie` in terms of
`TrieF` (something that most tutorials and introductions do), or we can think
of `TrieF` as a "non-recursive view" into `Trie`.  It's a way of *working* with
`Trie a` *as if* it were a non-recursive data type.

That's because *recursion-schemes* gives combinators (called "recursion
schemes") to abstract over common explicit recursion patterns.  The key to
using *recursion-schemes* is to recognize which combinators abstracts over the
type of recursion you're using. You then give that combinator an algebra or a
coalgebra (more on this later), and you're done!

Learning how to use *recursion-schemes* effectively is basically picking the
right recursion scheme that abstracts over the type of function you want to
write for your data type.  It's all about becoming familiar with the "zoo" of
(colorfully named) recursion schemes you can pick from, and identifying which
one does the job in your situation.

That's the high-level view --- let's dive into writing out the API of our
`Trie`!

### Linking the base

One thing we need to do before we can start: we need to tell
*recursion-schemes* to link `TrieF` with `Trie`.  In the nomenclature of
*recursion-schemes*, `TrieF` is known as the "base type", and `Trie` is called
"the fixed-point".

Linking them requires some boilerplate, which is basically converting back and
forth from `Trie` to `TrieF`.

```haskell
!!!trie/trie.hs "type instance Base" "instance Recursive (Trie k v)" "instance Corecursive (Trie k v)"
```

Basically we just link the constructors and fields of `MkT` and `MkTF`
together.

As with all boilerplate, it is sometimes useful to clean it up a bit using
Template Haskell.  The *recursion-schemes* library offers such splice:

```haskell
data Trie k v = MkT (Maybe v) (Map   k (Trie k v))
  deriving Show

makeBaseFunctor ''Trie
```

This will define `TrieF`, the `Base` type family instance, and the `Recursive`
and `Corecursive` instances (in possibly a more efficient way than the way we
wrote by hand, too).

## Exploring the Zoo

Time to explore the zoo a bit!

Whenever you get a new recursive type and base functor, a good "first thing" to
try out is testing out `cata` and `ana` (catamorphisms and anamorphisms), the
basic "folder" and "unfolder".

### Hakuna My Cata

Catamorphisms are functions that "combine" or "fold" every layer of our
recursive type into a single value.  If we want to write a function of type
`Trie k v -> A`, we can reach first for a catamorphism.

Catamorphisms work by folding layer-by-layer, from the bottom up.  We can write
one by defining "what to do with each layer".  This description comes in the
form of an "algebra" in terms of the base functor:

```haskell
myAlg :: TrieF k v A -> A
```

If we think of `TrieF k v a` as "one layer" of a `Trie k v`, then `TrieF k v A
-> A` describes how to fold up one layer of our `Trie k v` into our final
result value (here, of type `A`).  Remember that a `TrieF k v A` contains a
`Maybe v` and a `Map k A`.  The `A` (the values of the map) contains the result
of folding up all of the original subtries along each key; it's the "results so
far".

And then we can use `cata` to "fold" our value along the algebra:

```haskell
cata myAlg :: Trie k v -> A
```

`cata` starts from the bottom-most layer, runs `myAlg` on that, then goes up a
layer, running `myAlg` on the results, then goes up another layer, running
`myAlg` on those results, etc., until it reaches the top layer and runs `myAlg`
again to produce the final result.

For example, we'll write a catamorphism that counts how many values/leaves we have in
our Trie into an `Int`.

```haskell
countAlg :: TrieF k v Int -> Int
```

This is the basic structure of an algebra: our final result becomes the
parameter of `TrieF k v`, and also the result of our algebra.

Remember that a `Trie k v` contains a `Maybe v` and a `Map k (Trie k v)`, and a
`TrieF k v Int` contains a `Maybe v` and a `Map k Int`.  The `Map`, then,
represents the number of values inside all of the original subtries along each
key.

Basically, our task is "How to find a count, given a map of sub-counts".

With this in mind, we can write `countAlg`:

```haskell
!!!trie/trie.hs "countAlg"
```

If `v` is indeed a leaf (it's `Just`), then it's one plus the total counts of
all of the subtees (remember, the `Map k Int` contains the counts of all of the
original subtries, under each key).  Otherwise, it's just the total counts of
all of the original subtries.

Our final `count` is therefore just:

```haskell
!!!trie/trie.hs "count"
```

```haskell
ghci> count testTrie
3
```

We can do something similar by writing a summer, as well:

```haskell
!!!trie/trie.hs "trieSumAlg" "trieSum"
```

```haskell
ghci> trieSum testTrie
14
```

In the algebra, the `Map k a` contains the sum of all of the subtries.  The
algebra therefore just adds up all of the subtrie sums with the value at that
layer. "How to find a sum, given a map of sub-sums".

#### Outside-In

Catamorphisms are naturally "inside-out", or "bottom-up".  However, some
operations are more naturally "outside-in", or "top-down".  One immediate
example is `lookup :: [k] -> Trie k v -> Maybe v`, which is clearly "top-down":
it first descends down the first item in the `[k]`, then the second, then the
third, etc. until you reach the end, and return the `Maybe v` at that layer.

In this case, it helps to invert control: instead of folding into a `Maybe v`
directly, fold into a "looker upper", a `[k] -> Maybe v`.  We generate a
"lookup function" from the bottom-up, and then run that all in the end on the
key we want to look up.

Our algebra will therefore have type:

```haskell
lookupperAlg
    :: Ord k
    => TrieF k v ([k] -> Maybe v)
    -> ([k] -> Maybe v)
```

A `TrieF k v ([k] -> Maybe v)` contains a `Maybe v` and a `Map k ([k] -> Maybe
v)`, or a map of "lookuppers".  Indexed at each key is function of how to look
up a given key in the original subtrie.

So, we are tasked with "how to implement a lookupper, given a map of
sub-lookuppers".

To do this, we can pattern match on the key we are looking up.  If it's `[]`,
then we just return the current leaf (if it exists).  Otherwise, if it's
`k:ks`, we can *run the lookupper of the subtrie at key `k`*.

```haskell
!!!trie/trie.hs "lookupperAlg" "lookup"
```

(Written using the -XLambdaCase extension, allowing for `\case` syntax)

```haskell
ghci> lookup "to" testTrie
Just 9
ghci> lookup "ton" testTrie
Just 3
ghci> lookup "tone" testTrie
Nothing
```

Note that because `Map`s have lazy keys by default, we only ever generate
"lookuppers" for subtries under keys that we eventually descend on; any other
subtries will be ignored (and no lookuppers are ever generated for them).

In the end, this version has all of the same performance characteristics as the
explicitly recursive one; we're assembling a "lookupper" that stops as soon as
it sees either a failed lookup (so it doesn't cause any more evaluation later
on), or stops when it reaches the end of its list.

#### I Think the System Works

We've now written a couple of non-recursive functions to "query" `Trie`.  But
what was the point, again?  What do we gain over writing explicit versions to
query Trie?  Why couldn't we just write:

```haskell
!!!trie/trie.hs "trieSumExplicit"
```

instead of

```haskell
!!!trie/trie.hs "trieSumCata"
```


One major reason, like I mentioned before, is to avoid using *explicit
recursion*.  It's extremely easy when using explicit recursion to accidentally
write an infinite loop, or to mess up your control flow somehow.  It's
basically like using `GOTO` instead of `while` or `for` loops in imperative
languages.  `while` and `for` loops are meant to abstract over a common type of
looping control flow, and provide a disciplined structure for them.  They also
are often much easier to read, because as soon as you see "while" or "for", it
gives you a hint at programmer intent in ways that an explicit GOTO might not.

Another major reason is to allow you to separate concerns.  Writing
`trieSumExplicit` forces you to think "how to fold this entire trie".  Writing
`trieSumAlg` allows us to just focus on "how to fold *this immediate* layer".
You only need to ever focus on the immediate layer you are trying to sum ---
and never the entire trie.  `cata` takes your "how to fold this immediate
layer" function and turns it into a function that folds an entire trie, taking
care of the recursive descent for you.

::::: {.note}
**Aside**

Before we move on, I just wanted to mention that `cata` is not a magic
function.  From the clues above, you might actually be able to implement it
yourself.  For our `Trie`, it's:

```haskell
!!!trie/trie.hs "cata'"
```

First we `project :: Trie k v -> TrieF k v (Trie k v)`, to "de-recursive" our
type.  Then we fmap our entire `cata alg :: Trie k v -> a`.  Then we run the
`alg :: TrieF k v a -> a` on the result.  Basically, it's
fmap-collapse-then-collapse.

:::::

### Ana Montana

Anamorphisms are functions that "generate" or "unfold" a value of a recursive
type, layer-by-layer.  If we want to write a function of type `A -> Trie k v`,
we can reach first for an anamorphism.

Anamorphisms work by unfolding "layer-by-layer", from the top down.  We can
write one by defining "how to generate the next layer".  This description comes
in the form of a "coalgebra" (pronounced like "co-algebra", and not like coal
energy "coal-gebra"), in terms of the base functor:

```haskell
myCoalg :: A -> TrieF k v A
```

If we think of `TrieF k v a` as "one layer" of a `Trie k v`, then `A -> TrieF k
v A` describes how to generate a new nested layer of our `Trie k v` from our
initial "seed" (here, of type `A`).  Remember that a `TrieF k v A` contains a
`Maybe v` and a `Map k A`.  The `A` (the values of the map) are then used to
seed the *new* subtries.  The `A` is the "continue expanding with..." value.

And then we can use `cata` to "unfold" our value along the coalgebra:

```haskell
ana myCoalg :: A -> Trie k v
```

`ana` starts from the an initial seed `A`, runs `myCoalg` on that, and then
goes down a layer, running `myCoalg` on each value in the map to create new
layers, etc., forever and ever.  In practice, it usually stops when we return a
`TrieF` with an empty map, since there are no more seeds to expand down.
However, it's nice to remember we don't have to special-case this behavior: it
arises naturally from the structure of maps.

I don't think there's really too many canonical or classical anamorphisms that
are as generally applicable as summing and counting leaves, but the general
idea is that if you want to create a value by repeatedly "expanding leaves",
an anamorphism is a perfect fit.

An example here that fits will with the nature of a trie is to produce a
"singleton trie": a trie that has only a single value at a single trie.

```haskell
!!!trie/trie.hs "mkSingletonCoalg"
```

Given a `v` value, we'll make a coalgebra `[k] -> TrieF k v [k]`.  Our "seed"
will be the `[k]` key we want to insert, and we'll generate our singleton key
by making sub-maps with sub-keys.

Our coalgebra ("layer generating function") goes like this:

1.  If our key-to-insert is empty `[]`, then we're here!  We're at *the layer*
    where we want to insert the value at, so `MkTF (Just v) M.empty`.

2   If our key-to-insert is *not* empty, then we're not here!  We return `MkTF
    Nothing`...but we know we leave a singleton map `M.singleton k ks :: Map k [k]`
    leaving a single seed.  When we run our coalgebra with `ana`, `ana` will go
    down and expand out that single seed (with our coalgebra) into an entire new
    sub-trie, with `ks` as its seed.

So, we have `singleton`:

```haskell
!!!trie/trie.hs "singleton"
```

We run the coalgebra on our initial seed (the key), and ana will run
`singletonCoalg` repeatedly, expanding out all of the seeds we deposit, forever
and ever (or at least until there are no more values of the seed type left,
which happens if we return an empty map).

```haskell
ghci> singleton "hi" 7
MkT Nothing $ M.fromList [
    ('h', MkT Nothing $ M.fromList [
        ('i', MkT (Just 7) M.empty )
      ]
    )
  ]
```

### Trie from Map

This 

```haskell
fromMapCoalg
    :: Ord k
    => Map [k] v
    -> TrieF k v (Map [k] v)
fromMapCoalg = uncurry rebuild . M.foldMapWithKey splitOut
  where
    rebuild  v      xs = MkTF (getFirst <$> v) (M.fromListWith M.union xs)
    splitOut []     v  = (Just (First v), mempty                 )
    splitOut (k:ks) v  = (mempty        , [(k, M.singleton ks v)])

fromMap
    :: Ord k
    => Map [k] v
    -> Trie k v
fromMap = ana fromMapCoalg

```

## Down to Business

So those are some examples to get our feet wet; now it's time to build our
prequel meme trie!



