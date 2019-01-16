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
!!!misc/trie.hs "data Trie k v"
```

A `Trie k v` will have keys of type `[k]`, where `k` is the key token type, and
values of type `v`.  Each layer might have a value (`Maybe v`), and branches
out to each new layer.

We could write the trie storing `(to, 9)`, `(ton, 3)`, and `(tax, 2)` as:

```haskell
import qualified Data.Map          as M
import qualified Data.Map.NonEmpty as NEM

myTrie :: Trie Char Int
myTrie = MkT Nothing $ M.fromList [
      ('t', MkT Nothing $ M.fromList [
          ('o', MkT (Just 9) $ M.fromList [
              ( 'n', MkT (Just 3) M.empty )

            ]
          )
        , ('a', MkT Nothing $ M.fromList [
              ( 'x', MkT (Just 2) M.empty )
            ]
          )
        ]
      )
    ]
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
!!!misc/trie.hs "data TrieF k v"
```

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
!!!misc/trie.hs "type instance Base" "instance Recursive (Trie k v)" "instance Corecursive (Trie k v)"
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



<!-- data Trie k v = TNow   v (Map   k (Trie k v)) -->
<!--               | TLater   (NEMap k (Trie k v)) -->

<!-- We can define what we call an "algebra" on `sum` -->



<!-- Now let's think of what happens when we put `ListF a` back "into" `x`: -->

<!-- 1.  If we "close off" the placeholder, we get `ListF a ()`, which is -->
<!--     essentially a list with at most 1 item (`NilF`, or `ConsF theVal ()`) -->

<!-- 2.  If we put `ListF a` back into the placeholder, we get `ListF a (ListF a -->
<!--     ())`, which is a list with at most 2 items: -->

<!--     ```haskell -->
<!--     zeroItems :: ListF Int (ListF Int ()) -->
<!--     zeroItems = NilF -->

<!--     oneItem   :: ListF Int (ListF Int ()) -->
<!--     oneItem   = ConsF 1 NilF -->

<!--     twoItems  :: ListF Int (ListF Int ()) -->
<!--     twoItems  = ConsF 1 (ConsF 2 ()) -->
<!--     ``` -->

<!-- 3.  If we put `ListF a (ListF ())` into the placeholder, we get `ListF a (ListF -->
<!--     a (ListF a ()))`, which is a list with at most 3 items: -->

<!--     ```haskell -->
<!--     zeroItems  :: ListF Int (ListF Int (ListF Int ())) -->
<!--     zeroItems  = NilF -->

<!--     oneItem    :: ListF Int (ListF Int (ListF Int ())) -->
<!--     oneItem    = ConsF 1 NilF -->

<!--     twoItems   :: ListF Int (ListF Int (ListF Int ())) -->
<!--     twoItems   = ConsF 1 (ConsF 2 NilF) -->

<!--     threeItems :: ListF Int (ListF Int (ListF Int ())) -->
<!--     threeItems = ConsF 1 (ConsF 2 (ConsF ())) -->
<!--     ``` -->

<!-- From this process, we can see that if we repeat this "forever", we get `ListF a -->
<!-- (ListF a (ListF a ...))`; if we have "infinite nesting", we get potentially -->
<!-- infinitely long lists, and we get back the original data type. -->

<!-- In Haskell, we can express "infinitely re-nested" using a data type, `Fix`: -->

<!-- ```haskell -->
<!-- newtype Fix f = Fix (f (Fix f)) -->
<!-- ``` -->

<!-- We can say that "`List` is just an infinitely re-nested `ListF`" by writing: -->

<!-- ```haskell -->
<!-- type List a = Fix (ListF a) -->
<!-- ``` -->

<!-- This is the same as our original `List`!  This is because a `Fix (ListF a)` -->
<!-- is either `Fix NilF`, or `Fix (ConsF theVal theRest)`, where `theRest` has type -->
<!-- `Fix (ListF a)`, or just `List a`.  And that's exactly the original `List` -->
<!-- type! -->

<!-- ```haskell -->
<!-- zeroItems :: List Int -->
<!-- zeroItems = Fix NilF -->

<!-- oneItem   :: List Int -->
<!-- oneItem   = Fix (ConsF ) -->
<!-- ``` -->
