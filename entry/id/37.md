Unique sample drawing & searches with List and StateT --- "Send more money"

============================================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on April 24, 2015.
> [Read online!](https://blog.jle.im/entry/unique-sample-drawing-searches-with-list-and-statet.html)

Nothing too crazy today, just a cute (basic/intermediate) haskell trick as a
response to Mark Dominus's excellent [Universe of
Discourse](http://blog.plover.com) post on [Easy exhaustive search with the list
monad](http://blog.plover.com/prog/haskell/monad-search.html) intended for
people new or unfamiliar with haskell demonstrating the common "list monad as a
constraint solver" approach that is standard fare for learning Haskell. I myself
have literally done [an entire series of blog
posts](http://blog.jle.im/entries/series/+monadplus-success-failure-monads) on
this usage.

Mark's use case however incorporates a bit of an extra pattern not typically
discussed. The list monad is good for taking "independent samples" of things
(looking at different samples from a list):

``` haskell
ghci> do x <- "abc"
         y <- "abc"
         z <- "abc"
         return [x,y,z]
["aaa","aab","aac","aba","abb" ... ]
```

However, what if you wanted to instead "draw" from a pool, and represent
different drawings? Traditionally, the answer was something like:

``` haskell
ghci> do x <- "abc"
         y <- filter (/= x) "abc"
         z <- filter (/= y) . filter (/= x) $ "abc"
         return [x,y,z]
"abc","acb","bac","bca","cab","cba"]
```

This is a little bit awkward...and it definitely gets a lot worse ($O(n^2)$)
when you have more items. Also, it relies on an `Eq` constraint --- what if our
thing doesn't have an `Eq` instance? And this also falls apart when our list
contains duplicate items. If we had used `"aabc"` instead of `"abc"`, the result
would be the same --- despite having more `'a'`s to pick from!

**Important note:** After writing this article, I found out that Douglas Auclair
in [11th issue of the Monad
Reader](https://wiki.haskell.org/wikiupload/6/6a/TMR-Issue11.pdf) solved this
exact same problem with pretty much the exact same approach. (Oops!) If you want
to do further reading, check it out! :D

## StateT

There's a type in the *transformers* library that provides a very useful monad
instance:

``` haskell
data StateT s m a = StateT (s -> m (a, s))
```

A `StateT s m a` is a function that takes an initial state `s` and returns a
result `a` with a modified state...in the context of `m`.

Specialize for `m ~ []` and we get

``` haskell
data StateT s [] a = StateT (s -> [(a, s)])
```

Which is basically describing a function from a initial state to a list of *ways
you can modify the state*, and different results from each one. It returns a
list of "all ways you can mutate this state".

For example,

``` haskell
foo :: StateT Int [] Bool
foo = StateT $ \x -> [(even x, x+1), (odd x, x-1), (x > 0, negate x)]
```

So `foo` takes a number, `x`, and says, "here are three ways we might proceed
from having this number. We can return whether or not it's even, in which case
the new state is `x+1`...we can return whether or not it's odd, in which case
the new state is `x-1`....or we can return whether or not it's positive, in
which case the new state is `negate x`"

What the monad instance does is that it allows you to "chain" forks, and go
along different forks, and gather together "all possible forks" you could have
taken. At the end, it outputs all possible forks. So if you did `foo >> foo`,
there'd be nine results --- one result for when you took the first route (the
`x+1`) twice, one for when you took the first and then the second (`x-1`), one
for when you took the first and the third....and the second and the
first...etc., etc.

### MonadPlus

One other tool we have at our disposal is `guard`:

``` haskell
guard :: Bool -> StateT Int [] ()
```

which is a `StateT` action that says "kill this current branch if given `False`,
or go on if given `True`"

## The Problem

The problem, as stated, was to find distinct digits for each letter to solve:

        S E N D
    +   M O R E
    -----------
      M O N E Y

So `SEND` is a four-digit number, `MORE` is a four-digit number, and `MONEY` is
a five-digit number that is the sum of the two. The first digit of `MONEY` has
to be the first digit of `MORE`, the last digit of `MORE` has to be the second
digit of `SEND`, etc.

The previous approach was done using the entire "pick from all
possibilities...except for the ones already chosen", using `(/=)` and filtering
over all of the things seen vs all of the things to pick from.

However, we can abstract over "picking dependently from a sample" by defining a
function called `select`, which really should be in the base libraries but isn't
for some reason:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/send-more-money.hs#L7-L9

select :: [a] -> [(a, [a])]
select []     = []
select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]
```

(Implementation thanks to Cale, who has fought valiantly yet fruitlessly to get
this into base for many years.)

`select` will take a list `[a]` and return a list of different "selected" `a`s,
with the rest of the list, too:

``` haskell
ghci> select "abcd"
[('a',"bcd"),('b',"acd"),('c',"abd"),('d',"abc")]
```

But, hey...does the type signature of `select` look like anything familiar?

It looks *exactly* like something that `StateT` is supposed to describe! Give an
initial state (`[a]`), and returns a list of all possible ways to "mutate" that
state (by removing one element from the state), and a result from each mutation
(the removed element).

``` haskell
StateT select :: StateT [a] [] a
```

And armed with this...we have all we need

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/send-more-money.hs#L3-L35

import Control.Monad             (guard, mfilter)
import Control.Monad.Trans.State
import Data.List                 (foldl')

asNumber :: [Int] -> Int
asNumber = foldl' (\t o -> t*10 + o) 0

main :: IO ()
main = print . flip evalStateT [0..9] $ do
    s <- StateT select
    e <- StateT select
    n <- StateT select
    d <- StateT select
    m <- StateT select
    o <- StateT select
    r <- StateT select
    y <- StateT select
    guard $ s /= 0 && m /= 0
    let send  = asNumber [s,e,n,d]
        more  = asNumber [m,o,r,e]
        money = asNumber [m,o,n,e,y]
    guard $ send + more == money
    return (send, more, money)
```

Remember, `StateT` here operates with an underlying state of `[Int]`, a list of
numbers not yet picked. `StateT select` picks one of these numbers, and modifies
the state to now only include the items that were not picked. So every time you
sequence `StateT select`, `select` draws from a smaller and smaller pool of
numbers, and makes the state list smaller and smaller. What sequencing `StateT`
does is allow us to explore *all* of the possible ways we could pick and modify
state, all at once. Using `guard`, we then "close off" and kill off the paths
that don't end up how we'd like.

`asNumber` takes a list like `[1,2,3]` and turns it into the number `123`;
credit to the source blog.

And, to test it out...

``` bash
$ ghc -O2 send-more-money.hs
$ ./send-more-money
# [(9567,1085,10652)]
```

It returns the one and only solution, `SEND = 9567`, `MORE = 1085`, and
`MONEY = 10652`.[^1]

::: note
**Aside**

We can make things a little bit more efficient with minimal cost in
expressiveness. But not that it matters...the original version runs fast
already.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/send-more-money.hs#L38-L59

select' :: [a] -> [(a,[a])]
select' = go []
  where
   go xs [] = []
   go xs (y:ys) = (y,xs++ys) : go (y:xs) ys

main' :: IO ()
main' = print . flip evalStateT [0..9] $ do
    s <- mfilter (/= 0) $ StateT select'
    m <- mfilter (/= 0) $ StateT select'
    e <- StateT select'
    n <- StateT select'
    d <- StateT select'
    o <- StateT select'
    r <- StateT select'
    y <- StateT select'
    let send  = asNumber [s,e,n,d]
        more  = asNumber [m,o,r,e]
        money = asNumber [m,o,n,e,y]
    guard $ send + more == money
    return (send, more, money)
```

This is a more performant version of `select` [courtesy of Simon
Marlow](http://chimera.labs.oreilly.com/books/1230000000929/pr01.html) that
doesn't preserve the order of the "rest of the elements".

Also, we use `mfilter` to "eliminate bad `s` and `m`s" right off the bat, before
having to pick any more things. `mfilter` can be thought of as "killing the fork
immediately" if the action doesn't satisfy the predicate. If the `s` picked
doesn't match `(/= 0)`, then the entire branch/fork is immediately ruled
invalid.
:::

By the way, isn't it neat that it does all of this in "constant space"? It just
keeps track of the output list, but the actual search processes is in constant
space. You don't need to keep track of all `10!` combinations in memory at once.
Hooray laziness!

## Other Applications

Using `select` and `StateT`, we can do a lot of things involving picking from a
sample, or permutations. Anything that you used to awkwardly do by using filter
not-equal-to's can work now. You can do things like drawing from a deck:

``` haskell
pokerGame :: [Ordering]
pokerGame = flip evalStateT [0..51] $ do
    p2Hand <- replicateM 5 (StateT select)
    p1Hand <- replicateM 5 (StateT select)
    return $ pokerCompare p1Hand p2Hand
```

Which would draw five distinct cards from a deck of `[0..51]`, and return who
won for each draw (assuming you had a suitable
`pokerCompare :: [Card] -> [Card] -> Ordering`). Note that if you use
`runStateT`, you'd get the results (the winner), *as well as* the leftover cards
in the deck for each path!

You can even combine the two sorts of drawings --- sampling independently (like
rolling dice) using `lift`, and drawing from an underlying deck. For example,
you might encode some game logic from a board game like monopoly:

``` haskell
combo = flip evalStateT initialDeck $ do
    roll <- lift [1..6]
    draw <- StateT select
    ...
```

Whenever you want a dice roll, use `lift [1..6]`...and whenever you want to draw
from the deck, use `StateT select`.

What you get in the end, remember, is a list of "all possible paths". You'll get
a list of every possible result from all of your different rolling and drawing
choices.

Happy Haskelling!

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

[^1]: For some reason this runs pretty slowly if you use `runghc`/`runHaskell`,
    but it runs in the blink of an eye when you actually compile it (and
    especially with optimizations on). The difference is pretty striking...and I
    don't really know what's going on here, to be honest. If anyone does know a
    good explanation, I'd love to hear it :)

