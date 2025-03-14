Abstracting over Sequential Random Algorithms with Free

========================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/abstracting-over-sequential-random-algorithms-with-free.html)

It's fair enough to say that I'm a little late to the free monad party, but I
still think their power is greatly either misunderstood or underrated or obscure
in Haskell, and this article will be my attempt to throw more examples of its
usage.

Here we're going to construct a type that can represent sequential computations
using randomness or entropy, and we're going to abstract over our entropy source
--- not by swapping out the pseudorandom generator, but by really abstracting
over what our computation really *is* to the barest essentials. We're going to
abstract over what an sequential random algorithm even is.

The man plain is to first create a type that represents just a value produced
from a random number...and then figure out how to chain them.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/free-random/Rand.hs#L18-L22

data RandF a where
    FromRandom   :: Random r =>           ( r  -> a) -> RandF a
    FromRandomR  :: Random r => r -> r -> ( r  -> a) -> RandF a
    FromRandoms  :: Random r =>           ([r] -> a) -> RandF a
    FromRandomRs :: Random r => r -> r -> ([r] -> a) -> RandF a
```

For those of you unfamiliar with GADT syntax, this is just basically specifying
a type by the type of its contructors; `RandF` has four constructors --- one of
which, `FromRandom`, takes a function `(r -> a)`, and returns a `RandF a`...as
long as the `r` is an instance of the `Random` typeclass (from the *random*
package).

For comparison, `Maybe` could have been written like:

``` haskell
data Maybe a where
    Just    :: a -> Maybe a
    Nothing :: Maybe a
```

Anyways, every constructor is basically made with a function, "*If* I had a
random number, what would I do with it?" `FromRandom` basically contains a
function `r -> a`...if I had a random number, how would I get my `a`? Note that
this can really contain a function from *any* `r` you want, as long as it is a
part of the `Random` typeclass. So we can have something that takes any random
`Bool` to a `String`:

``` haskell
FromRandom (\r -> show (r :: Bool)) :: RandF String
```

Which says, "if I had a random `Bool`, then I'd `show` it, to get a `String`".

Or maybe something that takes any random `Double` to an `Int`, like:

``` haskell
FromRandom (\r -> round (100 * r * r :: Double)) :: RandF Int
```

"If I had a random `Double`, I'd square it and divide it by three, then round
it, to get my `Int`".

Or something as simple as just getting a random `Double`:

``` haskell
FromRandom id :: RandF Double
```

"If I had a random `Double`...well, that's what I want in the end."

`FromRandomR` takes a range first before giving the function; `FromRandoms`
asks, "if I had an infinite list of random items, what would I do?"

``` haskell
FromRandomR 0 10 (\r -> 1 / sqrt r) :: RandF Double
FromRandoms (\rs -> sum (take 10 rs)) :: RandF Double
```

We can "evaluate" the random value in a `RandF` by just generating a random
value of the type desired and then applying the function to it. One typical way
of doing this is to ask for a random generator/seed from the user:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/free-random/Rand.hs#L31-L35

runRandomF :: RandomGen g => RandF a -> g -> a
runRandomF (FromRandom f)         = f . fst . random
runRandomF (FromRandomR r0 r1 f)  = f . fst . randomR (r0, r1)
runRandomF (FromRandoms f)        = f . randoms
runRandomF (FromRandomRs r0 r1 f) = f . randomRs (r0, r1)
```

We can try some of these out:

``` haskell
ghci> let s = mkStdGen 192837465
ghci> runRandomF (FromRandom (\r -> show (r :: Bool))) s
"False"
ghci> runRandomF (FromRandom (\r -> round (100 * r * r))) s
32
ghci> runRandomF (FromRandom id :: RandF Double) s
0.5631451666688826
ghci> runRandomF (FromRandomR 0 10 (\r -> 1 / sqrt r)) s
0.4213954281350406
ghci> runRandomF (FromRandoms (sum . take 10) :: RandF Double) s
9.434604856390711
```

We might also realize that we can make a `Functor` instance on `RandF`, where
`fmap` is like applying the fmapping function to the outbound value. For
example, if we had `FromRandom (\r -> r * 2)`, if we `fmap show`, we would want
`FromRandom (\r -> show (r * 2))`, so whenever we "ran" the `RandF`...if it was
"meant" to make a `10` originally, it would now make a `"10"`.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/free-random/Rand.hs#L24-L29

instance Functor RandF where
    fmap h rnd = case rnd of
        FromRandom         f -> FromRandom         (h . f)
        FromRandomR r0 r1  f -> FromRandomR r0 r1  (h . f)
        FromRandoms        f -> FromRandoms        (h . f)
        FromRandomRs r0 r1 f -> FromRandomRs r0 r1 (h . f)
```

``` haskell
ghci> let s = mkStdGen 192837465
ghci> let r = FromRandom (\r -> round (100 * r * r))
ghci> runRandomF r s
32
ghci> runRandomF (fmap show r) s
"32"
ghci> runRandomF (fmap negate r) s
-32
```

Put in a rather abstract way, if you think of a `RandF Double` as something that
"contains" a random `Double`, waiting to be computed...then `fmap show` applies
`show` to the "contained" random `Double`.

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

