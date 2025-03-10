Shuffling things up: Applying Group Theory in Advent of Code

=============================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on November 18, 2020.
> [Read online!](https://blog.jle.im/entry/shuffling-things-up.html)

So it's November, and [Advent of Code](https://adventofcode.com/) season is in
the air! It's time for everyone's favorite Santa-based light hearted
learn-to-program-or-a-new-language holiday season programming challenge series.
Every year a bunch of us gather around the fireplace, roast chestnuts, and
brainstorm all of the interesting ways we can solve these cute themed puzzles
every day. These puzzles are designed to accessible enough for most new
programmers, but deep enough to provide entertainment for experienced ones. I've
[written many blog
posts](https://blog.jle.im/entries/tagged/advent-of-code.html) on some of the
interesting insight some of the puzzles have yielded, and I also [post my
reflections on as many puzzles I
can](https://github.com/mstksg/advent-of-code-2019/blob/master/reflections.md)
while solving them in Haskell. And if you're solving things in Haskell, I also
published an [open-sourced rate-limited API
library](https://hackage.haskell.org/package/advent-of-code-api) so you can
fetch and submit answers from the comfort of your command line.

To kick off the season, I've decided to write about one of my favorite puzzles
from Advent of Code 2019 -- [Day 22: Slam
Shuffle](https://adventofcode.com/2019/day/22). To me, it stands out because
it's a perfect example of how Haskell's approach to mathematical abstraction
nudges you into the direction of an efficient solution --- in a way that other
languages would obscure or make less obvious.

So, let's dive in! In the end, hopefully this post can get you excited for this
wonderful season, and maybe also shed some insight into what it means when we
say that Haskell can help you leverage math to find good solutions to your real
problems.

Of course, this post has spoilers for Advent of Code 2019 Day 22, if you are
planning on trying to figure it out from yourself. If you haven't tried it, I
recommend you give it a shot and come back after! :D

## Slam Shuffle

If you haven't already, take some time to [read through the problem
statement](https://adventofcode.com/2019/day/22). The basic idea is that we are
given a series of operations to "shuffle" a deck of 10007 cards, such as:

    deal with increment 7
    deal into new stack
    deal into new stack
    ... etc

After performing all of the many operations, the question then asks about the
card at a given position (the 2019th card in the deck).

Part 2, which you might not be able to see if you haven't submitted an answer
yet for Part 1, involves the same process with a deck of 119315717514047 cards,
and repeating the entire shuffling sequence 101741582076661 times. It then asks
you to find the card that ends up at index 2020.

In this problem, it seems we have a list of "shuffles" that we want to run on a
deck of cards. However, let's think about this in a more data-driven approach:
instead of thinking about successive shufflings of cards, let's imagine the
specification of a "shuffle" itself as our main data, and how we can combine
shuffle operations together into new shuffle operations.

We are looking for "take shuffle A and shuffle B, and return a new shuffle that
represents doing B, then A". This is "shuffle composition", or "permutation
composition" ([permutation](https://en.wikipedia.org/wiki/Permutation) being the
mathematical word for "shuffling" here, basically)

Since we've identified that we want to begin implementing a way of
composing/combining permutations together, we can do a bit of reading to learn
that one of the most famous properties of permutation composition is that they
form a "group", which means they can be composed (associatively), have an
identity, and can be inverted. This means that if you have two permutations, you
can "squish" them to create a new permutation, and work with that *new*
permutation.

I've talked about [using group
theory](https://blog.jle.im/entry/alchemical-groups.html) principles before in
this blog to help guide us towards solutions and optimizations --- the main
principle is that if we express our program in terms of group operations, then
we can take advantage of the large body of knowledge built up over centuries to
understand, analyze, and potentially optimize our program.

The *first* big advantage in this situation is that we can treat our
transformations *as data*, and not as functions. And that if we have two
transformations, we can always create a new one (just a normal data type value)
that represents the composition of the two original ones.

## Now You're Thinking With Groups

Knowing permutations are a group, it means that once we settle on our
representation of them, `Perm`, we can write an instance of `Perm` for
`Semigroup`, `Monoid`, and `Group`, abstractions in Haskell that many types are
already instances of. Abstractions like `Semigroup` and `Monoid` are pretty much
an everyday thing in Haskell, so this fits in quite nicely. `Group` comes from
the *[groups](https://hackage.haskell.org/package/groups)* package, which also
provides some nice applications of group theory.

``` haskell
data Perm n = ... -- let's figure out the implementation later, where n is the number of cards
```

In Haskell, we express things like "`Perm` is a Semigroup/Monoid/Group" by
saying that they are instances of *typeclasses*, which (for this purpose) are
like interfaces in languages like Java.

``` haskell
-- | An instance m can be "combined" using `x <> y`
class Semigroup m where
    (<>) :: m -> m -> m

-- | There is always an identity element for <>:
--
-- x <> mempty == x
-- mempty <> x == x
--
class Semigroup m => Monoid m where
    mempty :: m

-- | Every m has an inverse:
--
-- x <> invert x == mempty
-- invert x <> x == mempty
--
class Monoid m => Group m where
    invert :: m -> m
```

This means that if `Perm` is an instance of `Group` (which has superclasses
`Semigroup` and `Monoid`), we can:

-   Compose permutations using `x <> y`, which means "shuffle with strategy `y`,
    then with strategy `x`"
-   Summon an "identity permutation" where `x <> mempty == x` (the identity
    permutation, which is "leave things alone").
-   Invert any shuffling (if we have `x`, we can reverse its effect with
    `invert x`)

In addition, the standard libraries also give us a useful function `stimes`

``` haskell
stimes :: Semigroup m => Int -> m -> m
```

which lets us compose `x` with itself (`stimes 5 x == x <> x <> x <> x <> x`),
but can do it in *log(n)* time using [repeated
squaring](https://en.wikipedia.org/wiki/Exponentiation_by_squaring). It's
extremely efficient in a lot of circumstances (more on that later) --- more so
than the naive compose-it-n-times implementation. This will definitely become
useful in part 2, where we have to do 101741582076661 compositions.

## Our Gameplan

Just *knowing* that permutations form a group naturally guides us to these
abstractions --- we already know what *interface* our type will have, even
before we write any code. We know that no matter *what* our implementation of
permutation will be, we will have `(<>)`, `stimes`, `mempty`, `invert` available
to us to use. So, let's do just that! We'll use a stub data type `Perm` to
represent our permutation and "pretend" we have that interface on it. We'll
write our functions first and then fill in the interface later!

``` haskell
-- | Represents a permutation of n cards
data Perm n = ....

-- | Given a permutation, find the place where a given index ends up.
runPerm :: Perm n -> Finite n -> Finite n

-- | Parse a string line into the permutation it represents
parsePerm :: String -> Perm n

-- | Given a permutation list, find the place where 2019 ends up
part1 :: [Perm 10007] -> Finite 10007
part1 perms = runPerm bigPerm 2019
  where
    bigPerm = mconcat perms
```

(`mconcat perms` composes all of the permutations one after another:
`mconcat [x,y,z] = x <> y <> z`)

And...that's it! For the actual "logic" of our part 1! All we need to do is
implement `runPerm` and `parsePerm`.

Here, I'm using `Finite n` from the great
*[finite-typelits](https://hackage.haskell.org/package/finite-typelits)*
library, where `Finite 100` represents "an index between 0 and 99", etc. It's
just exactly the right "shape" to represent the index of a deck of cards.
*finite-typelits* wasn't designed with group theory in mind, but it's still a
great tool here --- which is a testament to how flexible these abstractions can
actually be :)

For example, it means that for a `Perm 10007` (a permutation of 10007 cards),
the type of `runPerm` is `Perm 10007 -> Finite 10007 -> Finite 10007`, and the
type of `parsePerm` is `String -> Perm 10007`.

We can plan out our part 2 as well:

``` haskell
-- | Given a permutation list, find the index that will end up at 2020
part2 :: [Perm 119315717514047] -> Finite 119315717514047
part2 perms = runPerm (invert biiigPerm) 2020
  where
    bigPerm   = mconcat perms
    biiigPerm = stimes 101741582076661 bigPerm
```

Part 2, I think, is where the group theory really shines.

1.  We take advantage of `stimes`, which uses [repeated
    squaring](https://en.wikipedia.org/wiki/Exponentiation_by_squaring). That
    means that to compute `stimes 8 x`, instead of using

        x <> x <> x <> x <> x <> x <> x <> x

    it does

        let x2 = x <> x
            x4 = x2 <> x2
        in  x4 <> x4

    essentially cutting down the number of multiplications exponentially. This
    means that to compute `stimes 101741582076661`, we only need to do about 47
    multiplications (log base 2), and not 101741582076661.

    This is only possible because we know that permutation composition is
    associative, so it doesn't matter how we associate our parentheses. It is
    only "safe" to use repeated squaring if you *know* that your operation is
    associative. Having a semigroup abstraction *in the first place* guides us
    to this efficient solution --- in a way that is pre-built just for us! This
    is made all the more powerful because *semigroup* is a ubiquitous
    abstraction in Haskell, so we "think about" it all the time.

2.  Remember how `runPerm p 2019` gives us the index that `2019` is sent to?
    Well, we want something else in this case. We basically want the index that
    *will be sent to* `2020`. So, we want to *reverse the function*. Luckily,
    since our function is just a permutation, it is easy to reverse this: just
    `invert` the permutation!

    The idea that we can simply invert a permutation instead of having to write
    a whole new permutation representation just to do "backwards indexing" is
    something that we are *guided to*, just by recognizing that permutations
    form a group.

## A first guess at implementation

Now, time to do what we have been putting off and actually write our permutation
representation -- the definition of `Perm n`. A good *first guess* might be to
write our permutation as an actual function --- a function from index to index,
`Finite n -> Finite n`. Then, we can just use function composition as our
permutation composition.

``` haskell
data Perm n = Perm (Finite n -> Finite n)

runPerm :: Perm n -> Finite n -> Finite n
runPerm (Perm f) x  = f x

parsePerm :: KnownNat n => String -> Perm n
parsePerm str = case words str of
    "cut":n:_           -> Perm $ \i -> i - modulo (read n)
    "deal":"into":_     -> Perm $ \i -> maxBound - i
    "deal":"with":_:n:_ -> Perm $ \i -> i * modulo (read n)

instance Semigroup (Perm n) where
    Perm f <> Perm g = Perm (f . g)     -- apply g, then apply x
instance Monoid (Perm n) where
    mempty = Perm id
instance Group (Perm n) where
    invert (Perm f) = ?????
```

Note that `Finite n`'s `Num` instance is modular arithmetic, so things like
`negate` and multiplication will "do the right thing". We use `modulo`:

``` haskell
modulo :: KnownNat n => Integer -> Finite n
```

which "reads" an `Integer` into a `Finite n`, making sure to wrap it in a cyclic
way if it is negative or too high. `maxBound` also gives us the highest index
(the highest `Finite n`).

``` haskell
ghci> modulo 3 :: Finite 10
finite 3
ghci> modulo 15 :: Finite 10
finite 5
ghci> modulo (-1) :: Finite 10
finite 9
```

The `KnownNat` instance is a constraint that `modulo` needs in order to know
what quotient to modulo into.

This implementation *seems* to work, except for one apparent major problem: how
do we write `invert`? Also, `stimes` doesn't help us *too* much here, because
repeated squaring of function composition is...still a lot of function
compositions in the end.[^1] So, while composition with `<>` is cheap,
application with `runPerm` is expensive (and `stimes` works best when
composition is expensive and application is cheap). So, back to the drawing
board.

## A Second Implementation Attempt: Lookin' Affine Today

If we look carefully at `parsePerm`, we might start to see a pattern in all of
our permutations. In fact, they all seem to follow the same form:

``` haskell
"cut":n:_           -> Perm $ \i -> i - modulo (read n)
"deal":"into":_     -> Perm $ \i -> negate i + maxBound
"deal":"with":_:n:_ -> Perm $ \i -> i * modulo (read n)
```

They all seem to be some "scaling" and "adding" of `i`. If we align things up,
this becomes a little more clear:

``` haskell
"cut":n:_           -> Perm $ \i ->                1 * i - modulo (read n)
"deal":"into":_     -> Perm $ \i ->               -1 * i + maxBound
"deal":"with":_:n:_ -> Perm $ \i ->  modulo (read n) * i
```

Each of these seems to be some sort of scaling-and-adding of `i`...also known as
an [Affine Transformation](https://en.wikipedia.org/wiki/Affine_transformation),
but modulo some cyclic rotation.

Well...affine transformations on cyclic indices are a subset of permutations in
general. More importantly, we know (after some googling) that they are also
*closed with respect to composition and inversion* ... which means that they
are, themselves, a group! Maybe we can represent this as our permutation type:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/advent-shuffle.hs#L16-L28

data Affine n = Aff
    { aScale :: Finite n
    , aShift :: Finite n
    }

runPerm :: KnownNat n => Affine n -> Finite n -> Finite n
runPerm (Aff a b) x = a * x + b

parseAffine :: KnownNat n => String -> Affine n
parseAffine str = case words str of
    "cut":n:_           -> Aff                1  (-modulo (read n))
    "deal":"into":_     -> Aff        (negate 1)          maxBound
    "deal":"with":_:n:_ -> Aff (modulo (read n))                 0
```

This is "defunctionalization": if we notice a pattern in our functions, we can
instead abstract out the data that defines each instance of that pattern, and
work with that data instead.

So far so good! Now to think about how to define composition.

If we want to do $f(x) = a' x + b'$ after $g(x) = a x + b$, it's:

$$
\begin{aligned}
(f \circ g)(x) & = a' (a x + b) + b'\\
  (f \circ g)(x) & = a' x + a' b + b'
\end{aligned}
$$

So composing `a' x + b'` after `a x + b` is is `a' a x + a' b + b'`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/advent-shuffle.hs#L30-L31

instance KnownNat n => Semigroup (Affine n) where
    Aff a' b' <> Aff a b = Aff (a' * a) (a' * b + b')
```

Neat! We can now compose *and* run `Affine`s efficiently, which makes `stimes`
useful! And the `Num` instance (which requires `KnownNat n`) for `Finite n`
takes care of automatically doing modular arithmetic for us.

To define a `Monoid` instance, we need an identity permutation. This would just
leave x alone, so it makes sense that it's $f(x) = 1 x + 0$, `1 x + 0`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/advent-shuffle.hs#L33-L34

instance KnownNat n => Monoid (Affine n) where
    mempty = Aff 1 0
```

Now let's define the inverse, which is a bit trickier.

``` haskell
instance KnownNat n => Group (Affine n) where
    invert (Aff a b) = Aff a' b'
      where
        a' = -- ??
        b' = -- ??
```

*Inverting* something means that we want `invert p <> p == mempty`. That means
we want to find `a'` and `b'` such that:

``` haskell
      Aff a' b' <> Aff a b = Aff 1 0
```

From our definition of `<>` earlier, that means we have to find `a'` and `b'`
where:

``` haskell
Aff (a' * a) (a' * b + b') = Aff 1 0
```

So we need `a' * a = 1`, and `a' * b + b' = 0`.

To solve `a' * a = 1`, we can imagine that cycling `a` through the whole deck
gets you back to `a`. (If `n` is prime, then `a`, `a*a`, `a*a*a`, etc. will all
be unique...so you will keep on getting unique numbers until you exhaust the
entire space at `a^size` to arrive back at `a`) So:

             a^n = a
    => a^(n-1)*a = a    -- definition of exponentiation
    => a^(n-1)   = 1    -- a^(n-1) leaves a unchanged, so it must be 1
    => a^(n-2)*a = 1    -- definition of exponentiation

From this we can see that if `a' * a = 1`, then `a'` must be `a^(n-2)` for prime
`n`.[^2][^3]

The second case is a little simpler: we can just shuffle around
`a' * b + b' = 0` to get `b' = -(a' * b)`.

This gives us everything we need to write `invert`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/advent-shuffle.hs#L36-L41

-- | Group instance only works if n is prime
instance KnownNat n => Group (Affine n) where
    invert (Aff a b) = Aff a' b'
      where
        a' = a ^ (natVal (Proxy @n) - 2)
        b' = negate (a' * b)
```

And...we're done! This actually is pretty efficient with repeated squaring
(which is how `^` is implemented) because we are just squaring numbers.
`natVal (Proxy @n)` is how to get `n` as an integer at the value level so we can
use it as the exponent.

## The Full Implementation

Just to close us out, I'll re-paste the code we planned before, now with the
context that we have implemented the appropriate permutation types. We get the
`[Affine n]`s by using `parseAffine` on the `lines` of our puzzle input and
reversing that list.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/advent-shuffle.hs#L43-L54

-- | Part 1: Given a permutation list, find the place where 2019 ends up
part1 :: [Affine 10007] -> Finite 10007
part1 perms = runPerm bigPerm 2019
  where
    bigPerm = mconcat perms

-- | Part 2: Given a permutation list, find the index that will end up at 2020
part2 :: [Affine 119315717514047] -> Finite 119315717514047
part2 perms = runPerm (invert biiigPerm) 2020
  where
    bigPerm   = mconcat perms
    biiigPerm = stimes 101741582076661 bigPerm
```

You can load the finished code for this entire challenge
[here](https://github.com/mstksg/inCode/tree/master/code-samples/misc/advent-shuffle.hs).
I've also included the sample input string for my advent of code account, and
also parsed it conveniently into a list of properly ordered `Affine n`s for you
to test it yourself:

    $ ./advent-shuffle.hs
    ghci> part1 myShuffles
    finite 6978
    ghci> part2 myShuffles
    finite 24460989449140

As expected, Haskell performs these \~47 multiplication steps pretty quickly,
and part 2 is only about 3 times slower than part 1 (\~40μs vs. \~14μs on my
machine).

## The Big Picture

Every time I make a post about how Haskell lets you "use" math, there's a lot of
room for confusion and misunderstanding. A common misconception is that you need
to know math to use Haskell, or that writing a Haskell program is like solving a
math equation.[^4]

Instead, when we say we "use" math in Haskell, it means that Haskell naturally
nudges us to phrase our problems in a way that can help illuminate connections
to the groundwork that has already been laid for us through centuries of
mathematical discoveries --- and in many cases, allow us to translate those
insights into making helpful improvements and optimizations in our actual code.

Haskell is "functional programming", but I think that betrays the major insight
here: we got our main conceptual leap when we thought about shuffling not as "a
function", but rather *as data*: our shuffle is itself *data* (here, integers),
and not an "algorithm". Had we latched onto an algorithmic approach from the
beginning, we might have gotten stuck in the mire of finding a way to "optimize
an algorithm". But because we initially started thinking about permutations and
shuffles as *data structures*, we actually end up thinking about how to most
effectively manipulate the data structures themselves. Instead of manipulating
the cards, we manipulate the shuffle! We combine and invert the *shuffles*, not
the cards. And math --- especially abstract algebra --- is all about different
properties of how objects can combine and universal properties about certain
operations.

As we head into this wonderful season, stay safe and happy haskellings,
everyone! :D

## Special Thanks

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts. Very special
thanks to my supporter at the "Amazing" level on
[patreon](https://www.patreon.com/justinle/overview), Josh Vera! :)

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

[^1]: We only allocate a few function pointers (once for each `<>`, where [both
    sides themselves point to the same function
    pointer](https://www.reddit.com/r/haskell/comments/jwl93i/shuffling_things_up_solving_advent_of_code_with/gcudwg4?utm_source=share&utm_medium=web2x&context=3)),
    so it's very efficient in space as well, but to actually "run" that final
    function, we need to still traverse all of those nested pointers the full
    number of times.

[^2]: You can also use the [Extended Euclidean
    Algorithm](https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm) to
    find the multiplicative inverse here as well if you are a (cool) nerd. But I
    wanted to show a way to do this without requiring knowledge of any ring
    theory.

[^3]: As [pointed out by rogercaptain on
    reddit](https://www.reddit.com/r/haskell/comments/jwl93i/shuffling_things_up_solving_advent_of_code_with/gct4ihy/?context=3),
    this also "works" in the case where `n` is not prime too: only *some* (and
    not all) `Affine n`s represent permutations when `n` is not prime, and for
    those specific `Affine n`s (namely, where `a` is coprime to `n`), this
    technique does work.

[^4]: Admittedly, we did do that a few times here. But that's not *all* we do :)

