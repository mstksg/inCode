Wolf, Goat, Cabbage: The List MonadPlus & Logic Problems

=========================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on December 26, 2013.
> [Read online!](https://blog.jle.im/entry/wolf-goat-cabbage-the-list-monadplus-logic-problems.html)

Today we're going to learn to solve the classic and ageless logic problems
without any data structures besides List's monadic properties as a MonadPlus!

We are going to be solving this old-as-time logic puzzle, which
[Wikipedia](http://en.wikipedia.org/wiki/Fox,_goose_and_bag_of_beans_puzzle)
claims dates back to the 9th century:

> A farmer has a wolf, a goat, and a cabbage that he wishes to transport across
> a river. Unfortunately, his boat can carry only one thing at a time with him.
> He can't leave the wolf alone with the goat, or the wolf will eat the goat. He
> can't leave the goat alone with the cabbage, or the goat will eat the cabbage.
> How can he properly transport his belongings to the other side one at a time,
> without any disasters?

We're going to assume a somewhat basic familiarity with functional programming
concepts and a basic understanding of monads (if you don't know that much, check
out
[adit's](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)
great concise guide). If you aren't familiar with MonadPlus/Alternative (and how
they work as monads) check out [Part
1](http://blog.jle.im/entry/practical-fun-with-monads-introducing-monadplus) and
[Part
2](http://blog.jle.im/entry/the-list-monadplus-practical-fun-with-monads-part),
which should provide all the background and most of the syntax. Most Haskell
syntax is either be explained here as we get to it or in the previous parts.
Still, if you have any questions, feel free to leave a comment, give [Learn You
A Haskell](http://learnyouahaskell.com) a quick read, or stop by freenode's
friendly #haskell!

### A MonadPlus Review

The usefulness of a monad depends on how you define the characteristic "bind" or
"chaining" behavior. For this article, MonadPlus refers to the design pattern
(and Haskell typeclass) where you model this "chaining" as a "success/fail"
process[^1].

There is a common language with to talk about this process: `mzero` means "fail
here" and `return x` means "succeed with a result of the value `x` here". So
chaining is implemented such that chaining anything to a failure will propagate
that failure forward. That is, `mzero >> return x` = `mzero`.

## Our Approach

So, armed with what we learned in [Part
2](http://blog.jle.im/entry/the-list-monadplus-practical-fun-with-monads-part),
let's formulate a general plan for finding all solutions in `n` moves.

Now, in the List monad, we can think of things as "journeys" or stories: subject
your value to a long and arduous journey, specifying at every step of the way
what choices it has to continue. Then specify where journeys fail and end. At
the end of it all, the result is a list of the finishing values of all trails
that have completed the journey.

With the List monad, we say "Here is the description of *a* (single) journey.
What journeys following this description succeed?"

So what could this journey be for us? Well, we think of a journey in this
situation as the accumulation of moves to a plan. We start out with a blank plan
("Do nothing"). The next step, we add one move to our plan ("Just move the fox",
for example). Then the next step, we add another move ("First move the fox, then
move the farmer").

1.  Start with a blank plan; a tabula rasa.
2.  Add a legal and safe move to it.
3.  Repeat Step 2 `n` times
4.  Fail if you aren't a solution; succeed if you are.

Simple, right? We just laid out *the path of a single plan*, from its birth to
its eventual death or ascension.

This is the most significant thing about this approach: it allows you to
describe **one journey**, in general terms, and List will "automatically" find
out all successful journeys that fit your mold. You don't ever have to worry
about the ensemble or manually deal with explicit branching or filtering.
Cognitively, all you have to do is *write **one** story*. Just *one*. That is
the power of the List Monad abstraction.

## Our Types

The first thing we do when writing any Haskell program: define our types!

``` haskell
data Character = Farmer | Wolf | Goat | Cabbage -- 1
        deriving (Show, Eq, Enum)

newtype Move = MoveThe Character                -- 2
        deriving (Eq)

instance Show Move where                        -- 3
    show (MoveThe Farmer)  = "F"
    show (MoveThe Wolf)    = "W"
    show (MoveThe Goat)    = "G"
    show (MoveThe Cabbage) = "C"

type Plan = [Move]                              -- 4

data Position = West | East                     -- 5
    deriving (Show, Eq)
```

1.  First, we define the enumerated type `Character` all the characters we will
    be working with: the farmer, the wolf, the goat, and the cabbage.
2.  Next, we define a simple `Move` container, which just contains a character.
    A `MoveThe Farmer` will represent a movement of only the farmer, a
    `MoveThe Wolf` will represent the movement of both the farmer and the wolf,
    etc.
3.  For the purposes of easy debugging, we're going to define our own instance
    of `Show` for moves so that we can use `print` on them.
4.  A simple type synonym; a `Plan` is just a list of `Move`s. Note that we are
    not using this list as a MonadPlus --- it's just a plain dumb list of moves
    in our plan.
5.  A `Position` type: either on the west bank or on the east bank of the river.
    Everyone starts out on the west bank, and we want them all to end up on the
    east bank.

::: note
**Welcome to Haskell!**

Hi! These "Welcome to Haskell" asides are for people unfamiliar with Haskell,
mostly for Haskell syntax stuff. If you already feel comfortable, feel free to
skip them.

There's a lot of Haskell syntax and concepts here; mostly, all we are doing is
declaring new types.

1.  We declare that `Character` is "either" a `Farmer`, `Wolf`, `Goat`, or
    `Cabbage`. This is like saying that a `Bool` is either a `False` or a
    `True`: in fact, you could define your own `Bool` with something like this:
    (or even your own `Int`)

    ``` haskell
    data Bool = False | True
    data Int = -536870912 ... | -1 | 0 | 1 | 2 | ... 536870911
    ```

    The `deriving` syntax tells the compiler to automatically derive functions
    for printing the type (Show), testing for equality (Eq), and enumerating
    through them (Enum)

2.  We declare a new type `Move` which is just a wrapper around a `Character`.
    We can create a new `Move` by using `MoveThe`:

    ``` haskell
    ghci> :t MoveThe
    MoveThe :: Character -> Move
    ghci> :t MoveThe Wolf
    MoveThe Wolf :: Move
    ```

    (`ghci>` represents a command at the interactive prompt ghci, and `:t` asks
    for the type of whatever comes after it)

3.  Here we define custom functions for printing out a `Move`

4.  Here is a type synonym `Plan`. Every time we use `Plan` as a type, we really
    mean `[Move]`, and the compiler treats the two things as the same.

5.  `Position`: same deal as `Character`.
:::

## Implementation

### The Final Step

We're going to skip to the end and write our final step and what it is supposed
to be, and then fill in the functions that are necessary to make it happen.

The last stage of our journey is after we have made all `n` moves, we end the
journey if it is not a solution.

``` haskell
makeNMoves :: Int -> [Plan]         -- 1
isSolution :: Plan -> Bool

findSolutions :: Int -> [Plan]      -- 2
findSolutions n = do
    p <- makeNMoves n               -- 3
    guard $ isSolution p            -- 4
    return p                        -- 5
```

1.  The type signatures of the helper functions we will be using.
2.  `findSolutions n` is going to be the all successful plans after `n` moves.
3.  Let `p` be a plan after `n` moves have been added to it. Note that
    `makeNMoves` is itself a journey --- a sub-journey. So `p` is a single plan
    that has *already gone through* the `makeNMoves` journey. We are continuing
    that journey.
4.  End the journey unless `p` is a solution (all characters are on the east
    side)
5.  Succeed with `p` if the journey has not yet ended.

Hm. Sounds good! We're done!

So now we only need to implement `makeNMoves` and `isSolution`!

::: note
**Welcome to Haskell!**

Haskell is a functional language...but that "do" block sure looks very
imperative to me. What gives?

As explained in [Part
1](http://blog.jle.im/entry/practical-fun-with-monads-introducing-monadplus),
all do blocks are just syntactical sugar for repeated applications of `>>=`:

``` haskell
findSolutions :: Int -> [Plan]
findSolutions =
    makeNMoves n >>= (\p -> guard (isSolution p) >> return p)
```

And `>>=` is just the (hopefully) familiar bind. Again, look at [Part
1](http://blog.jle.im/entry/practical-fun-with-monads-introducing-monadplus) or
[adit's](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)
tutorial for a fuller explanation.
:::

### makeNMoves

`makeNMoves` is going to be the main logic of our program. We want it to be a
journey, itself --- a journey of a single plan going through `n` additions of
moves.

That means we want something like:

``` haskell
makeMove :: Plan -> [Plan]

startingPlan :: Plan
startingPlan = []

makeNMoves :: Int -> [Plan]
makeNMoves n = do
    m1 <- makeMove startingPlan
    m2 <- makeMove m1
    m3 <- makeMove m2
    -- ... (n times)
    mn <- makeMove mx
    return mn
```

Which says "The journey of `makeNMoves` is repeatedly making a move `n` times."

Of course we have seen that particular type of `do` block before, it is simply:

``` haskell
makeNMoves :: Int -> [Plan]
makeNMoves n =
    makeMove startingPlan >>= makeMove
        >>= makeMove >>= makeMove   -- ...
        >>= makeMove                -- (n times)
```

Luckily there is a function in the standard library that allows us to repeatedly
apply a function `n` times: `iterate :: (a -> a) -> a -> [a]`. `iterate f x`
takes a function `f :: a -> a` and repeatedly applies it to a starting value
`x :: a` and yields the results as a list:

``` haskell
iterate f x = [ x, f x, f (f x), f (f (f x)) ... ]
```

And so to get the `n`th application, we use `iterate f x !! n` (`!!` being the
indexing function, getting the `n`th element of the list)

So now we can define `makeNMoves`:

``` haskell
makeNMoves :: Int -> [Plan]
makeNMoves n = iterate (>>= makeMove) (return startingPlan) !! n
```

We say "apply `(>>= makeMove)` `n` times, starting the single starting plan".

::: note
**Welcome to Haskell!**

Remember that `return x >>= f` is the same as `f x`. You can see this here:

``` haskell
foo1 = do
    y <- return x
    f y

-- identical
foo2 = f x
```

Where `return x` says "succeed with the value `x`", and `y <-` says "set `y` to
the value of that success". Of course, `y` is just going to be `x`, because we
had just said "succeed with the value of `x`. That means that `f y` is the same
as `f x`.
:::

Even though the syntax is not the cleanest, it is important to remember here
that what we are doing is simply defining the journey `makeNMoves` as the result
of taking `n` `makeMove` journeys one after the other. The same as that first do
block.

### isSolution

Let's define our helper function `isSolution :: Plan -> Bool`. Basically, we
want to check if the positions of all of the characters are `East`.

First, we need a way to get the position of a farmer/animal after a given plan
has been executed.

#### positionOf

Our function `positionOf :: Plan -> Character -> Position` is going to take a
`Plan` and a `Character`, and report what side of the river the character is on.

Because every single move swaps the position of the farmer, the final position
of the farmer depends only on the even-/odd-ness of the number of total moves.
If it is even, then the farmer is on the west bank still (consider 0 moves, two
moves, etc.). If it is odd, then the farmer is on the east bank.

``` haskell
positionOf :: Plan -> Character -> Position
positionOf p c = case c of
    Farmer  -> positionFromCount $ length p
    _       -> undefined
    where
        positionFromCount n | even n      = West
                            | othherwise  = East
```

Now, what if we want to know about non-farmers?

Instead of finding the total number of moves, we only need to find the number of
moves involving that given animal.

Let's first filter the Plan `p` by moves involving the character `c`:

``` haskell
filter (== MoveThe c) p
```

This will return a new Plan, but with only the moves involving the character
`c`. We can then use the length of *that*.

::: note
**Welcome to Haskell!**

`filter :: (a -> Bool) -> [a] -> [a]` is a common function that takes a
predicate `a -> Bool` and a list, and returns a new list with only the items for
which the predicate returns true.

`(== MoveThe c)` is a function that returns true if the move is equal to
`MoveThe c`.
:::

Putting it all together:

``` haskell
positionOf :: Plan -> Character -> Position
positionOf p c = case c of
    Farmer  -> positionFromCount . length $ p
    c       -> positionFromCount . length $ filter (== MoveThe c) p
    where
        positionFromCount n | even n      = West
                            | othherwise  = East
```

::: note
**Welcome to Haskell!**

What is `positionFromCount . length $ p`?

In Haskell, the `(.)` operator represents function composition. `(f . g) x` is
equivalent to `f (g x)`. "Apply `g` first, then apply `f`".

Also recall that you can think of `$` as adding an implicit parentheses around
both sides of it. You visualize it like the spine of a butterfly --- the "wings"
are wrapped parentheses around either side of it. In that sense, `f . g $ x` is
the same as `(f . g) (x)` (A rather lopsided butterfly).

So, altogether, `positionFromCount . length $ p` is the same as
`(positionFromCount . length) p`, which says "first, find the length of `p`,
then turn that length into a position."

In the same way, `positionFromCount . length $ filter (== MoveThe c) p` is
`(positionFromCount . length) (filter (== MoveThe c) p)` --- find the length of
the filtered list, then turn that length into a position. We use `$` mostly
because we don't like writing parentheses everywhere when we don't have to.
:::

Does this actually work? Let's try out some examples.

``` haskell
ghci> let p = [MoveThe Goat, MoveThe Farmer, MoveThe Wolf, MoveThe Goat]
ghci> positionOf p Goat
West
ghci> positionOf p Wolf
East
ghci> positionOf p Farmer
West
```

It works! By the way, as an unrelated note, isn't it cool that our `Plan`
literal reads a lot like English? MoveThe Goat, MoveThe Farmer, MoveThe Wolf...

#### Checking the Path

Now we have to check that the plan is a solution.

Simple --- that means that all `Characters` are on the east side.

We can check this manually:

``` haskell
isSolution :: Plan -> Bool
isSolution p =
    positionOf p Farmer == East
    && positionOf p Wolf == East
    && positionOf p Goat == East
    && positionOf p Cabbage == East
```

Hm. Rather ugly.

We see a common pattern that we need `positionOf p c` for all `c`s. That looks
like a map!

We also compare all of them to `East`. That sounds like a job for the prelude
function `all :: (a -> Bool) -> [a] -> Bool`, which takes a predicate and a list
and returns true if all items in the list satisfy the predicate.

Let's piece it all together:

``` haskell
isSolution p = all (== East) positions
    where
        positions = map (positionOf p) [Farmer ..]
```

::: note
**Welcome to Haskell!**

`map` is probably the most ubiquitous concept in functional programming --- it
takes a function and a list and returns a new list with the function applied to
every item.

For example, `map f [x,y,z]` = `[f x, f y, f z]`. If we wanted to find the
lengths of a list of strings, we'd do:

``` haskell
map length ["alice","bob"]
= [length "alice", length "bob"]
= [5,3]
```

So in our case:

``` haskell
map (positionOf p) [Farmer, Wolf, Goat, Cabbage]
= [ positionOf p Farmer         -- Position of the farmer
  , positionOf p Wolf           -- Position of the wolf
  , positionOf p Goat           -- Position of the goat
  , positionOf p Cabbage        -- Position of the cabbage
  ]
```
:::

We use `[Farmer ..]` as shorthand for `[Farmer, Wolf, Goat, Cabbage]` --- this
is because `Character` is an Enum, so it can be enumerated using enumeration
syntax. It basically means "`Farmer`, etc."

### makeMove

So let's get down to the meat of our journey. How do we make a move?

``` haskell
makeMove :: Plan -> [Plan]
```

`makeMove` will be a function that takes a plan and returns all the successful
ways you can add a move to that plan. It takes a plan and takes it through a
journey of adding a move, and returns the results of all of the successful ways
it can fulfill this journey. This is similar to our old
`halveOrDouble :: Int -> [Int]`, which takes an int and returns the successful
paths our int could have taken (it could have been halved...or doubled).

What does a plan have to "go through" in its journey in adding a move?

1.  First, we get the move we want to add. We could pick a `MoveThe Farmer`, a
    `MoveThe Goat`, or anything!
2.  Then, we fail/end the journey if we pick a move that isn't legal. For
    example, we can't move the goat if the farmer is not on the same side of the
    river that the goat is on.
3.  Now, we add that move that we got to the plan.
4.  Then, we fail/end the journey if that new plan is "unsafe" --- if it leaves
    either the Wolf and Goat alone on a riverbank or the Goat and Cabbage.
5.  At the end of it all, we succeed with the new plan.

Let's try this out:

``` haskell
moveLegal :: Plan -> Move -> Bool           -- 1
safePlan :: Plan -> Bool

makeMove :: Plan -> [Plan]
makeMove p = do
    next <- MoveThe <$> [Farmer .. Cabbage] -- 2
    guard $ moveLegal p next                -- 3
    let
        p' = p ++ [next]                    -- 4
    guard $ safePlan p'                     -- 5
    return p'                               -- 6
```

1.  Here are the types of the helper functions we will be using.
2.  In this context, `MoveThe <$>` means to apply `MoveThe` to whatever we
    choose out of `[Farmer .. Cabbage]`. Kind of an "intercept it on the way
    out, and turn it into a Move". So `next` is `MoveThe Farmer` or
    `MoveThe     Wolf`, etc.; `next` is *one* of those. For every journey, we
    pick *one* of the possible moves.
3.  We insta-fail if the move is not legal with the given plan. By this, we mean
    that we can't possibly move an animal unless the farmer is on the same side
    as the animal.
4.  Let's let `p'` be `next` appended to the original plan `p`.
5.  We insta-fail unless the new plan is safe.
6.  If we haven't failed yet, then we succeed with the new plan as the result.

::: note
**Welcome to Haskell!**

Okay, so I was slightly hand-wavey with `<$>`. But it is true that something
like:

``` haskell
x <- (*2) <$> Just 3
```

will put 6 (`3 * 2`) into `x` --- it'll take out the 3 and then apply `(*2)` to
it before storing it in `x`.

What's going on under the hood is actually less magical. `<$>` basically says
"apply inside". It is like `$`, but "inside". Remember how we can do:

``` haskell
ghci> (*2) $ 3
6
```

to apply `(*2)` to 3? We can then also do:

``` haskell
ghci> (*2) $ 3
6
ghci> (*2) <$> Just 3
Just 6
ghci> (*2) <$> [3]
[6]
```

Now, if we think of a List like a list of possible successes, then applying a
function "inside" means applying the function to all of the possible successes:

``` haskell
ghci> (*2) <$> [3,4,5]
[6,8,10]

ghci> MoveThe $ Farmer
MoveThe Farmer
ghci> MoveThe <$> [Farmer, Wolf, Goat, Cabbage]
[MoveThe Farmer, MoveThe Wolf, MoveThe Goat, MoveThe Cabbage]
```

So when I say

``` haskell
next <- MoveThe <$> [Farmer, Wolf, Goat, Cabbage]
```

I really mean

``` haskell
next <- [MoveThe Farmer, MoveThe Wolf, MoveThe Goat, MoveThe Cabbage]
```

But still, it sometimes is cool to think of it as "Get the item inside, and then
apply this function to it before you bind it to your variable", if only for
funsies.
:::

#### Thought experiment

So let's say our plan is, currently,
`[MoveThe Goat, MoveThe Farmer, MoveThe Wolf]`. At the end of it all, our goat,
wolf, and farmer are on the east bank, and the cabbage is on the west bank.

What happens on a typical journey of `makeMove`?

1.  First, we pick something to move. Let's say `next` is `MoveThe Farmer`.
2.  This move is legal (moving the farmer is always legal).
3.  Our new plan is
    `[MoveThe Goat, MoveThe Farmer, MoveThe Wolf, MoveThe     Farmer]`
4.  This plan is not safe. If we move the farmer, the goat and the wolf will be
    alone, and that is bad news for the goat. We fail at the second guard.
5.  We don't return anything, because this journey is a total and utter failure.

Huh. How unfortunate. Let's try again with another pick for `next`:

1.  Let's pick `MoveThe Cabbage` this time for `next`.
2.  This move isn't even legal! The cabbage is on the west bank but the farmer
    is on the east. Failure!

Well, that's kind of depressing. Let's try another:

1.  We pick `MoveThe Goat` for `next`.
2.  This move is legal; both the goat and the farmer are on the east bank.
3.  Our new plan is
    `[MoveThe Goat, MoveThe Farmer, MoveThe Wolf, MoveThe     Goat]`.
4.  This plan is indeed safe. The goat and the cabbage are now on the west bank,
    but so is the farmer.
5.  Because all is well, we return our new plan!

Hooray!

As an exercise, see how the journey fares if we had picked `MoveThe Wolf` for
`next`.

Anyways, at the end of it all, `makeMove` will return all new plans from the
successful journeys. So it won't be returning the plans with `MoveThe Farmer`
and `MoveThe Cabbage` added to it, but will likely be retuning the plans with
`MoveThe Goat` and `MoveThe Wolf` added to it. And it'll return those two
together in a List structure.

We're almost there! Now to just define our helper predicates `moveLegal` and
`safePlan`.

#### moveLegal

What makes a move legal? Well, the farmer has to be on the same side as whatever
is being moved.

We can re-use our `positionOf :: Plan -> Character -> Position` function here.

``` haskell
moveLegal :: Plan -> Move -> Bool
moveLegal p (MoveThe Farmer)  = True
moveLegal p (MoveThe c)       = positionOf p c == positionOf p Farmer
```

#### safePlan

One last piece. How can we tell if a plan is safe or not?

The plan is safe if nothing can eat anything else. That means if the wolf and
goat or goat and cabbage sit on the same bank, so too must the farmer. Some
boolean arithmetic will show that this is the same as if either the farmer is on
the same side as the goat or the goat and cabbage are both "safe" (not on the
side of their predators).

``` haskell
safePlan :: Plan -> Bool
safePlan p = goatPos == farmerPos || safeGoat && safeCabbage
    where
        goatPos     = positionOf p Goat
        farmerPos   = positionOf p Farmer
        safeGoat    = goatPos /= positionOf p Wolf
        safeCabbage = positionOf p Cabbage /= goatPos
```

And...that's it! We finished!

#### Exercise

Notice that sometimes we are going to make "redundant moves". For example, we
could move the farmer or goat twice in a row. How can we add another guard to
check if the move isn't redundant? That is, that the move we are adding isn't
identical to the last move of the plan?

The implementation is in the final solution later on, but think about how you
would do it and compare the final solution to yours!

## Wrapping Up

The final code for this project is available [on
Github](https://github.com/mstksg/inCode/blob/master/code-samples/monad-plus/WolfGoatCabbage.hs)
so you can follow along yourself. You can also [load it interactively
online](https://www.fpcomplete.com/user/jle/wolf-goat-cabbage) on FPComplete, a
great online Haskell IDE where you can test your code right there in the
browser.

So...let's test it!

### Tests

First, let's load it up on ghci:

``` haskell
ghci> :l WolfGoatCabbage.hs
Ok, modules loaded: Main.
```

Let's try a few plan lengths and see when we get one that has a valid solution:

``` haskell
ghci> findSolutions 5
[]
ghci> findSolutions 6
[]
ghci> findSolutions 7
[[G,F,W,G,C,F,G],[G,F,C,G,W,F,G]]
```

Great, we have two solutions of length 7. If we try them out, it seems like they
both work! Notice that, interestingly enough, the two solutions are their own
reverses. This makes sense, because any solution of getting from the west bank
to the east bank must also be, backwards, a valid solution of getting from the
east bank to the west bank.

It turns out that the solutions of length 9 and 11 are both identical to the
solutions for length 7, just with some redundant moves thrown in (moving the
farmer twice in a row, moving the goat twice in a row, etc.). Also, note that
all possible solutions are of odd lengths, because for even lengths, the farmer
ends up on the west bank.

If we add the filter on redundant moves mentioned earlier, the next valid
solutions with no direct redundancies come at length 13, and then at 19:

``` haskell
ghci> findSolutions 13
[[G,F,W,G,C,W,G,C,W,G,C,F,G]
,[G,F,C,G,W,C,G,W,C,G,W,F,G]]
ghci> findSolutions 19
[[G,F,W,G,C,W,G,C,W,G,C,W,G,C,W,G,C,F,G]
,[G,F,C,G,W,C,G,W,C,G,W,C,G,W,C,G,W,F,G]]
```

Again note that both of these solutions come in pairs, with one being the
reverse of the other. Also curious is the fact that they are actually identical
to the length 7 solutions, just with cycles of `W,G,C` (or `C,G,W`) over and
over again in the middle.

### Reflections

We have solved the classic logic puzzle without using any control flow other
than the List's MonadPlus instance. The solution isn't necessarily optimal, but
it is interesting that we can model something like this simply as saying: "Here
is the description of a journey. What journeys following this description
succeed?"

With the List MonadPlus, you can solve any problem that can be described as the
result of a nondeterministic journey with choices and pitfalls along the way.

In this particular puzzle, you could have done something similar from the start
using only maps and filters. However, sometimes it is more useful or more
insightful to, instead of using maps and filters, use abstractions that help you
frame the problem in a more meaningful way.

Hopefully as a result of this three part series and through playing around with
the source code, you can appreciate the wonders of Succeed/Fail and MonadPlus!

### The future

Where to go from here? You might want to take a look at the
[Alternative](http://hackage.haskell.org/package/base/docs/Control-Applicative.html)
typeclass/design pattern, which also deals with the concept of success/failure
--- just not with their consecutive chaining, like MonadPlus. It deals with
their parallel choices, actually, as the name implies. This functionality is
redundantly implemented in MonadPlus in Haskell today (2013), and the
parallel-choice operator `<|>` for Alternative is `mplus` for MonadPlus. I might
write something on the matter some day. Anyways, learning about Alternative will
help you see more about the usefulness of the success/fail design pattern, and
it might help you gain the perspective which much of the early Haskell
implementors apparently lacked: not everything is a monad!

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

[^1]: You might be aware that in the current Haskell standard library
    organization, the implementation of MonadPlus also provides separate
    functionality --- the "Plus". We won't be focusing on this part, because it
    is commonly regarded that it is more of a characteristic of the
    *Alternative* typeclass/design pattern. For the purposes of this article,
    MonadPlus is essentially "MonadZero", as it should have been.

