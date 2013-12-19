Wolf, Goat, Cabbage: The List MonadPlus and Logic Problems (Part 3 of 3)
========================================================================

Categories
:   Haskell
:   Ramblings
Tags
:   functional programming
Series
:   MonadPlus: Success/Failure Monads
:   Practical Monads
CreateTime
:   2013/12/17 13:16:07
PostDate
:   Never
Identifier
:   monad-plus-3

Today we're going to learn to solve classic and ageless logic problems without
any data structures besides List's monadic properties as a MonadPlus!

If you aren't familiar with MonadPlus/Alternative, or Haskell in general,
check out [Part 1][] and [Part 2][] --- it provides all of the background
necessary for any new, beginning or intermediate Haskeller to understand this,
including all essential Haskell syntax :)

[Part 1]: http://blog.jle.im/entry/practical-fun-with-monads-introducing-monadplus
[Part 2]: http://blog.jle.im/entry/the-list-monadplus-practical-fun-with-monads-part

We are going to be solving this old-as-time logic puzzle, which [Wikipedia][]
claims dates back to the 9th century:

[Wikipedia]: http://en.wikipedia.org/wiki/Fox,_goose_and_bag_of_beans_puzzle

> A farmer has a wolf, a goat, and a cabbage that he wishes to transport
> across a river.  Unfortunately, his only boat can carry one thing at a time.
> He can't leave the wolf alone with the goat, or the wolf will eat the goat.
> He can't leave the goat alone with the cabbage, or the goat will eat the
> cabbage.  How can he properly transport his belongings to the other side one
> at a time, without any disasters?


### A monadPlus Review

Remember that in this series, MonadPlus refers to the design pattern (and
Haskell typeclass) where you model the chaining functions on objects together
as a "success/fail" process.

<aside>
    ###### Note

You might be aware that in the current Haskell standard library organization,
the implementation of MonadPlus also provides separate functionality ---  the
"Plus".  We won't be focusing on this part, because it is commonly reguarded
that it is more of a characteristic of the *Alternative* typeclass/design
pattern.  For the purposes of this article, MonadPlus is essentially
"MonadZero", as it should have been.
</aside>

We adopt a common language to talk about this process: `mzero` means "fail
here" and `return x` means "succeed with the value `x` here".  Chaining works
in the manner that chaining anything to a failure will propagate that failure
forward.

Our Approach
------------

So, armed with what we leared in part 2, let's formulate a general plan for
finding all solutions in `n` moves.  In the List monad, we like to to think of
things as "journeys" --- subject your value to a long and arduous journey,
specifying at every step of the way what choices it has to continue.  Then
specify where journeys fail and end.  At the end of it all, all trails that
have completed the journey are the solution.

So what could this journey be?  How about we see this journey as the
accumulation of moves to a plan?  We start out with a blank plan ("Do
nothing").  The next step, we add one move to our plan: "Just move the
fox", for example.  Then the next step, we add another move: "First move the
fox, then move the farmer."

1.  At the beginning, your plan is blank.  You start out as a tabula
    rasa.
2.  Then, you add a "possible and safe" move (move the farmer, for example)
3.  Then, you add another "possible and safe" move (move the fox, for
    example).  Repeat this all until your total plan is `n` moves long.
    "Safe" means that it doesn't involve anything eating anything.
4.  After adding `n` moves, we fail you if you aren't a succesful
    plan/solution.

Seems simple enough, right?  Every possible plan will go through that journey,
and the ones that don't fail will be the ones that are succesful.

Note one important thing about this approach is that you model things as a
"journey" of one individual possibility.  At no point do you ever deal
manually with any branching or filtering and at no points do you ever deal
with the set of all movelists as a whole.  You abstract everything as if you
were telling *one individual* story.

So let's get down to it already!

Our Types
---------

The first thing we do when writing any Haskell program --- define our types!

~~~haskell
data Character = Farmer | Wolf | Goat | Cabbage -- 1
        deriving (Show, Eq, Enum)

data Move = Move Character                      -- 2
        deriving (Eq)

instance Show Move where                        -- 3
    show (Move Farmer)  = "F"
    show (Move Wolf)    = "W"
    show (Move Goat)    = "G"
    show (Move Cabbage) = "C"

type Plan = [Move]                              -- 4

data Position = West | East                     -- 5
    deriving (Show, Eq, Ord, Enum)
~~~

1.  First, we define the enumerated type `Character` all the characters we
    will be working with --- a farmer, a wolf, a goat, and a cabbage.
2.  Next, we define a `Move` container type, which just contains a character.
    A `Move Farmer` will represent a movement of only the farmer, a `Move
    Wolf` will represent the movement of both the farmer and the wolf, etc.
3.  For the purposes of easy debugging, we're going to define our own instance
    of `Show` for moves so that we can use `print` on them.
4.  A simple type synonym --- a `Plan` is just a list of `Move`s.  Note
    that we are not using this list as a MonadPlus --- it's just a list of
    moves in our plan.
5.  For convenience, we define a `Position` type --- either on the west bank
    or on the east bank of the river.  Everyone starts out on the west bank,
    and we want them all to end up on the east bank.

Implementation
--------------

### The Final Step

We're going to skip to the end and write our final step and what it
is supposed to be, and then fill in the functions that are necessary to make
it happen.

The last stage of our journey is after we have made all `n` moves, we end the
journey if it is not a solution.

~~~haskell
makeNMoves :: Int -> [Plan]         -- 1
isSolution :: Plan -> Bool

findSolutions :: Int -> [Plan]      -- 2
findSolutions n = do
    p <- makeNMoves n               -- 3
    guard $ isSolution p            -- 4
    return p                        -- 5
~~~

1.  The type signatures of the helper functions we will be using.
2.  `findSolutions` is going to be the all succesful plans after `n`
    moves.
3.  Let `p` be a plan after `n` moves have been added to it.
4.  End the journey unless `p` is a solution (all characters are on the east
    side)
5.  Succeed with `p` if the journey has not yet ended.

Hm.  Sounds good!  We're done!

So now we only need to implement `makeNMoves` and `isFinalSol`!

### makeNMoves

`makeNMoves` is going to be the main logic of our program.  We want it to be
a journey, itself --- a journey of a single solution going through `n`
additions of moves.

That means we want something like:

~~~haskell
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
~~~

Which says "The journey of `makeNMoves` is repeatedly making a move `n`
times."

Of course we have seen that particular type of `do` block before, it is
simply:

~~~haskell
makeNMoves :: Int -> [Plan]
makeNMoves n =
    makeMove startingPlan >>= makeMove
        >>= makeMove >>= makeMove   -- ...
        >>= makeMove                -- (n times)
~~~

Luckily there is a function in the standard library that allows us to
repeatedly apply a function `n` times --- `iterate :: (a -> a) -> a -> [a]`.
`iterate f x` takes a function `f :: a -> a` and repeatedly applies it to a
starting value `x :: a` and yields the results as a list:

~~~haskell
iterate f x = [ x, f x, f (f x), f (f (f x)) ... ]
~~~

And so to get the `n`th application, we use `iterate f x !! n` (`!!` being the
indexing function, getting the `n`th element of the list)

So now we can define `makeNMoves`:

~~~haskell
makeNMoves :: Int -> [Plan]
makeNMoves n = iterate (>>= makeMove) (return startingSol) !! n
~~~

We say "apply `(>>= makeMove)` `n` times, starting the single starting
plan".

<aside>
    ###### Note

Remember that `return x >>= f` is the same as `f x`.  You can see this here:

~~~haskell
foo1 = do
    y <- return x
    f y

-- identical
foo2 = f x
~~~

Where `return x` says "succeed with the value `x`", and `y <-` immediately
says "set `y` to the value of that success".  Of course, `y` is just going to
be `x`, because we had just said "succeed with the value of `x`.  That means
that `f y` is the same as `f x`.
</aside>

### isFinalSol

Let's define our function `isSolution :: Plan -> Bool`.  Basically, we
want to check if the positions of all of the characters are `East`.

First, we need a way to get the position of a farmer/animal after a given plan
has been executed.

#### positionOf

Our function `positionOf :: Plan -> Character -> Position` is going to
take a `Plan` and a `Character`, and report what side of the
river the character is on.

Because every single move swaps the position of the farmer, the final position
of the farmer depends only on the even-/odd-ness of the number of total moves.
If it is even, then the farmer is on the west bank still (consider 0 moves,
two moves, etc.).  If it is odd, then the farmer is on the east bank.

~~~haskell
positionOf :: Plan -> Character -> Position
positionOf p c = case t of
    Farmer  -> countToPosition $ length p
    _       -> undefined
    where
        countToPosition n | even n      = West
                          | othherwise  = East
~~~

Now, what if we want to know about non-farmers?

Instead of finding the total number of moves, we only need to find the number
of moves involving that given animal.

Let's first filter the Plan `p` by moves involving the character `c`:

~~~haskell
filter (== Move c) p
~~~

This will return a new Plan, but with only the moves involving the
character `c`.  We can then use the length of *that*.

~~~haskell
positionOf :: Plan -> Character -> Position
positionOf p c = case c of
    Farmer  -> countToPosition . length $ p
    c       -> countToPosition . length $ filter (== Move c) p
    where
        countToPosition n | even n      = West
                          | othherwise  = East
~~~

<aside>
    ###### Aside

What is `countToPosition . length $ p`?

In Haskell, the `(.)` operator represents function composition.
`(f . g) x` is equvalient to `f (g x)`.  "Apply `g` first, then apply `f`".

Also recall that you can think of `$` as adding an implict parentheses
around both sides of it.  You can think of it like the spine of a butterfly
--- the "wings" are wrapped parentheses around either side of it.  In that
sense, `f . g $ x` is the same as `(f . g) (x)` (A rather lopsided butterfly).

So, altogether, `countToPosition . length $ p` is the same as
`(countToPosition . length) p`, which says "first, find the length of `p`,
then turn that length into a position."

In the same way, `countToPosition . length $ filter (== Move c) p` is
`(countToPosition . length) (filter (== Move c) p)` --- find the length of the
filtered list, then turn that length into a position.  We use `$` mostly
because we don't like writing parentheses everywhere when we don't have to.
</aside>

Does this actually work?  Let's try out some examples.

~~~haskell
λ: let p = [Move Goat, Move Farmer, Move Wolf, Move Goat]
λ: positionOf p Goat
West
λ: positionOf p Wolf
East
λ: positionOf p Farmer
West
~~~

It works!  By the way, as an unrelated note, isn't it cool that our `Plan`
literal reads a lot like English? Move Goat, Move Farmer, Move Wolf...

#### Checking the Path

Now we have to check that the plan is a solution.

Simple --- that means that all `Characters` are on the east side.

We can check this manually:

~~~haskell
isSolution :: Plan -> Bool
isSolution p =
    positionOf p Farmer == East
    && positionOf p Wolf == East
    && positionOf p Goat == East
    && positionOf p Cabbage == East
~~~

Hm.  Rather ugly.

We see a common pattern that we need `positionOf p c` for all `c`s.  That
looks like a map!

We also compare all of them to `East`.  That sounds like a job for the prelude
function `all :: (a -> Bool) -> [a] -> Bool`, which takes a predicate and a
list and returns true if all items on the list satisfy the predicate.

Let's piece it all together:

~~~haskell
isSolution p = all (== East) positions
    where
        positions = map (positionOf p) [Farmer .. Cabbage]
~~~

We use `[Farmer .. Cabbage]` as shorthand for `[Farmer, Wolf, Goat, Cabbage]`
--- this is because `Character` is an Enum, so it can be enumerated using
enumeration syntax.  It basically means "all characters from `Farmer` to
`Cabbage`", ordered by the order they were defined.


### makeMove

So let's get down to the meat of our journey.  How do we make a move?

~~~haskell
makeMove :: Plan -> [Plan]
~~~

`makeMove` will be a function that takes a plan and returns all the succesful
ways you can make a move on that plan. This is similar to our old
`halveOrDouble :: Int -> [Int]`, which takes an int and returns the succesful
paths our int could have taken (it could have halved...or doubled)

What does a plan have to "go through" in its journey in adding a move?

1.  First, we get the move we want to add.  We could pick a `Move Farmer`, a
    `Move Goat`, or anything!
2.  Then, we fail/end the journey if we pick a move that isn't legal.  For
    example, we can't move the goat if the farmer is not on the same side of
    the river that the goat is on.
3.  Now, we add that move that we got to the plan.
4.  Then, we fail/end the journey if that new plan is "unsafe" --- if it
    leaves either the Wolf and Goat alone on a riverbank or the Goat and
    Cabbage.
5.  At the end of it all, we succed with the new plan.

Let's try this out:

~~~haskell
moveLegal :: Plan -> Move -> Bool           -- 1
safePlan :: Plan -> Bool

makeMove :: Plan -> [Plan]
makeMove p = do
    next <- Move <$> [Farmer .. Cabbage]    -- 2
    guard $ moveLegal p next                -- 3
    let
        p' = p ++ [next]                    -- 4
    guard $ safePlan p'                     -- 5
    return p'                               -- 6
~~~

1.  Here are the types of the helper functions we will be using.
2.  Look at me being all fancy applicative!  In this context, `Move <$>` means
    to apply `Move` to whatever we choose out of `[Farmer .. Cabbage]`.  Kind
    of an "intercept it on the way out, and turn it into a Move".  So `next`
    is `Move Farmer`, `Move Wolf`, etc.  `next` is *one* of those.  For every
    journey, we pick one of those.
3.  We insta-fail if the move is not legal with the given plan.  By this, we
    mean that we can't possibly move an animal unless the farmer is on the
    same side as the animal.
4.  Let's let `p'` be `next` appended to the original plan `p`.
5.  We insta-fail unless the new plan is safe.
6.  If we haven't failed yet, then we succed, returning the new plan in our
    success.

So let's say our plan is, currently, `[Move Goat, Move Farmer, Move Wolf]`.
At the end of it all, our goat, wolf, and farmer are on the east bank, and the
cabbage is on the west bank.

What happens on this journey of `makeMove`?

1.  First, we pick something to move.  Let's say `next` is `Move Farmer`.
    Technically, we pick `Farmer`, and then turn it into a `Move` as we are
    picking it.
2.  This move is legal (moving the farmer is always legal).
3.  Our new plan is `[Move Goat, Move Farmer, Move Wolf, Move Farmer]`
4.  This plan is not safe.  If we move the farmer, the goat and the wolf will
    be alone, and that is bad news for the goat.  We fail at the second guard.
5.  We don't return anything, because this journey is a total and utter
    failure.

Huh.  How unfortunate.  Let's try again with another pick for `next`:

1.  Let's pick `Move Cabbage` this time for `next`.
2.  This move isn't even legal!  The cabbage is on the west bank but the
    farmer is on the east.  Failure!

Well, that's kind of depressing.  Let's try another:

1.  We pick `Move Goat` for `next`.
2.  This move is legal --- both the goat and the farmer are on the east bank.
3.  Our new plan is `[Move Goat, Move Farmer, Move Wolf, Move Goat]`.
4.  This plan is indeed safe.  The goat and the cabbage are now on the west
    bank, but so is the farmer.
5.  Because all is well, we return our new plan!

Hooray!

As an exercise, see how the journey fares if we had picked `Move Wolf` for
`next`.

Anyways, at the end of it all, `makeMove` will return all new plans from the
succesful journeys.  So it won't be returning the plans with `Move Farmer` and
`Move Cabbage` added to it, but will likely be retuning the plans with `Move
Goat` and `Move Wolf` added to it.  And it'll return those two together in a
List strucure.

We're almost there --- now to just define our helper predicates `moveLegal`
and `safePlan`.

#### moveLegal

What makes a move legal?  Well, the farmer has to be on the same side as
whatever is being moved.

We can re-use our `positionOF :: Plan -> Character -> Position` function here.

~~~haskell
moveLegal :: Plan -> Move -> Bool
moveLegal p (Move Farmer)   = True
moveLegal p (Move c)        = positionOf p c == positionOf p Farmer
~~~

#### safePlan

One last piece.  How can we tell if a plan is safe or not?

Well, the plan is safe if the wolf and goat or goat and cabbage are together,
and the farmer is not.  Some boolean arithmetic shows that this is equivalent
to saying either the Farmer is with the Goat, or the goat and cabbage aren't
together and the wolf and goat aren't together.

<!-- $\neg ((P_w = P_g \wedge \neg (P_f = P_g)) \vee (P_g = P_c \wedge \neg (P_f = P_g))$ -->

~~~haskell
safePlan :: Plan -> Bool
safePlan p = goatPos == farmerPos || safeGoat && safeCabbage
    where
        goatPos     = positionOf p Goat
        farmerPos   = positionOf p Farmer
        safeGoat    = goatPos /= positionOf p Wolf
        safeCabbage = positionOf p Cabbage /= goatPos
~~~

And...that's it!

The Full Solution
-----------------



