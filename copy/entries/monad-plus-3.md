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
accumulation of moves to a solution?  We start out with a blank solution ("Do
nothing").  The next step, we add one move to our solution: "Just move the
fox", for example.  Then the next step, we add another move: "First move the
fox, then move the farmer."

1.  At the beginning, your move list is blank.  You start out as a tabula
    rasa.
2.  Then, you add a "possible and safe move" (move the farmer, for example)
3.  Then, you add another "possible and safe move" (move the fox, for
    example).  Repeat this all until your total move list is `n` moves long.
    "Safe" means that it doesn't involve anything eating anything.
4.  After `n` moves, only the paths that end on correct solutions are allowed
    to survive.

Seems simple, right?

Note one important thing about this approach is that you model things as a
"journey" of one individual possibility.  At no point do you ever deal
manually with any branching or filtering and at no points do you ever deal
with the set of all movelists as a whole.  You abstract everything as if you
were telling *one individual* story.

So let's get down to it already!

Our Types
---------

People often say that Haskell programs "write themselves".  To see this magic
happen, the first thing we always do is define our types.

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

type Solution = [Move]                          -- 4

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
4.  A simple type synonym --- a `Solution` is just a list of `Move`s.  Note
    that we are not using this list as a MonadPlus.
5.  For convenience, we define a `Position` type --- either on the west bank
    or on the east bank of the river.  Everyone starts out on the west bank,
    and we want them all to end up on the east bank.

Implementation
--------------

### The Final Step

Okay, so we're going to skip to the end and write our final step and what it
is supposed to be, and then fill in the functions that are necessary to make
it happen.

The last stage of our journey is after we have made all `n` moves, we end the
journey if it is not a solution.

~~~haskell
makeNMoves :: Int -> [Solution]     -- 1
isFinalSol :: Solution -> Bool

findSolutions :: Int -> [Solution]  -- 2
findSolutions n = do
    s <- makeNMoves n               -- 3
    guard $ isFinalSol s            -- 4
    return s                        -- 5
~~~

1.  The type signatures of the helper functions we will be using before.
2.  `findSolutions` is going to be the all succesful solutions after `n`
    moves.
3.  Let `s` be the result after making `n` moves
4.  End the journey unless `s` is a "final solution" (all characters are on
    the east side)
5.  Succeed with `s` if the journey has not yet ended.

Hm.  Sounds good!  We're done!

So now we only need to implement `makeNMoves` and `isFinalSol`!

### makeNMoves

`makeNMoves` is going to be the main logic of our program.  We want it to be
a journey, itself --- a journey of a single solution going through `n`
additions of moves.

That means we want something like:

~~~haskell
makeMove :: Solution -> [Solution]

startingSol :: Solution
startingSol = []

makeNMoves :: Int -> [Solution]
makeNMoves n = do
    m1 <- makeMove startingSol
    m2 <- makeMove m1
    m3 <- makeMove m2
    -- ... (n times)
    mn <- makeMove mx
    return mn
~~~

Of course we have seen that type of `do` block before, it is simply:

~~~haskell
makeNMoves :: Int -> [Solution]
makeNMoves n =
    makeMove startingSol >>= makeMove
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
makeNMoves :: Int -> [Solution]
makeNMoves n = iterate (>>= makeMove) (return startingSol) !! n
~~~

We say "apply `(>>= makeMove)` `n` times, starting the single starting
solution path".

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

Let's define our function `isFinalSol :: Solution -> Bool`.  Basically, we
want to check if the positions of all of the characters are `East`.

First, we need a way to get the position of a farmer/animal from a given
solution.

Our function `positionOf :: Solution -> Character -> Position` is going to
take a `Solution` and a `Character` to care about, and report what side of the
river the character is on.

Because every single move swaps the position of the farmer, the final position
of the farmer depends only on the parity of the number of total moves.  If it
is even, then the farmer is on the west bank still (consider 0 moves, two
moves, etc.).  if it is odd, then the farmer is on the east bank.

And because `Position` is an `Enum`, Haskell provides us a way to turn any
`Int` into a corresponding `Enum`: `toEnum :: Enum a => Int -> a`.  Let's see
it at work:

~~~haskell
λ: toEnum 0 :: Position
West
λ: toEnum 1 :: Position
East
~~~

Okay, so `West` is the 0th position, and `East` is the 1st position.

Knowing that we can get the length of a solution with `length :: [a] -> Int`
and we can turn any odd or even number into 

~~~haskell
positionOf :: Solution -> Character -> Position
positionOf s c = case t of
    Farmer  -> countToPosition $ length s
    _       -> undefined
    where
        countToPosition n | even n      = West
                          | othherwise  = East
~~~

Now, what if we want to know about non-farmers?

Instead of finding the total number of moves, we only need to find the number
of moves involving that given animal.

Let's first filter the Solution `s` by moves involving the character `c`:

~~~haskell
filter (== Move c) s
~~~

This will return a new Solution `s`, but with only the moves involving the
character `c`.  We can then use the length of *that*.

~~~haskell
positionOf :: Solution -> Character -> Position
positionOf s c = case c of
    Farmer  -> countToPosition . length $ s
    c       -> countToPosition . length $ filter (== Move c) s
    where
        countToPosition n | even n      = West
                          | othherwise  = East
~~~

<aside>
    ###### Aside

What freak is `countToPosition . length $ s`?

Well, remember that in Haskell, the `(.)` operator represents function
composition.  `(f . g) x` is equvalient to `f (g x)`.  "Apply `g` first, then
apply `f`".  Also recall that you can think of `($)` as adding an implict
parentheses around both sides of it.  In that sense, `f . g $ x` is the same
as `(f . g) (x)`.

So, altogether, `countToPosition . length $ s` is the same as
`(countToPosition . length) s`, which says "first, find the length of `s`,
then turn that length into a position."

Also, `countToPosition . length $ filter (== Move c) s` can be interpreted as
`(countToPosition . length) (filter (== Move c) s)` --- find the length of the
filtered list, then turn that length into a position.  We use `($)` mostly
because we don't like writing parentheses everywhere when we don't have to.
</aside>

Does this actually work?  Let's try out some examples.

~~~haskell
λ: positionOf [Move Farmer, Move Goat, Move Farmer] Goat
East
λ: positionOf [Move Farmer, Move Goat, Move Farmer] Wolf
West
λ: positionOf [Move Farmer, Move Goat, Move Farmer] Farmer
East        -- remember, the farmer is moves every single move.
~~~
