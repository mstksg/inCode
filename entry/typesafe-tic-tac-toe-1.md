Type-safe Tic Tac Toe (Part 1)

===============================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/typesafe-tic-tac-toe-1.html)

One problem with adoption of dependent types in everyday programming, I think,
is that most examples out there are sort of small and self-contained. There
aren't *too* many larger-scale examples out there showing how dependent types
can permeate your whole program to make everything more robust and error-free.

So, this series will be implementing a type-safe *tic tac toe* game (a
medium-scale Haskell app) that can be played both on the console (using
Haskeline) and in the browser (using Miso), using some custom built AI. We will:

1.  Build up our core game engine, talking about what it really means to be type
    safe
2.  Use our type-safe engine to build type-safe controllers (AI, GUI)

This series will also be a mini-tutorial on the
*[decidable](https://hackage.haskell.org/package/decidable)* package that I just
recently released :) We will also be heavily using the
*[singletons](https://hackage.haskell.org/package/singletons)* library. I
strongly strongly recommend reading my [Introduction to
Singletons](https://blog.jle.im/entries/series/+introduction-to-singletons.html)
series (and doing the exercises), if you are new to the singletons library.
However, I will do my best to explain singletons concepts in brief as they come
up.

## Type-Safety

First off, we should ask the question: what does it mean to be type-safe?

?????

## The Specification

We're going to create a type that represents a *valid* game state. The goal is
to make a GADT where you can only construct values whose types represent *valid*
game states. If we have a value of this type, then we know that the game state
must be valid.

A good way to start with this is by thinking of *induction rules* for defining a
valid state.

We'll say that there are two parts of a game state:

1.  The current board
2.  The current player

and that there are two ways of "constructing" a valid state:

1.  The empty board with player X is a valid state.

2.  If we have:

    -   A valid state with board *b* and current player *p*
    -   The game is still in play
    -   We can add a valid move by player *p* to board *b*

    Then the result of this move represents a new valid board *b*, with swapped
    player *p*.

This is a denotative way to talk about what it means for a state to be valid.

Note that our "type safety" is only as strong as the specification we just
wrote. Type safety using dependent types isn't omnipotent, and it can't read
your mind. However, there is a nice assurance that as long as your
*specification* is right, your program will work as expected. And hey, it's a
step up from the untyped case, where you can have a specification wrong, but
implement it incorrectly. With "type-safety", you cut out one huge area where
bugs come from: the implementation.

Alright, let's do this!

## Valid State

First, we'll define the types we need to specify our state:

``` haskell
$(singletons [d|
  data Piece = PX | PO
    deriving (Eq, Ord)

  type Board = [[Maybe Piece]]
  |])
```

A `Piece` will also represent our player -- either `PX` or `PO`. Our `Board`
will be a list of lists of `Maybe Piece`. If the spot contains `Nothing`, the
spot is unplayed; if the spot is `Just p`, then it means the spot has been
played by `p`.

And some values and functions we need to talk about empty boards and state
transformations:

``` haskell
$(singletons [d|
  emptyBoard :: Board
  emptyBoard = [ [Nothing, Nothing, Nothing]
               , [Nothing, Nothing, Nothing]
               , [Nothing, Nothing, Nothing]
               ]

  altP :: Piece -> Piece
  altP PX = PO
  altP PO = PX
  |])
```

Let's just throw in a quick proof as a sanity check:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L54-L56

altP_cyclic :: Sing p -> AltP (AltP p) :~: p
altP_cyclic SPX = Refl @'PX
altP_cyclic SPO = Refl @'PO
```

With that in mind, we can write our valid state constructor. We'll do that with
two helper types that we will implement later. First, we'll use the
[decidable](https://hackage.haskell.org/package/decidable) library to talk about
the kind of a *type-level predicate*.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L58-L58

data InPlay :: Predicate Board
```

`InPlay` is a predicate that a given board is in-play; a value of type
`InPlay @@ b` is a witness or proof that a board is in play.

We also need to define a type for a valid update by a given player onto a given
board:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L73-L73

data Update :: Piece -> Board -> Board -> Type where
```

A value of type `Update p b1 b2` will represent a valid update to board `b1` by
player `p` to create a board `b2`.

And finally, our valid state constructor:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L60-L71

data GameState :: Piece -> Board -> Type where
    -- | The empty board is a valid state
    GSStart
        :: GameState 'PX EmptyBoard
    -- | We can also construct a valid game state if we have:
    GSUpdate
        :: forall p b1 b2. ()
        => InPlay          @@ b1     -- ^ a proof that b1 is in play
        -> Update    p        b1 b2  -- ^ a valid update
        -> GameState p        b1     -- ^ a proof that p, b1 are a valid state
        -- ---------------------------- then
        -> GameState (AltP p)    b2  -- ^ `AltP p`, b2 is a valid satte
```

And that's it --- a verified-correct representation of a game state, directly
transcribed from our plain-language denotative specification.

Now we just need to talk about `InPlay` and `Update`. In particular, we need:

1.  A definition of `Update`, and a way to turn user-input into a valid `Update`
    (or reject it if it isn't valid).
2.  A definition of `InPlay`, and a way to decide whether or not a given board
    `b` is `InPlay`. This is something that the appropriately named
    *[decidable](https://hackage.haskell.org/package/decidable)* library will
    help us with.

### Update

Let's go about what thinking about what defines a valid update. Remember, the
kind we wanted was:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L73-L73

data Update :: Piece -> Board -> Board -> Type where
```

An `Update p b1 b2` will be a valid update of `b1` by player `p` to produce
`b2`. So, we need to:

1.  Produce `b2` from `b1`
2.  Be sure that the move is valid --- namely, that it is placed in a clean spot
    so that it doesn't overwrite any previous moves.

Producing `b2` from `b1` is simple enough as a type family. In fact, we can just
use the *[lens-typelevel](https://hackage.haskell.org/package/lens-typelevel)*
library to update our nested list:

``` haskell
$(singletonsOnly [d|
  placeBoard :: N -> N -> Piece -> Board -> Board
  placeBoard i j p = set (ixList i . ixList j) (Just p)
  |])
```

This is just lenses --- `set l x` is a function that sets the field specified by
`l` to `x`. Here, we set the jth item of the ith list to be `Just p`. That means
we can now produce `b2` from `b1` -- it's just `PlaceBoard i j p b1`.

Here, `N` is the peano nat type (a lot of libraries define it, but it's also
defined as a uility in *lens-typelevel*). It's essentially `[()]` (which makes
it useful as an index type), or:

``` haskell
data N = Z | S N
```

A natural number is either zero, or the successor of another natural number.
`S (S Z)`, for example, would represent 2.

The trickier part is making sure that the spot at *(i, j)* isn't already taken.
For that, we'll introduce a common helper type to say *what* the piece at spot
*(i, j)* is:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L87-L87

data Coord :: (N, N) -> [[k]] -> k -> Type where
```

A `Coord '(i, j) xss x` is a data type that specifies that the jth item in the
ith list in `b` is `p`.

And we require `Update` to only be constructable if the spot at *(i, j)* is
`Nothing`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L73-L78

data Update :: Piece -> Board -> Board -> Type where
    MkUpdate
        :: forall i j p b. ()
        => Coord '(i, j) b 'Nothing         -- ^ If the item at (i, j) in b is Nothing
        -- ------------------------------------- then
        -> Update p b (PlaceBoard i j p b)  -- ^ Placing `Just p` at i, j is a valid update
```

`Update` is now defined so that, for `Update p b1 b2`, `b2` is the update via
placement of a piece `p` at some position in `b1`, where the placement does not
overwrite a previous piece. Note that our `MkUpdate` constructor only has four
"free" variables, `i`, `j`, `p`, and `b`. If we use `MkUpdate`, it means that
the "final board" is fully determined from only `i`, `j`, `p`, and `b`.

#### Coord

Now we need to define `Coord`. We're going to do that in terms of a simpler type
that is essentially the same for normal lists --- a type:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L80-L80

data Sel :: N -> [k] -> k -> Type where
```

A value of type `Sel n xs x` says that the nth item in `xs` is `x`.

We can define this type inductively, similar to the common
[`Index`](http://hackage.haskell.org/package/type-combinators-0.2.4.3/docs/Data-Type-Index.html)
data type. We can mention our induction rules:

1.  The first item in a list as at index 0 (`Z`)
2.  If an item is at index `n` in list `as`, then it is also at index `S n` in
    list `b ': as`.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L80-L85

data Sel :: N -> [k] -> k -> Type where
    -- | The first item in a list is at index ''Z'
    SelZ :: Sel 'Z (a ': as) a
    SelS :: Sel     n        as  a  -- ^ If item `a` is at index `n` in list `as`
         -- ---------------------------- then
         -> Sel ('S n) (b ': as) a  -- ^ Item `a` is at index `S n` in list `b : as`
```

For example, for the type-level list `'[10,5,2,8]`, we can make values:

``` haskell
SelZ             :: Sel         'Z   '[10,5,2,8] 10
SelS SelZ        :: Sel     ('S 'Z)  '[10,5,2,8] 5
SelS (SelS SelZ) :: Sel ('S ('S 'Z)) '[10,5,2,8] 2
```

etc.

We can then use this to define `Coord`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L87-L92

data Coord :: (N, N) -> [[k]] -> k -> Type where
    (:$:) :: forall i j rows row p. ()
          => Sel i rows row         -- ^ If the ith list in `rows` is `row`
          -> Sel j row  p           -- ^ And the jth item in `row` is `p`
          -- --------------------------- then
          -> Coord '(i, j) rows p   -- ^ The item at (i, j) is `p`
```

A `Coord '(i, j) rows piece` contains a selection into the ith list in `rows`,
to get `row`, and a selection into the jth item in `row`, to get `piece`.

### Trying it out

That's it! Let's see if we can generate some sensible `Update`s, and maybe even
play a sample game.

We'll start with the `EmptyBoard`, and let's add a piece by `PX` at the middle
spot, index (1,1). This means we want `SelS SelZ :$: SelS SelZ` (a `Coord` with
two indexes into spots 1 and 1) applied to `MkUpdate`. We'll use
*-XTypeApplications* to specify the type variables `p` and `b`:

``` haskell
ghci> :t MkUpdate @_ @_ @'PX @EmptyBoard (SelS SelZ :$: SelS SelZ)
Update
  'PX
  '[ '[ 'Nothing, 'Nothing , 'Nothing],
     '[ 'Nothing, 'Nothing , 'Nothing],
     '[ 'Nothing, 'Nothing , 'Nothing]
   ]
  '[ '[ 'Nothing, 'Nothing , 'Nothing],
     '[ 'Nothing, 'Just 'PX, 'Nothing],
     '[ 'Nothing, 'Nothing , 'Nothing]
  ]
```

Nice! This update produces exactly he board expected.

Let's see if we can see if this prevents us from creating an illegal board.
We'll take the result board and see if we can place a `PO` piece there:

``` haskell
ghci> let NewBoard = '[ '[ 'Nothing, 'Nothing , 'Nothing ]
                      , '[ 'Nothing, 'Just 'PX, 'Nothing ]
                      , '[ 'Nothing, 'Nothing , 'Nothing ]
                      ]
ghci> :k MkUpdate @_ @_ @'PO @NewBoard (SelS SelZ :$: SelS SelZ)
    • Couldn't match type ‘'Nothing’ with ‘'Just 'PX’
```

Right! That's because `SelS SelZ :&: SelS SellZ`, applied to `NewBoard`, gives
`Coord '('S 'Z, 'S 'Z) NewBoard ('Just 'PX)`. However, in order to be used with
`MkUpdate`, the final field has to be `'Nothing`, not `'Just 'PX`. So, type
error.

### Type-safe Play

At the end of this all, we finally have enough to write a truly type-safe `play`
function that allows us to play a round of our game!

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L94-L100

play
    :: forall i j p b. ()
    => InPlay @@ b
    -> Coord '(i, j) b 'Nothing
    -> GameState p b
    -> GameState (AltP p) (PlaceBoard i j p b)
play r c = GSUpdate r (MkUpdate c)
```

`play` is basically the entirety of our game engine! (Minus defining `InPlay`,
which we will take care of later). It'll take our new move and a proof that the
game is still in play, and return a updated new game state. Our entire game is
done, and type-safe! It's impossible to play a game in an incorrect way! (once
we define `InPlay`).

Let's try out a few rounds in ghci, using `undefined` instead of a proper
`InPlay` for now:

``` haskell
ghci> g1 = play undefined (SelS SelZ :$: SelS SelZ) GSStart   -- X plays (1,1)
ghci> :t g1
GameState 'PO
    '[ '[ 'Nothing, 'Nothing , 'Nothing]
     , '[ 'Nothing, 'Just 'PX, 'Nothing]
     , '[ 'Nothing, 'Nothing , 'Nothing]
     ]

ghci> g2 = play undefined (SelZ :$: SelS SelZ) g1   -- O plays (0,1)
ghci> :t g2
GameState 'PX
    '[ '[ 'Nothing, 'Just 'PO, 'Nothing]
     , '[ 'Nothing, 'Just 'PX, 'Nothing]
     , '[ 'Nothing, 'Nothing , 'Nothing]
     ]

ghci> g3 = play undefined (SelZ :$: SelS SelZ) g2   -- X plays (1,0)
ghci> :t g3
GameState 'PO
    '[ '[ 'Nothing , 'Just 'PO, 'Nothing]
     , '[ 'Just 'PX, 'Just 'PX, 'Nothing]
     , '[ 'Nothing , 'Nothing , 'Nothing]
     ]

ghci> g4 = play undefined (SelS SelZ :$: SelS SelZ) g3   -- O plays (1,1)
    • Couldn't match type ‘'Just 'PX’ with ‘'Nothing’

ghci> g4 = play undefined (SelS (SelS (SelS SelZ)) :$: SelZ) g3  -- O plays (3,0)
    • Couldn't match type ‘'[]’ with ‘'Nothing ': as’
```

`play` enforces:

1.  Turns are always alternating X, then O
2.  We cannot place a piece in a previously-played spot
3.  We cannot place a piece out-of-bounds.

Note that the usage of `undefined` in place of a true witness for `InPlay` is a
nice tool for *incremental* and *interactive* development using dependent types.
A lot of people have the false impression that dependently typed programs are
difficult to program incrementally or interactively, but this example shows a
good way of going about programming in an incremental process. We just know that
our program is complete when we are finally able to get rid of all the
`undefined`s!

## Decision Functions and Views

This seems nice, but we're forgetting an important part. `play` requires us to
only give valid inputs, and enforces that the inputs are valid. However, how do
we *create* valid inputs, in a way that satisfies `play`?

As we'll see, this is one of the core problems that dependently typed
programming gives us tools to solve.

At this point, we've reached the important part of any "type-safe" application:
*decision functions* and dependent *views*. *Decision functions* let you slowly
refine your more general values (types) into more specific valid types. *Views*
let you sort out your our values into more "useful" perspectives.

We're going to allow for users to pick to move at any natural number pair
(`(N, N)`), but only *some* of those natural numbers can become valid updates.
In particular, we only allow an `Update` to be made if `(N, N)` represent valid
updates.

What are two ways this can go wrong? Well, if we allow the user to enter any two
natural numbers, here are all of the potential outcomes:

1.  We might get a coordinate that is out of bounds in x
2.  We might get a coordinate that is in bounds in x, but out of bounds in y
3.  We might get a coordinate that is in bounds in x, in bounds in y, but
    referencing a position that has already been played.
4.  We might get a coordinate that is in bounds in x, in bounds in y, and
    references a blank position. This is the only "success" case.

Note that we could also just have a "success or nor success" situation, but,
because we might want to provide feedback to the user, it is helpful to not be
"[decision-blind](https://twitter.com/cattheory/status/887760004622757890)" (a
cousin of [boolean
blindness](https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/)).

We'll call these potential "views" out of `(N, N)` with respect to some board
`b`. Let's create a data type representing all of these possibilities (using
`OutOfBounds` as a placeholder predicate for an out-of-bounds coordinate):

``` haskell
-- | Placeholder predicate if a given number `n` is out of bounds for a given
-- list.  Predicate is from the 'decidable' library
data OutOfBounds n :: Predicate [k]
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L108-L118

data Pick :: (N, N, Board) -> Type where
    -- | We are out of bounds in x
    PickOoBX   :: OutOfBounds i @@ b                         -> Pick '(i, j, b)
    -- | We are in-bounds in x, but out of bounds in y
    PickOoBY   :: Sel i b row        -> OutOfBounds j @@ row -> Pick '(i, j, b)
    -- | We are in-bounds in x, in-bounds in y, but spot is taken by `p`.
    -- We include `Sing p` in this constructor to potentially provide
    -- feedback to the user on what piece is already in the spot.
    PickPlayed :: Coord '(i, j) b ('Just p) -> Sing p        -> Pick '(i, j, b)
    -- | We are in-bounds in x, in-bounds in y, and spot is clear
    PickValid  :: Coord '(i, j) b 'Nothing                   -> Pick '(i, j, b)
```

(A value of type `OutOfBounds n @@ xs` is a witness that `xs` satisfies the
`OutOfBounds n` --- that is, `n` is out of bounds of `xs`. More on this later
when we talk about the *decidable* library!)

So, if we have an `(N, N, Board)`, we should be able to categorize it into one
of each of these potential views.

### Proving functions

This is the job of a "decision function"; in this case, actually, a "proving
function", or a "viewing function". We need to be able to write a function:

``` haskell
pick :: Sing '(i, j, b)
     -> Pick '(i, j, b)
```

That is, given any coordinate and board, we should be able to *totally*
categorize it to one of the four categories (think of them like four
perspectives/classifications), without exception.

This can be considered the boundary between the unsafe and the safe world. And,
to me, this is the "hard part" about dependently typed programming :)

Now, let's write `pick`. If we want to take any `i`, `j`, and `b`, and turn it
into a valid `Pick`, remember that a valid `Pick '(i, j, b)` contains a
`Coord '(i, j) b 'Nothing`, which contains a `Sel i b row` and a
`Sel j row 'Nothing`. So we need to "convert" some `i`, `j` into a `Sel i b row`
and `Sel j row 'Nothing`.

Essentially, we want a function:

``` haskell
sel :: Sing i
    -> Sing xs
    -> Sel i xs ??????
```

where `????` is whatever value is in `xs` at index `i`. It's something we might
not know directly from the input types, necessarily, because it might not even
exist (the list might be too short).

### Existential return types and Dependent Pairs

This pattern --- where we don't know the type of something until after we
receive the function inputs --- is something you might recognize as an
*existential type*, implementable using a *dependent pair* (or dependent sum).

We could write our own dependent pair from scratch, but this is a good
opportunity to practice using *singletons* library's versatile "anonymous
dependent pair" type, `Σ` (or `Sigma`), from *Data.Singletons.Sigma*.

``` haskell
data Sigma k :: Predicate k -> Type where
    (:&:) :: Sing x -> (p @@ x) -> Sigma k f

type Σ k = Sigma k
```

A value of type `Sigma k p` contains an `p @@ x` (a witness that `p` satisfies
`x`), existentially *hiding* the `x :: k`, and also `x` itself (as `Sing x`;
remember that `Sing x` is essentially a value-level representation of type `x`).

You can think of `Sigma k p` as a proof that `p` is satisfied for *some*
`x :: k`, but we can't know which `x` it is until you pattern match. If you
pattern match, you'll get both `Sing x` (to find out the `x`), and the `p @@ x`
(the witness that `p` is satisfied by `x`).

We can use this to return a `Sel n xs ????`, *hiding* the `???`.

Now for some plumbing: We can't directly give `Sel n xs` to `Sigma` (because it
expects a `Predicate k`, not a `k -> Type`), but we can turn a type constructor
into a `Predicate` using `TyPred`, a convenient combinator from the *decidable*
library:

``` haskell
TyPred :: (k -> Type) -> Predicate k
```

``` haskell
ghci> :k Pick
(N, N, Board) -> Type

ghci> :k TyPred Pick
Predicate (N, N, Board)

ghci> :k Sel 'Z EmptyBoard
[Maybe Piece] -> Type

ghci> :k TyPred (Sel 'Z EmptyBoard)
Predicate [Maybe Piece]
```

Let's make sure this type works like we expect it to. We want a
`Σ k (TyPred (Sel n xs))` to contain the `x` at position `n` in `xs`, *and* the
`Sel` into that position.

To make things a little less verbose, we can define a type synonym for
`Σ k (TyPred (Sel n xs))`, `SelFound n xs`:

``` haskell
type SelFound n (xs :: [k]) = Σ k (TyPred (Sel n xs))
```

Or, as practice, maybe we can treat `SelFound n` as a predicate on a list? The
predicate `SelFound n` will be satisfied if list `xs` has some item `x` at index
`n`!

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L102-L103

data SelFound :: N -> Predicate [k]
type instance Apply (SelFound n) (xs :: [k]) = Σ k (TyPred (Sel n xs))
```

We can check to make sure this works, by checking the type of witnesses of
`SelFound 'Z @@ '[ 'True, 'False ]`:

``` haskell
ghci> :kind! SelFound 'Z @@ '[ 'True, 'False ]
Σ Bool (TyPred Sel ('Z '[ 'True, 'False ]))
```

Now let's make some sample witnesses of predicate `SelFound n` to ensure we are
thinking about things correctly:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L165-L167

selFoundTest1 :: SelFound 'Z @@ '[ 'True, 'False ]
selFoundTest1 = STrue :&: SelZ
                       -- ^ Sel 'Z '[ 'True, 'False ] 'True
```

Note that `SFalse :&: SelZ` would be a type error, because the second half
`SelZ` would be `Sel :: 'Z '[ 'True, 'False ] 'True` (because `'True` is the 0th
item in the list), so we have to have the first half match `'True`, with
`STrue`.

We can write a witness for `SelFound ('S 'Z) @@ '[ 'True, 'False ]`, as well, by
giving the value of the list at index 1, `'False`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L169-L171

selFoundTest2 :: SelFound ('S 'Z) @@ '[ 'True, 'False ]
selFoundTest2 = SFalse :&: SelS SelZ
                        -- ^ Sel ('S 'Z) '[ 'True, 'False ] 'False
```

Before moving on, I strongly recommend trying to write some values of type
`SelFound n @@ xs` for different `n :: N` and `xs :: [a]`, to see what
type-checks and what doesn't. It'll help you get a feel of the types we are
working with, which might be more advanced than types you might encounter in
everyday Haskell programming. Remember that you can load up all of the
definitions in this post into a ghci session by downloading [the source
code](https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs)
and executing it on the command line, `./Part1.hs`.

Now, we now have enough tools to write the type of the function we would like:

``` haskell
selFound
    :: Sing n
    -> Sing xs
    -> SelFound n @@ xs
```

Remember, a `SelFound n @@ xs` contains both a `Sel n xs x` *and* a `Sing x`: a
selection into the `i`th item in `xs` (the `Sel n xs x`), and also the item
itself (the `Sing x`).

We can start writing this, but the type system will soon show you where you run
into problems. And that's one of the best things about type systems! They help
you realize when you're trying to do something that doesn't make sense.

``` haskell
selFound
    :: Sing n
    -> Sing xs
    -> SelFound n @@ xs
selFound = \case
    SZ -> \case
      SNil -> _ :&: _
```

Things start out pretty standard. We want to match on all potential constructors
of `N` and `[a]`: `N` has `Z` and `S`, so we match on singleton constructors
`SZ` and `SS`; `[a]` has `[]` and `(:)`, so we match on singleton constructors
`SNil` and `SCons`.

If you ask ghc what goes in the typed holes, it'll say that we need a `Sing x`
and a `Sel 'Z '[] x` (which is because matching on `SZ` tells us we are in `'Z`,
and matching on `SNil` tells us we are in `'[]`). And this...is a problem.

Remember that the `x` is supposed to be the `n`th item in `xs`. Here, in this
pattern match branch, we want the zeroth (first) item in `[]`. This doesn't
exist! That's because there is no item in `[]`, so there is nothing we can put
for the `Sing x`.

There's also nothing we could put for the `Sel` (the right hand side of `:&:`),
since there is no constructor of `Sel` that returns a `Sel n '[]` (the
constructors of `Sel` all return `x ': xs`, never `Nil`).

So, this branch is impossible to fulfil. We know now that we made a large
conceptual error (aren't types great?)

The problem? Well, indexing into item `n` in list `xs` *might not always
succeed*. We might try to index into an empty list, so we can't ever get a
result!

### Decision Functions

What we need is not a *proving function*, but, rather, a *decision* function. A
decision function for a predicate `P` is a function:

``` haskell
decidePred :: Sing x
           -> Decision (P @@ x)
```

That is, instead of producing a `P @@ x` directly, we produce a
`Decision (P @@ x)`. Here, `Decision` is:

``` haskell
data Decision a
    = Proved     a                -- ^ `a` is provably true
    | Disproved (a -> Void)       -- ^ `a` is provably false

-- | The type with no constructors.  If we have a function `a -> Void`, it must
-- mean that no value of type `a` exists.
data Void
```

A decision function means that, for any `x`, we can say that either `P @@ x` can
be proven true or can be proven false. See [this
section](https://blog.jle.im/entry/introduction-to-singletons-3.html#decision)
for a deeper discussion on why `Decision` has both the `Proved` and `Disproved`
branch. Essentially, it prevents us from just returning "disproved" without
proving it (so we can be sure that our decision function is "correct" and not
just cheating), and, in the long term, we keep track of "provably false" because
we can use it later to build other useful decision functions and proving
functions.

We use decision functions when we want to "conditionally prove" something --- it
might be true, or it might not be (but definitely one or the other). It might
exist, or it might not. We can construct the view, or we can't. Whatever the
perspective, it's always one or the other.

`selFound` fits this category: for a `Sel n xs ????`, either `n` is "in bounds"
of `xs` (and we can prove this with the item `x` in `xs`), or `n` is "out of
bounds". Either we get the `x` out of `xs` at slot `n`, or we prove that no
possible `x` exists in `xs` at slot `n`.

#### Deciding SelFound

Enough talk, let's get to it!

Let's write our first dependently typed function. We start the same way --- by
looking at every possible constructor of `N` and `[a]`:

``` haskell
selFound
    :: Sing n
    -> Sing xs
    -> Decision (SelFound n @@ xs)
selFound = \case
    SZ -> \case
      SNil         -> _   -- n is 'Z, xs is '[]
      x `SCons` xs -> _   -- n is 'Z, xs is (x ': xs)
    SS n -> \case
      SNil         -> _   -- n is ('S n), xs is '[]
      x `SCons` xs -> _   -- n is ('S n), xs is (x ': xs)
```

Okay, four cases. Initially daunting, but we can just handle this one by one.
Normally we can just fill in the blanks with the "right" responses, but, for
learning's sake, let's split these branches into four helper functions --- one
for each case.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L120-L155

selFound
    :: Sing n
    -> Sing xs
    -> Decision (SelFound n @@ xs)
selFound = \case
    SZ -> \case
      SNil         -> selFound_znil
      x `SCons` xs -> selFound_zcons x xs
    SS n -> \case
      SNil         -> selFound_snil n
      x `SCons` xs -> selFound_scons n x xs

selFound_znil
    :: Decision (SelFound 'Z @@ '[])

selFound_zcons
    :: Sing x
    -> Sing xs
    -> Decision (SelFound 'Z @@ (x ': xs))

selFound_snil
    :: Sing n
    -> Decision (SelFound ('S n) @@ '[])

selFound_scons
    :: Sing n
    -> Sing x
    -> Sing xs
    -> Decision (SelFound ('S n) @@ (x ': xs))
```

1.  For the first branch, we have `'Z` and `'[]`. This should be false, because
    there is no item in the zeroth position in `[]`. But, also, there is no way
    to construct the `Sel` necessary for the witness, since there is no
    constructor for `Sel` that gives `'[]`.

    We can witness this by using a *total* helper function,
    `noEmptySel :: Sel n '[] a -> Void`, which is a "disproof" of the fact that
    an empty `Sel` can exist. It's a disproof because, if we *had* such a `Sel`,
    we could produce `Void` with it...but `Void` has no constructors. So no such
    `Sel` must exist!

    We implement it by pattern matching on all potential patterns (using the
    *-XLambdaCase* extension):

    ``` haskell
    -- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L132-L134

    noEmptySel :: Sel n '[] a -> Void
    noEmptySel = \case {}
                -- ^ we handle all 0 of the valid patterns for Sel n '[] a
    ```

    `noEmptySel` successfully implements `Sel n '[] as` by successfully matching
    on every legal constructor that could produce `Sel n '[] as`. But, because
    there are no constructors for `Sel` that produce `Sel n '[] as` (we just
    have `SelZ` and `SelS`, which both produce non-empty `Sel`s), that means we
    have to handle all *zero* legal constructors. Once we handle all zero legal
    constructors, we're done! (Remember to enable *-Werror=incomplete-patterns*
    to be sure! GHC will then reject the program if there is a pattern we do not
    handle)

    So we can write this as `Disproved`, which takes a
    `SelFound 'Z @@ '[] -> Void`:

    ``` haskell
    -- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L136-L138

    selFound_znil
        :: Decision (SelFound 'Z @@ '[])
    selFound_znil = Disproved \(_ :&: s) -> noEmptySel s
    ```

    Armed with the `Sel 'Z '[] as` that is inside the `SelFound 'Z @@ '[]`, we
    can use `noEmptySel` to produce the `Void`. We successfully disprove the
    fact that there is any item that can be found in `'[]`, by providing a
    function `SelFound 'Z @@ '[] -> Void`.

    Note that with the *-XBlockArguments* extension, we don't need the `$` after
    `Disproved`.

2.  For the second branch, we have `'Z` and `(x ': xs)`. We want to prove that
    there exists an item at position `'Z` in the list `x ': xs`. The answer is
    *yes*, there does, and that item is `x`, and the `Sel` is `SelZ`!

    ``` haskell
    -- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L140-L144

    selFound_zcons
        :: Sing x
        -> Sing xs
        -> Decision (SelFound 'Z @@ (x ': xs))
    selFound_zcons x _ = Proved (x :&: SelZ)
    ```

3.  For the third branch, we have `'S n` and `'[]`. Again, this should be false,
    because there is no item in the `'S n` position in `'[]`. We should be able
    to use the same strategy for the first branch, by re-using our helper
    function `noEmptySel`:

    ``` haskell
    -- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L146-L149

    selFound_snil
        :: Sing n
        -> Decision (SelFound ('S n) @@ '[])
    selFound_snil _ = Disproved \(_ :&: s) -> noEmptySel s
    ```

4.  The fourth branch is the most interesting one. We have `'S n` and
    `(x ':     xs)`. How do we know if the list `x ': xs` has an item in the
    `'S n` spot?

    Well, we can check if the list `xs` has an item in its `n` spot.

    -   If it does, then call that item `y`, and we know that `x ': xs` has `y`
        in its `'S n` spot.

    -   If it doesn't, then we can't have an item at `'S n` spot in `x ': xs`
        either! To show why, we can do a proof by contradiction.

        Suppose there *was* an item `y` at the `'S n` spot in `x ': xs`. If so,
        then that means that there would be an item `y` in the `n` spot in `xs`.
        However, this was found to be false. Therefore, we cannot have an item
        in the `'S n` spot in `x ': xs`.

        This is a situation where having a disproof in the `Disproved` branch is
        useful --- we use them to build more complex disproofs from simple ones.

    ``` haskell
    -- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L151-L163

    selFound_scons
        :: Sing n
        -> Sing x
        -> Sing xs
        -> Decision (SelFound ('S n) @@ (x ': xs))
    selFound_scons n _ xs = case selFound n xs of
        Proved (y :&: s) ->       -- if xs has y in its n spot
          Proved (y :&: SelS s)   -- then (x : xs) has y in its (S n) spot
        Disproved v      -> Disproved -- v is a disproof that an item is in n spot in xs
          \(y :&: s) ->      -- suppose we had item y in (S n) spot in (x : xs)
            case s of
              SelS s' ->     -- this would mean that item 'y' is in 'n' spot in xs
                v (y :&: s') -- however, v disproves this.
    ```

    Note again the usage of *-XBlockArguments*, allowing us to not need the `$`
    after `Disproved`.

    If you have problems understanding this, try playing around with typed holes
    in GHC, or trying to guess what types everything has in the implementation
    above, until you can figure out what is happening when.

### Proving Pick

Now that we can decide `SelFound`, let's finally prove `Pick`.

``` haskell
pick
    :: Sing i
    -> Sing j
    -> Sing b
    -> Pick '(i, j, b)
pick i j b =
```

Remember, the goal is to try to prove we have a valid pick. We want to create
something with the `PickValid` constructor if we can:

``` haskell
PickValid  :: Coord '(i, j) b 'Nothing -> Pick '(i, j, b)

(:$:) :: Sel i rows row
      -> Sel j row  p
      -> Coord '(i, j) rows p
```

So we need a `Coord '(i, j) b 'Nothing`, which means we need a `Sel i b row` and
a `Sel j row 'Nothing`. Let's use our decision functions we wrote to get these!
In particular, we can use `selFound i b` to get our `Sel i b row` and `row`, and
then use `selFound j row` to get our `Sel j row piece` and `piece`!

Note that we also might need to handle the cases where the picks are
out-of-bounds, which requires an `OuOfBounds n` predicate.

``` haskell
PickOoBX   :: OutOfBounds i @@ b -> Pick '(i, j, b)
```

We never defined `OutOfBounds n`, so let's do it now. We know that `SelFound n`
is the predicate where `SelFound n @@ xs` is satisfied when index `n` is "in
bounds" of `xs`. So, `OutOfBounds n` is the predicate that we *can't* produce
`SelFound n`; a witness of `OutOfBounds n @@ xs` is a proof that we can't make
`SelFound n @@ xs`:

``` haskell
data OutOfBounds :: N -> Predicate [k]
type instance Apply (OutOfBounds n) xs = SelFound n @@ xs -> Void
```

Remember that `a -> Void` is a witness that `a` cannot exist, since if it could
exist, we could create a value of type `Void`, but no such value exists. So if
we had a `SelFound n @@ xs -> Void`, it means that no such `SelFound n @@ xs`
could exist. It means that `n` must *not* be in bounds of `xs`.

Actually, we could take advantage of some of the combinators that the
*decidable* library provides to define `OutOfBounds` in a cleaner way:

``` haskell
-- | Provided by decidable; it's the negation of a predicate
data Not :: Predicate k -> Predicate k
type instance Apply (Not p) x = p @@ x -> Void
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L105-L105

type OutOfBounds n = Not (SelFound n)
```

`Not` is a *predicate combinator*; it takes a predicate and returns new
predicate. `Not p @@ x` is true whenever `p @@ x` is false, or `p @@ x -> Void`
is inhabited. Note that combinators like `Not` are one of the reasons why it's
useful to think of predicates in terms of defunctionalization symbols
(`k ~> Type`), instead of as type families or type constructor: `Not` expects a
"partially applied" predicate (`Not` takes `p`, not `p @@ x`).

Alright, now that everything is defined, let's start writing our viewing
function for `Pick`. Recall again the definition of `Pick`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L108-L118

data Pick :: (N, N, Board) -> Type where
    -- | We are out of bounds in x
    PickOoBX   :: OutOfBounds i @@ b                         -> Pick '(i, j, b)
    -- | We are in-bounds in x, but out of bounds in y
    PickOoBY   :: Sel i b row        -> OutOfBounds j @@ row -> Pick '(i, j, b)
    -- | We are in-bounds in x, in-bounds in y, but spot is taken by `p`.
    -- We include `Sing p` in this constructor to potentially provide
    -- feedback to the user on what piece is already in the spot.
    PickPlayed :: Coord '(i, j) b ('Just p) -> Sing p        -> Pick '(i, j, b)
    -- | We are in-bounds in x, in-bounds in y, and spot is clear
    PickValid  :: Coord '(i, j) b 'Nothing                   -> Pick '(i, j, b)
```

And let's start writing. First, we'll use our decision functions `selFound` to
*try* to produce the `Sel`s necessary for the `PickValid` branch:

``` haskell
pick
    :: Sing i
    -> Sing j
    -> Sing b
    -> Pick '(i, j, b)
pick i j b = case selFound i b of
    Proved (row :&: selX) -> case selFound j row of
      Proved (p :&: selY) ->
        let c = selX :$: selY
        in  ????
```

In the case where both `i` and `j` are in-bounds, what do we do? Well, we can
create the `Coord` from the `selX` and `selY`. Can't we just give this to
`PickValid`?

``` haskell
pick
    :: Sing i
    -> Sing j
    -> Sing b
    -> Pick '(i, j, b)
pick i j b = case selFound i b of
    Proved (row :&: selX) -> case selFound j row of
      Proved (p :&: selY) ->
        let c = selX :$: selY
        in  PickValid c
```

Ah, but this is a compiler error. It's hard to see why without a trusty compiler
to help us. But that's the best part about using dependent types --- the
compilers are here to help us write our programs!

The compiler basically tells us that `c` is supposed to point to
`'Nothing`...but right now it's pointing to some type variable that we don't
know is `'Nothing` or `'Just`. So `c` doesn't *necessarily* work, because it
*might* be pointing to `'Just`, and not `'Nothing`. We haven't satisfied
`PickValid` yet, because we have to be certain that it is `'Nothing` and not
`'Just`

Just to clarify what's going on, let's give types to the names above:

``` haskell
b    :: Sing (b   :: [[Maybe Piece]])
row  :: Sing (row ::  [Maybe Piece] )
selX :: Sel i b row
p    :: Sing (p   ::   Maybe Piece  )
selY :: Sel j row p
c    :: Coord '(i, j) b p
```

`row` above is the `Sing` that comes attached with all `Σ` constructors, which
is why we can give it to `selFound j`, which expects a singleton of the list.

So, now we have `Coord '(i, j) b p`. We know that `i` and `j` are in-bounds.
But, we need to know that `p` is `'Nothing` before we can use it with
`PickValid`. To do that, we can pattern match on `p`, because it's the singleton
that comes with the `Σ` constructor:

``` haskell
pick
    :: Sing i
    -> Sing j
    -> Sing b
    -> Pick '(i, j, b)
pick i j b = case selFound i b of
    Proved (row :&: selX) -> case selFound j row of
      Proved (p :&: selY) ->
        let c = selX :$: selY
        in  case p of
              SNothing -> PickValid   c     -- p is 'Nothing
              SJust q  -> PickPlayed  c q   -- p is 'Just q
```

Finally, knowing that `p` is `'Nothing`, we can create `PickValid`!

As a bonus, if we know that `p` is `'Just p`, we can create `PickPlayed`, which
is the constructor for an in-bounds pick but pointing to a spot that is already
occupied by piece `p'`.

``` haskell
PickPlayed :: Coord '(i, j) b ('Just p)
           -> Sing p
           -> Pick '(i, j, b)
```

We now have to deal with the situations where things are out of bounds.

``` haskell
PickOoBX :: OutOfBounds i @@ b
         -> Pick '(i, j, b)
PickOoBY :: Sel i b row
         -> OutOfBounds j @@ row
         -> Pick '(i, j, b)
```

`PickOoBX` requires an `OutOfBounds i @@ b`, which we defined as
`SelFound i @@ b -> Void`. Well, that's *exactly* what the `Disproved`
constructor contains, that `selFound i b` returns! And `PickOoBY` requires an
`OutOfBounds j @@ row`, which we defined as `SelFound j @@ row -> Void`. And
that's *exactly* what the `Disproved` constructor of `selFound j row` returns.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L173-L190

pick
    :: Sing i
    -> Sing j
    -> Sing b
    -> Pick '(i, j, b)
pick i j b = case selFound i b of
    Proved (row :&: selX) -> case selFound j row of
      Proved (p :&: selY) ->
        let c = selX :$: selY
        in  case p of
              SNothing -> PickValid   c     -- p is 'Nothing
              SJust q  -> PickPlayed  c q   -- p is 'Just q
      Disproved vY -> PickOoBY selX vY    -- vY :: SelFound j @@ row -> Void
                                          -- vY :: Not (SelFound j) @@ row
                                          -- vY :: OutOfBounds j @@ row
    Disproved vX -> PickOoBX vX   -- vX :: SelFound i @@ b   -> Void
                                  -- vX :: Not (SelFound i) @@ b
                                  -- vX :: OutOfBounds i @@ b
```

And that's it!

## Play Ball

Bringing it all together, we can write a simple function to take user input and
*play* it.

First, some utility functions to get user input and print out boards:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L192-L211

intToN :: Int -> Maybe N
intToN n = case compare n 0 of
    LT -> Nothing
    EQ -> Just Z
    GT -> S <$> intToN (n - 1)

getN :: String -> IO N
getN prompt = do
    putStrLn $ "Enter non-negative integer for " ++ prompt ++ ":"
    res <- getLine
    case intToN =<< readMaybe res of
      Nothing -> putStrLn "Bad." >> getN prompt
      Just n  -> pure n

printBoard :: Board -> IO ()
printBoard = mapM_ $ putStrLn . intercalate "|" . map showPiece
  where
    showPiece Nothing   = " _ "
    showPiece (Just PX) = " X "
    showPiece (Just PO) = " O "
```

And here is the logic for getting user input, viewing it using `pick`, and
updating the `GameState`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L218-L242

simplePlayIO'
    :: Sing p
    -> Sing b
    -> GameState p b
    -> IO ()
simplePlayIO' p b gs = do
    printBoard $ FromSing b
    FromSing i <- getN "row"
    FromSing j <- getN "column"
    case pick i j b of
      PickOoBX _ -> do
        putStrLn "Out of bounds in rows.  Try again."
        simplePlayIO' p b gs
      PickOoBY _ _ -> do
        putStrLn "Out of bounds in cols.  Try again."
        simplePlayIO' p b gs
      PickPlayed _ q -> do
        putStrLn $ "Already played by " ++ show (fromSing q) ++ ". Try again."
        simplePlayIO' p b gs
      PickValid c -> do
        putStrLn "Success!"
        let p'  = sAltP p                 -- update player (enforced by `play`)
            b'  = sPlaceBoard i j p b     -- update board  (enforced by `play`)
            gs' = play undefined c gs     -- update game state
        simplePlayIO' p' b' gs'
```

We use the `FromSing :: Sing (x :: a) -> a` pattern synonym here to jump between
the value-level and type-level with our values. First we use it as a
`Sing (b :: Board) -> Board` to give us the `Board` that `printBoard` demands.
Then we use it as a constructor to "get" a `Sing (i :: N)` from the value-level
`N` that `getN` returns. If `FromSing x :: a`, then `x` is the singleton of
`FromSing x`. That is, `True == FromSing STrue`, and `S Z == FromSing (SS SZ)`.

Note that the type of `play` enforces that we modify `p` and `b` according to
nothing other than exactly what the type of a new board game demands.

And to start it off, we give `simplePlayIO'` an initial state:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Part1.hs#L213-L216

simplePlayIO :: IO ()
simplePlayIO = simplePlayIO' SPX sEmptyBoard GSStart
-- alternatively
-- simplePlayIO = simplePlayIO' sing sing GSStart
```

This isn't too bad! A type-safe tic-tac-toe that enforces that:

1.  Players alternate
2.  You can't place a piece not on the board
3.  You can't place a piece over an existing piece

```{=html}
<!-- -->
```
    ghci> simplePlayIO
     _ | _ | _
     _ | _ | _
     _ | _ | _
    Enter non-negative integer for row:
    10
    Enter non-negative integer for column:
    100
    Out of bounds in rows.  Try again.
     _ | _ | _
     _ | _ | _
     _ | _ | _
    Enter non-negative integer for row:
    0
    Enter non-negative integer for column:
    0
    Success!
     X | _ | _
     _ | _ | _
     _ | _ | _
    Enter non-negative integer for row:
    1
    Enter non-negative integer for column:
    1
    Success!
     X | _ | _
     _ | O | _
     _ | _ | _
    Enter non-negative integer for row:
    1
    Enter non-negative integer for column:
    1
    Already played by PO. Try again.
     X | _ | _
     _ | O | _
     _ | _ | _
    Enter non-negative integer for row:
    ^C

(Again, note that `undefined` is used here instead of an actual witness for
`InPlay` as a nice tool to enable incremental and interactive development of
dependently typed programs.)

Our core engine is pretty much complete, except that we haven't defined `InPlay`
yet, so the game can still go on *after* it has already been won. So, next,
let's implement our `InPlay` predicate and finish everything up!

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

