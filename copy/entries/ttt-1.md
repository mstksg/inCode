---
title: "Type-safe Tic Tac Toe (Part 1)"
categories: Haskell
series: Type-safe Tic Tac Toe
tags: functional programming, dependent types, haskell, singletons, types, decidable
create-time: 2018/10/29 16:48:33
identifier: ttt-1
slug: typesafe-tic-tac-toe-1
---

One problem with adoption of dependent types in everyday programming, I think,
is that most examples out there are sort of small and self-contained.  There
aren't *too* many larger-scale examples out there showing how dependent types
can permeate your whole program to make everything more robust and error-free.

So, this series will be implementing a type-safe *tic tac toe* game (a
medium-scale Haskell app) that can be played both on the console (using
Haskeline) and in the browser (using Miso), using some custom built AI.  We
will:

1.  Build up our core game engine, talking about what it really means to be
    type safe
2.  Use our type-safe engine to build type-safe controllers (AI, GUI)

This series will also be a mini-tutorial on the *[decidable][]* package that I
just recently released :)  We will also be heavily using the *[singletons][]*
library.  Where relevant, I will explain singletons concepts in brief.  If you
want a more in-depth introduction to the *singletons* library, however, check
out my [Introduction to Singletons][] series!

[decidable]: https://hackage.haskell.org/package/decidable
[singletons]: https://hackage.haskell.org/package/singletons
[Introduction to Singletons]: https://blog.jle.im/entries/series/+introduction-to-singletons.html

Type-Safety
-----------

First off, we should ask the question: what does it mean to be type-safe?

?????

The Specification
-----------------

We're going to create a type that represents a *valid* game state.  The goal is
to make a GADT where you can only construct values whose types represent
*valid* game states.  If we have a value of this type, then we know that the
game state must be valid.

A good way to start with this is by thinking of *induction rules* for defining
a valid state.

We'll say that there are two parts of a game state:

1.  The current board
2.  The current player

and that there are two ways of "constructing" a valid state:

1.  The empty board with player X is a valid state.
2.  If we have:

    *   A valid state with board *b* and current player *p*
    *   The game is still in play
    *   We can add a valid move by player *p* to board *b*

    Then the result of this move represents a new valid board *b*, with swapped
    player *p*.

This is a denotative way to talk about what it means for a state to be valid.

Note that our "type safety" is only as strong as the specification we just
wrote.  Type safety using dependent types isn't omnipotent, and it can't read
your mind.  However, there is a nice assurance that as long as your
*specification* is right, your program will work as expected.  And hey, it's a
step up from the untyped case, where you can have a specification wrong, but
implement it incorrectly.  With "type-safety", you cut out one huge area where
bugs come from: the implementation.

Alright, let's do this!

Valid State
-----------

First, we'll define the types we need to specify our state:

```haskell
$(singletons [d|
  data Piece = PX | PO
    deriving (Eq, Ord)
  
  type Board = [[Maybe Piece]]
  |])
```

A `Piece` will also represent our player -- either `PX` or `PO`.  Our `Board`
will be a list of lists of `Maybe Piece`.  If the spot contains `Nothing`, the
spot is unplayed; if the spot is `Just p`, then it means the spot has been
played by `p`.

And some values and functions we need to talk about empty boards and state
transformations:

```haskell
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

```haskell
!!!ttt/Part1.hs "altP_cyclic"
```

With that in mind, we can write our valid state constructor.  We'll do that
with two helper types that we will implement later.  First, we'll use the
[decidable][] library to talk about the kind of a *type-level predicate*.

```haskell
!!!ttt/Part1.hs "data InPlay"1
```

`InPlay` is a predicate that a given board is in-play; a value of type `InPlay
@@ b` is a witness or proof that a board is in play.

We also need to define a type for a valid update by a given player onto a given
board:

```haskell
!!!ttt/Part1.hs "data Update"1
```

A value of type `Update p b1 b2` will represent a valid update to board `b1` by
player `p` to create a board `b2`.

And finally, our valid state constructor:

```haskell
!!!ttt/Part1.hs "data GameState"
```

And that's it --- a verified-correct representation of a game state, directly
transcribed from our plain-language denotative specification.

Now we just need to talk about `InPlay` and `Update`.  In particular, we need:

1.  A definition of `Update`, and a way to turn user-input into a valid
    `Update` (or reject it if it isn't valid).
2.  A definition of `InPlay`, and a way to decide whether or not a given board
    `b` is `InPlay`.  This is something that the appropriately named
    *[decidable][]* library will help us with.

Update
------

Let's go about what thinking about what defines a valid update.   Remember, the
kind we wanted was:

```haskell
!!!ttt/Part1.hs "data Update"1
```

An `Update p b1 b2` will be a valid update of `b1` by player `p` to produce
`b2`.  So, we need to:

1.  Produce `b2` from `b1`
2.  Be sure that the move is valid --- namely, that it is placed in a clean
    spot so that it doesn't overwrite any previous moves.

Producing `b2` from `b1` is simple enough as a type family.  In fact, we can
just use the *[lens-typelevel][]* library to update our nested list:

[lens-typelevel]: https://hackage.haskell.org/package/lens-typelevel

```haskell
$(singletonsOnly [d|
  placeBoard :: N -> N -> Piece -> Board -> Board
  placeBoard i j p = set (ixList i . ixList j) (Just p)
  |])
```

This is just lenses --- `set l x` is a function that sets the field specified
by `l` to `x`.  Here, we set the jth item of the ith list to be `Just p`.  That
means we can now produce `b2` from `b1` -- it's just `PlaceBoard i j p b1`.

Here, `N` is the peano nat type (a lot of libraries define it, but it's also
defined as a uility in *lens-typelevel*). It's essentially `[()]` (which makes
it useful as an index type), or:

```haskell
data N = Z | S N
```

A natural number is either zero, or the successor of another natural number. `S
(S Z)`, for example, would represent 2.

The trickier part is making sure that the spot at *(i, j)* isn't already taken.
For that, we'll introduce a common helper type to say *what* the piece at spot
*(i, j)* is:

```haskell
!!!ttt/Part1.hs "data Coord"1
````

A `Coord '(i, j) xss x` is a data type that specifies that the jth item in the
ith list in `b` is `p`.

And we require `Update` to only be constructable if the spot at *(i, j)* is
`Nothing`:

```haskell
!!!ttt/Part1.hs "data Update"
```

`Update` is now defined so that, for `Update p b1 b2`, `b2` is the update via
placement of a piece `p` at some position in `b1`, where the placement does not
overwrite a previous piece.  Note that our `MkUpdate` constructor only has four
"free" variables, `i`, `j`, `p`, and `b`.  If we use `MkUpdate`, it means that
the "final board" is fully determined from only `i`, `j`, `p`, and `b`.

### Coord

Now we need to define `Coord`.  We're going to do that in terms of a simpler
type that is essentially the same for normal lists --- a type:

```haskell
!!!ttt/Part1.hs "data Sel"1
```

A value of type `Sel n xs x` says that the nth item in `xs` is `x`.

We can define this type inductively, similar to the common [`Index`][Index]
data type.  We can mention our induction rules:

[Index]: http://hackage.haskell.org/package/type-combinators-0.2.4.3/docs/Data-Type-Index.html

1.  The first item in a list as at index 0 (`Z`)
2.  If an item is at index `n` in list `as`, then it is also at index `S n` in
    list `b ': as`.

```haskell
!!!ttt/Part1.hs "data Sel"
```

For example, for the type-level list `'[10,5,2,8]`, we can make values:

```haskell
SelZ             :: Sel         'Z   '[10,5,2,8] 10
SelS SelZ        :: Sel     ('S 'Z)  '[10,5,2,8] 5
SelS (SelS SelZ) :: Sel ('S ('S 'Z)) '[10,5,2,8] 2
```

etc.

We can then use this to define `Coord`:

```haskell
!!!ttt/Part1.hs "data Coord"
```

A `Coord '(i, j) rows piece` contains a selection into the ith list in `rows`,
to get `row`, and a selection into the jth item in `row`, to get `piece`.

### Trying it out

That's it!  Let's see if we can generate some sensible `Update`s, and maybe
even play a sample game.

We'll start with the `EmptyBoard`, and let's add a piece by `PX` at the middle
spot, index (1,1).  This means we want `SelS SelZ :$: SelS SelZ` (a `Coord`
with two indexes into spots 1 and 1) applied to `MkUpdate`.  We'll use
*-XTypeApplications* to specify the type variables `p` and `b`:

```haskell
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

Nice!  This update produces exactly he board expected.

Let's see if we can see if this prevents us from creating an illegal board.
We'll take the result board and see if we can place a `PO` piece there:

```haskell
ghci> let NewBoard = '[ '[ 'Nothing, 'Nothing , 'Nothing ]
                      , '[ 'Nothing, 'Just 'PX, 'Nothing ]
                      , '[ 'Nothing, 'Nothing , 'Nothing ]
                      ]
ghci> :k MkUpdate @_ @_ @'PO @NewBoard (SelS SelZ :$: SelS SelZ)
    • Couldn't match type ‘'Nothing’ with ‘'Just 'PX’
```

Right!  That's because `SelS SelZ :&: SelS SellZ`, applied to `NewBoard`, gives
`Coord '('S 'Z, 'S 'Z) NewBoard ('Just 'PX)`.  However, in order to be used with
`MkUpdate`, the final field has to be `'Nothing`, not `'Just 'PX`.  So, type
error.

We can also use this with `GameState`, using `undefined` as a placeholder for
our `InPlay` witness, since we haven't defined it yet:

```haskell
ghci> :t GSStart
GameState
    'PX
    '[ '[ 'Nothing, 'Nothing, 'Nothing]
     , '[ 'Nothing, 'Nothing, 'Nothing]
     , '[ 'Nothing, 'Nothing, 'Nothing]
     ]

ghci> let s1 = GSUpdate undefined (MkUpdate (SelS SelZ :$: SelS SelZ))
             $ GSStart
ghci> :t s1
GameState
    'PO         -- Player switches correclty!
    '[ '[ 'Nothing, 'Nothing , 'Nothing]
     , '[ 'Nothing, 'Just 'PX, 'Nothing]
     , '[ 'Nothing, 'Nothing , 'Nothing]
     ]

ghci> let s2 = GSUpdate undefined (MkUpdate (SelZ :$: SelZ))
             $ s1
ghci> :t s2
GameState
    'PX
    '[ '[ 'Just 'PO, 'Nothing , 'Nothing]
     , '[ 'Nothing , 'Just 'PX, 'Nothing]
     , '[ 'Nothing , 'Nothing , 'Nothing]
     ]
ghci> let s3 = GSUpdate undefined (MkUpdate (SelZ :$: SelZ))
             $ s2
    • Couldn't match type ‘'Nothing’ with ‘'Just 'PO’
```

We see that `GSUpdate` appropriately alternates the player each turn, keeps
track of our updated boards, and also cannot be constructed if we give an
illegal move (that is, if we try to place a piece where a piece has already
been placed).  Note that type inference allows us to not have to manually
specify the board at each point in time.

### Creating Updates from User Input

Yay!
