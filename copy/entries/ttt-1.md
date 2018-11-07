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
library.  I strongly strongly recommend reading my [Introduction to
Singletons][] series (and doing the exercises), if you are new to the
singletons library.  However, I will do my best to explain singletons concepts
in brief as they come up.

[decidable]: https://hackage.haskell.org/package/decidable
[singletons]: https://hackage.haskell.org/package/singletons
[Introduction to Singletons]: https://blog.jle.im/entries/series/+introduction-to-singletons.html
!!![Part1.hs]:ttt/Part1.hs

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

### Update

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

#### Coord

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

### Type-safe Play

At the end of this all, we finally have enough to write a truly type-safe
`play` function that allows us to play a round of our game!

```haskell
!!!ttt/Part1.hs "play"
```

`play` is basically the entirety of our game engine! (Minus defining `InPlay`,
which we will take care of later).  It'll take our new move and a proof that
the game is still in play, and return a updated new game state.  Our entire
game is done, and type-safe! It's impossible to play a game in an incorrect
way! (once we define `InPlay`).

Let's try out a few rounds in ghci, using `undefined` instead
of a proper `InPlay` for now:

```haskell
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

Decision Functions and Views
----------------------------

This seems nice, but we're forgetting an important part.  `play` requires us to
only give valid inputs, and enforces that the inputs are valid.  However, how
do we *create* valid inputs, in a way that satisfies `play`?

As we'll see, this is one of the core problems that dependently typed
programming gives us tools to solve.

At this point, we've reached the important part of any "type-safe" application:
*decision functions* and dependent *views*.  *Decision functions* let you
slowly refine your more general values (types) into more specific valid types.
*Views* let you sort out your our values into more "useful" perspectives.

We're going to allow for users to pick to move at any natural number pair
(`(N, N)`), but only *some* of those natural numbers can become valid updates.
In particular, we only allow an `Update` to be made if `(N, N)` represent valid
updates.

What are two ways this can go wrong?  Well, if we allow the user to enter any
two natural numbers, here are all of the potential outcomes:

1.  We might get a coordinate that is out of bounds in x
2.  We might get a coordinate that is in bounds in x, but out of bounds in y
3.  We might get a coordinate that is in bounds in x, in bounds in y, but
    referencing a position that has already been played.
4.  We might get a coordinate that is in bounds in x, in bounds in y, and
    references a blank position.  This is the only "success" case.

Note that we could also just have a "success or nor success" situation, but,
because we might want to provide feedback to the user, it is helpful to not be
"[decision-blind][]" (a cousin of [boolean blindness][]).

[decision-blind]: https://twitter.com/cattheory/status/887760004622757890
[boolean blindness]: https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/

We'll call these potential "views" out of `(N, N)` with respect to some board
`b`.  Let's create a data type representing all of these possibilities (using
`OutOfBounds` as a placeholder predicate for an out-of-bounds coordinate):

```haskell
-- | Placeholder predicate if a given number `n` is out of bounds for a given
-- list.  Predicate is from the 'decidable' library
data OutOfBounds n :: Predicate [k]
!!!ttt/Part1.hs "data Pick"
```

(A value of type `OutOfBounds n @@ xs` is a witness that `xs` satisfies the
`OutOfBounds n` --- that is, `n` is out of bounds of `xs`.  More on this
later when we talk about the *decidable* library!)

So, if we have an `(N, N, Board)`, we should be able to categorize it into one
of each of these potential views.

### Proving functions

This is the job of a "decision function"; in this case, actually, a "proving
function", or a "viewing function".  We need to be able to write a function:

```haskell
pick :: Sing '(i, j, b)
     -> Pick '(i, j, b)
```

That is, given any coordinate and board, we should be able to *totally*
categorize it to one of the four categories (think of them like four
perspectives/classifications), without exception.

This can be considered the boundary between the unsafe and the safe world.
And, to me, this is the "hard part" about dependently typed programming :)

Now, let's write `pick`.  If we want to take any `i`, `j`, and `b`, and turn it
into a valid `Pick`, remember that a valid `Pick '(i, j, b)` contains a `Coord
'(i, j) b 'Nothing`, which contains a `Sel i b row` and a `Sel j row 'Nothing`.
So we need to "convert" some `i`, `j` into a `Sel i b row` and `Sel j row
'Nothing`.

Essentially, we want a function:

```haskell
sel :: Sing i
    -> Sing xs
    -> Sel i xs ??????
```

where `????` is whatever value is in `xs` at index `i`.  It's something we
might not know directly from the input types, necessarily, because it might not
even exist (the list might be too short).

### Existential return types and Dependent Pairs

This pattern --- where we don't know the type of something until after we
receive the function inputs --- is something you might recognize as an
*existential type*, implementable using a *dependent pair* (or dependent sum).

We could write our own dependent pair from scratch, but this is a good
opportunity to practice using *singletons* library's versatile "anonymous
dependent pair" type, `Σ` (or `Sigma`), from *Data.Singletons.Sigma*.

```haskell
data Sigma k :: Predicate k -> Type where
    (:&:) :: Sing x -> (p @@ x) -> Sigma k f

type Σ k = Sigma k
```

A value of type `Sigma k p` contains an `p @@ x` (a witness that `p` satisfies
`x`), existentially *hiding* the `x :: k`, and also `x` itself (as `Sing x`;
remember that `Sing x` is essentially a value-level representation of type
`x`).

You can think of `Sigma k p` as a proof that `p` is satisfied for *some*
`x :: k`, but we can't know which `x` it is until you pattern match.  If you pattern
match, you'll get both `Sing x` (to find out the `x`), and the `p @@ x` (the
witness that `p` is satisfied by `x`).

We can use this to return a `Sel n xs ????`, *hiding* the `???`.

Now for some plumbing: We can't directly give `Sel n xs` to `Sigma` (because it
expects a `Predicate k`, not a `k -> Type`), but we can turn a type constructor
into a `Predicate` using `TyPred`, a convenient combinator from the *decidable*
library:

```haskell
TyPred :: (k -> Type) -> Predicate k
```

```haskell
ghci> :k Pick
(N, N, Board) -> Type

ghci> :k TyPred Pick
Predicate (N, N, Board)

ghci> :k Sel 'Z EmptyBoard
[Maybe Piece] -> Type

ghci> :k TyPred (Sel 'Z EmptyBoard)
Predicate [Maybe Piece]
```

Let's make sure this type works like we expect it to.  We want a `Σ k (TyPred
(Sel n xs))` to contain the `x` at position `n` in `xs`, *and* the `Sel` into
that position.

To make things a little less verbose, we can define a type synonym for `Σ k
(TyPred (Sel n xs))`, `SelFound n xs`:

```haskell
type SelFound n (xs :: [k]) = Σ k (TyPred (Sel n xs))
```

Or, as practice, maybe we can treat `SelFound n` as a predicate on a list?  The
predicate `SelFound n` will be satisfied if list `xs` has some item `x` at
index `n`!

```haskell
!!!ttt/Part1.hs "data SelFound"
```

We can check to make sure this works, by checking the type of witnesses of
`SelFound 'Z @@ '[ 'True, 'False ]`:

Now let's make some sample witnesses of predicate `SelFound n` to ensure we are
thinking about things correctly:

```haskell
!!!ttt/Part1.hs "selFoundTest1"
```

Note that `SFalse :&: SelZ` would be a type error, because the second half
`SelZ` would be `Sel :: 'Z '[ 'True, 'False ] 'True` (because `'True` is the
0th item in the list), so we have to have the first half match `'True`, with
`STrue`.

We can write a witness for `SelFound ('S 'Z) @@ '[ 'True, 'False ]`, as
well, by giving the value of the list at index 1, `'False`:

```haskell
!!!ttt/Part1.hs "selFoundTest2"
```

Before moving on, I strongly recommend trying to write some values of type
`SelFound n @@ xs` for different `n :: N` and `xs :: [a]`, to see what type-checks
and what doesn't.  It'll help you get a feel of the types we are working with,
which might be more advanced than types you might encounter in everyday Haskell
programming.  Remember that you can load up all of the definitions in this post
into a ghci session by downloading [the source code][Part1.hs] and executing it
on the command line, `./Part1.hs`.

Now, we now have enough tools to write the type of the function we would like:

```haskell
sel :: Sing n
    -> Sing xs
    -> SelFound n @@ xs
```

Remember, a `SelFound n @@ xs` contains both a `Sel n xs x` *and* a `Sing x`: a
selection into the `i`th item in `xs` (the `Sel n xs x`), and also the item
itself (the `Sing x`).

We can start writing this, but the type system will soon show you where you run
into problems.  And that's one of the best things about type systems!   They
help you realize when you're trying to do something that doesn't make sense.

```haskell
sel :: Sing n
    -> Sing xs
    -> SelFound n @@ xs
sel = \case
    SZ -> \case
      SNil -> _ :&: _
```

Things start out pretty standard.  We want to match on all potential
constructors of `N` and `[a]`: `N` has `Z` and `S`, so we match on singleton
constructors `SZ` and `SS`; `[a]` has `[]` and `(:)`, so we match on singleton
constructors `SNil` and `SCons`.

If you ask ghc what goes in the typed holes, it'll say that we need a `Sing x`
and a `Sel 'Z '[] x` (which is because matching on `SZ` tells us we are in
`'Z`, and matching on `SNil` tells us we are in `'[]`).   And this...is a
problem.

Remember that the `x` is supposed to be the `n`th item in `xs`.  Here, in this
pattern match branch, we want the zeroth (first) item in `[]`.  This doesn't
exist! That's because there is no item in `[]`, so there is nothing we can put
for the `Sing x`...there's also nothing we could put for the `Sel`, since there
is no constructor of `Sel` that returns a `Sel n '[]` (the constructors of
`Sel` all return `x ': xs`, never `Nil`).

So, this branch is impossible to fulfil.  We know now that we made a large
conceptual error (aren't types great?)

The problem?  Well, indexing into item `n` in list `xs` *might not always
succeed*.  We might try to index into an empty list, so we can't ever get a
result!

### Decision Functions

What we need is not a *proving function*, but, rather, a *decision* function.
A decision function for a predicate `P` is a function:

```haskell
decidePred :: Sing x
           -> Decision (P @@ x)
```

That is, instead of producing a `P @@ x` directly, we produce a `Decision (P @@
x)`.  Here, `Decision` is:

```haskell
data Decision a
    = Proved     a                -- ^ `a` is provably true
    | Disproved (a -> Void)       -- ^ `a` is provably false

-- | The type with no constructors.  If we have a function `a -> Void`, it must
-- mean that no value of type `a` exists.
data Void
```

A decision function means that, for any `x`, we can say that either `P @@ x`
can be proven true or can be proven false.  See [this
section][singletons-decision] for a deeper discussion on why `Decision` has both
the `Proved` and `Disproved` branch.  Essentially, it prevents us from just
returning "disproved" without proving it (so we can be sure that our decision
function is "correct" and not just cheating), and, in the long term, we keep
track of "provably false" because we can use it later to build other useful
decision functions and proving functions.

[singletons-decision]: https://blog.jle.im/entry/introduction-to-singletons-3.html#decision

We use decision functions when we want to "conditionally prove" something ---
it might be true, or it might not be (but definitely one or the other).  It
might exist, or it might not.  We can construct the view, or we can't.
Whatever the perspective, it's always one or the other.

`sel` fits this category: for a `Sel n xs ????`, either `n` is "in bounds" of
`as` (and we can prove this with the item `x` in `xs`), or `n` is "out of
bounds".  Either we get the `x` out of `xs` at slot `n`, or we prove that no
possible `x` exists in `xs` at slot `n`.

#### Deciding SelFound

Enough talk, let's get to it!

Let's write our first dependently typed function.  We start the same way --- by
looking at every possible constructor of `N` and `[a]`:

```haskell
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

Okay, four cases.  Initially daunting, but we can just handle this one by one.
Normally we can just fill in the blanks with the "right" responses, but, for
learning's sake, let's split these branches into four helper functions --- one
for each case.

```haskell
!!!ttt/Part1.hs "selFound" "selFound_znil"2 "selFound_zcons"4 "selFound_snil"3 "selFound_scons"5
```

1.  For the first branch, we have `'Z` and `'[]`.  This should be
    false, because there is no item in the zeroth position in `[]`.  But,
    also, there is no way to construct the `Sel` necessary for the witness,
    since there is no constructor for `Sel` that gives `'[]`.

    We can witness this by using a *total* helper function, `noEmptySel :: Sel
    n '[] a -> Void`, which is a "disproof" of the fact that an empty `Sel` can
    exist. It's a disproof because, if we *had* such a `Sel`, we could produce
    `Void` with it...but `Void` has no constructors.  So no such `Sel` must
    exist!

    We implement it by pattern matching on all potential patterns (using the
    *-XLambdaCase* extension):

    ```haskell
    !!!ttt/Part1.hs "noEmptySel"
    ```

    `noEmptySel` succesfuly implements `Sel n '[] as` by succesfully matching
    on every legal constructor that could produce `Sel n '[] as`.  But, because
    there are no constructors for `Sel` that produce `Sel n '[] as` (we just
    have `SelZ` and `SelS`, which both produce non-empty `Sel`s), that means we
    have to handle all *zero* legal constructors.  Once we handle all zero
    legal constructors, we're done!  (Remember to enable
    *-Werror=incomplete-patterns* to be sure!  GHC will then reject the program
    if there is a pattern we do not handle)

    So we can write this as `Disproved`, which takes a `SelFound 'Z @@ '[] ->
    Void`:

    ```haskell
    !!!ttt/Part1.hs "selFound_znil"
    ```

    Armed with the `Sel 'Z '[] as` that is inside the `SelFound 'Z @@ '[]`, we
    can use `noEmptySel` to produce the `Void`.  We succefully disprove the
    fact that there is any item that can be found in `'[]`, by providing a
    function `SelFound 'Z @@ '[] -> Void`.

2.  For the second branch, we have `'Z` and `(x ': xs)`.  We want to
    prove that there exists an item at position `'Z` in the list `x ': xs`.
    The answer is *yes*, there does, and that item is `x`, and the `Sel` is
    `SelZ`!

    ```haskell
    !!!ttt/Part1.hs "selFound_zcons"
    ```

3.  For the third branch, we have `'S n` and `'[]`.  Again, this should be
    false, because there is no item in the `'S n` position in `'[]`.  We should
    be able to use the same strategy for the first branch, by re-using our
    helper function `noEmptySel`:

    ```haskell
    !!!ttt/Part1.hs "selFound_snil"
    ```

4.  The fourth branch is the most interesting one.  We have `'S n` and `(x ':
    xs)`.  How do we know if the list `x ': xs` has an item in the `'S n` spot?

    Well, we can check if the list `xs` has an item in its `n` spot.

    *   If it does, then call that item `y`, and we know that `x ': xs` has `y`
        in its `'S n` spot.

    *   If it doesn't, then we can't have an item at `'S n` spot in `x ': xs`
        either!  To show why, we can do a proof by contradiction.

        Suppose there *was* an item `y` at the `'S n` spot in `x ': xs`.  If
        so, then that means that there would be an item `y` in the `n` spot in
        `xs`.  However, this was found to be false.  Therefore, we cannot have
        an item in the `'S n` spot in `x ': xs`.

        This is a situation where having a disproof in the `Disproved` branch
        is useful --- we use them to build more complex disproofs from simple
        ones.

    ```haskell
    !!!ttt/Part1.hs "selFound_scons"
    ```

    If you have problems understanding this, try playing around with typed
    holes in GHC, or trying to guess what types everything has in the
    implementation above, until you can figure out what is happening when.


<!-- PickValid  :: Coord '(i, j) b 'Nothing -> Pick '(i, j, b) -->

<!-- Well, remember that a *succesful* `Pick` contains a `Sel i b row` and a `Sel j -->
<!-- row p`.  We need to somehow take an `i :: N` and turn it into a `Sel i b row`, -->
<!-- and take a `j :: N` and turn it into a `Sel j row p`.  We need to "convert" a -->
<!-- `N` into some `Sel`, in a way that could potentially fail. -->

<!-- We can write this by scratch, by hand, but we're going to look at a couple of -->
<!-- useful tools from the *decidable* library to help us. -->

<!-- ## The Decidable Library -->

<!-- The *[decidable][]* library offers a couple of conceptual tools to work with -->
<!-- views and predicates.  Here's a quick run-down: -->

<!-- The main type that the library works with is `Predicate`: -->

<!-- ```haskell -->
<!-- type Predicate k = k ~> Type -->
<!-- ``` -->

<!-- `k ~> Type` is the kind of a *defunctionalization symbol* --- it's a dummy data -->
<!-- type that can be passed around, and represents a function `k ~> Type` that can -->
<!-- be "applied" using `Apply` or `@@`.  We say that, for predicate `MyPred`, we define: -->

<!-- ```haskell -->
<!-- type instance Apply MyPred x = MyWitness -->
<!-- ``` -->

<!-- Where `MyWitness` is the witness for the type-level predicate `MyPred`. -->
<!-- We can define a predicate from scratch by declaring the above type family -->
<!-- instance, but the library is defined so that you rarely ever have to define a -->
<!-- `Predicate` by hand.  Usually, we can use predicate "combinators", to construct -->
<!-- predicates from simpler pieces. -->

<!-- For example, we have the `TyPred` combinator: -->

<!-- ```haskell -->
<!-- TyPred :: (k -> Type) -> Predicate k -->
<!-- ``` -->

<!-- It turns a normal `k -> Type` type constructor into a `Predicate k`.  So, we -->
<!-- can use `Pick :: (N, N, Board) -> Type` -->

<!-- ```haskell -->
<!-- ghci> :k TyPred Pick -->
<!-- Predicate (N, N, Board) -->
<!-- ``` -->

<!-- `TyPred Pick` is a predicate that, given a coordinate and a board, we can -->
<!-- create a valid `Pick` using one of the `Pick` constructors.  The *witness* of -->
<!-- the `TyPred` predicate is the type constructor itself: -->

<!-- ```haskell -->
<!-- type instance Apply (TyPred t) a = t a -->
<!-- ``` -->

<!-- So the witness for `TyPred Pick @@ a` is just `Pick a`: -->

<!-- ```haskell -->
<!-- ghci> :kind! TyPred Pick @@ '( 'Z, 'Z, EmptyBoard ) -->
<!-- Pick '( 'Z, 'Z, [ [Nothing,Nothing.Nothing] -->
<!--                 , [Nothing,Nothing,Nothing] -->
<!--                 , [Nothing,Nothing,Nothing] -->
<!--                 ] -->
<!--       ) -->

<!-- ghci> :kind! forall a. TyPred Pick @@ a -->
<!-- Pick a      -- the witness for `TyPred Pick @@ a` is just `Pick a` -->
<!-- ``` -->

<!-- ### Provable -->

<!-- We can say that a type-level predicate (or view) `P` is *provable* if we can -->
<!-- always create a value of `P @@ x`, for any `x`.  If `P` is a view, it means -->
<!-- that we can always view any `x` from the "perspective" of one of the -->
<!-- constructors of `P`. -->

<!-- Essentially, it means we can write a function -->

<!-- ```haskell -->
<!-- prove :: Sing x       -- ^ for any x -->
<!--       -> P @@ x       -- ^ P @@ x can be generated; you can view from -->
<!--                       --   perspective of a constructor of P -->
<!-- ``` -->

<!-- Our viewing function for predicate `TyPred Pick` would look like this: -->

<!-- ```haskell -->
<!-- pick :: Sing '(i, j, b) -->
<!--      -> TyPred Pick @@ '(i, j, b) -->
<!-- ``` -->



<!-- *decidable* makes this a little nicer to work with by providing a typeclass for -->
<!-- predicates with "canonical" viewing functions, called `Provable`: -->

<!-- ```haskell -->
<!-- -- | Class providing a canonical proving function or view for predicate `p`. -->
<!-- class Provable p where -->
<!--     -- | Given any `x`, produce the witness `p @@ x`. -->
<!--     prove :: forall x. Sing x -> (p @@ x) -->
<!-- ``` -->

<!-- The benefit of using a typeclass is that we can associate a canonical -->
<!-- proving/viewing function with a consistent name, and also so that higher-order -->
<!-- predicate combinators can build proving functions based on proving functions of -->
<!-- the predicates they are parameterized on. -->

<!-- In our case, writing a view function would look like this: -->

<!-- ```haskell -->
<!-- instance Provable (TyPred Pick) where -->
<!--     prove :: Sing ijb -> Pick ijb -->
<!--     prove (STuple3 i j b) = undefined -->
<!--         -- ^ STuple3 is the singleton for three-tuples -->
<!-- ``` -->

<!-- Then, given any `(i, j, b)` combination, we can classify it into one of the -->
<!-- constructors of `Pick` by just using `prove @(TyPred Pick) sIJB`. -->

<!-- Now that we've restated things in the context of *decidable*...how do we -->
<!-- actually write `prove @(TyPred Pick)`? -->

<!-- Well, remember that a *succesful* `Pick` contains a `Sel i b row` and a `Sel j -->
<!-- row p`.  We need to somehow take an `i :: N` and turn it into a `Sel i b row`, -->
<!-- and take a `j :: N` and turn it into a `Sel j row p`.  We need to "convert" a -->
<!-- `N` into some `Sel`, in a way that could potentially fail. -->

<!-- ### ParamPred -->

<!-- Another useful kind synonym that *decidable* gives is in -->
<!-- *Data.Type.Predicate.Param*, the "parameterized predicate": -->

<!-- ```haskell -->
<!-- type ParamPred k v = k -> Predicate v -->
<!-- ``` -->

<!-- If `MyPP :: ParamPred k v` is a parameterized predicate, then `MyPP x` is a -->
<!-- `Predicate v`. -->

<!-- The main usage of parameterized predicate is for usage with the `Found` -->
<!-- predicate combinator: -->

<!-- ```haskell -->
<!-- Found :: ParamPred k v -> Predicate k -->
<!-- ``` -->

<!-- `Found MyPP` is a predicate that, for any `x :: k`, we can find *some* `y :: v` -->
<!-- that satisfies `MyPP x @@ y`. -->

<!-- Again, the library is constructed so that you shouldn't need to define -->
<!-- a `ParamPred` by hand; you can just use combinators and constructors. -->

<!-- For example, we have `TyPP`: -->

<!-- ```haskell -->
<!-- TyPP :: (k -> v -> Type) -> ParamPred k v -->
<!-- ``` -->

<!-- Which turns any normal type constructor into a `ParamPred`. -->
<!-- For example, let's look at `Sel 'Z`: -->

<!-- ```haskell -->
<!-- ghci> :k TyPP (Sel 'Z) -->
<!-- ParamPred [k] k -->
<!-- ``` -->

<!-- `TyPP (Sel 'Z)` is the parameterized predicate that, given a list `xs :: -->
<!-- [k]`, we can produce an `x :: k` that is at index `'Z`.  That's because its -->
<!-- witness is `Sel 'Z xs x` (the witness that `x` is at position `'Z` in `xs`). -->

<!-- What is `Found (TyPP (Sel 'Z))`? -->

<!-- ```haskell -->
<!-- ghci> :k Found (TyPP (Sel 'Z)) -->
<!-- Predicate [k] -->
<!-- ``` -->

<!-- Judging from the type, it is some predicate on a type level list.  And knowing -->
<!-- what we know about `Found`, we can conclude what it is: It is a predicate that, -->
<!-- given some list `xs`, there *is some value `x`* at position `'Z`.  It's -->
<!-- essentially a predicate that the list *has* something at position `'Z`. -->

<!-- We can generalize it further; `Found (TyPP (Sel ('S 'Z)))` must be the -->
<!-- predicate that some given list `xs` has a value `x` at position `'S 'Z`.  It -->
<!-- says that there must be *some* value at `'S 'Z`. -->

<!-- Really, `Found (TyPP (Sel n))` is a predicate that some list `xs` is *at least* -->
<!-- `n + 1` items long.  That's because we know that the list has to have some item -->
<!-- at position `n`. -->

<!-- There's a better name for this --- we'll call it `InBounds` -->

<!-- ```haskell -->
<!-- !!!ttt/Part1.hs "type InBounds" -->
<!-- ``` -->

<!-- `InBounds n :: Predicate [k]` is the predicate that, given some list `xs`, `n` -->
<!-- is "in bounds" of `xs`. -->

<!-- And *decidable* is nice because it offers a predicate combinator `Not`, which -->
<!-- gives the negation of any predicate: -->

<!-- ```haskell -->
<!-- !!!ttt/Part1.hs "type OutOfBounds" -->
<!-- ``` -->

<!-- `OutOfBounds n :: Predicate [k]` is the predicate that, given some list `xs`, `n` -->
<!-- is *not* in bounds of `xs`, and that it is actually *out* of bounds. -->

<!-- ### Decidable -->

<!-- Now, is `InBounds n` going to be `Provable`?  No, not quite.  That's because a -->
<!-- given list `xs` might be actually out of bounds.  For example, `InBounds 'Z @@ -->
<!-- '[1,2,3]` is satisfiable, but `InBounds ('S 'Z) '[]` is not. -->

<!-- To implement our view of `Pick`, we would like a function that can *decide* -->
<!-- whether or not `InBounds n` is satisfied by a given list `xs`.  What we want is -->
<!-- a *decision function*: -->

<!-- ```haskell -->
<!-- inBounds :: forall n xs. () -->
<!--          => Sing xs -->
<!--          -> Decision (InBounds n @@ xs) -->
<!-- ``` -->

<!-- Remember that `Decision` is a data type that is kind of like `Maybe`, but with -->
<!-- a "disproof" if the input is disprovable: -->

<!-- ```haskell -->
<!-- data Decision a -->
<!--     = Proved     a                -- ^ `a` is provably true -->
<!--     | Disproved (a -> Void)       -- ^ `a` is provably false -->

<!-- -- | The type with no constructors.  If we have a function `a -> Void`, it must -->
<!-- -- mean that no value of type `a` exists. -->
<!-- data Void -->
<!-- ``` -->

<!-- The *decidable* library offers a typeclass for a *canonical* decision function -->
<!-- for any `Predicate`: -->

<!-- ```haskell -->
<!-- -- | Class providing a canonical decision function for predicate `p`. -->
<!-- class Decidable p where -->
<!--     -- | Given any `x`, either prove or disprove the witness `p @@ x`. -->
<!--     decide :: forall x. Sing x -> Decision (p @@ x) -->
<!-- ``` -->

<!-- Of course, we could always just write our decision function `inBounds` from -->
<!-- scratch, but it's convenient to pull everything into a typeclass instead for -->
<!-- the reasons discussed earlier. -->

<!-- ### Deciding InBounds -->

<!-- Alright, time to write our first bona-fide decision function for `InBounds`, -->
<!-- which we will use to write our view function for `Pick`. -->

<!-- The decision function requires us to produce a witness for `InBounds n @@ -->
<!-- xs`...so we need to know what that witness looks like. -->

<!-- To do this, we could either look at the documentation for `Found` (because -->
<!-- `InBounds n = Found (TyPP (Sel n))`) to find its `Apply` instance, or we could -->
<!-- just ask GHC what this looks like for a given input, using `:kind!`: -->

<!-- ```haskell -->
<!-- ghci> :kind! InBounds 'Z @@ '[1,2,3]  -- what is the type of the witness for `InBounds 'Z1 ? -->
<!-- Σ Nat (TyPP (Sel 'Z) '[1,2,3]) -->
<!-- ``` -->

<!-- In general, the witness for `Found (p :: ParamPred k v)` is: -->

<!-- ```haskell -->
<!-- type instance Apply (Found p) x = Σ v (p x) -->
<!-- ``` -->

<!-- `Σ` might seem a little scary, but remember that it's a type synonym for -->
<!-- the dependent pair `Sigma` type, from *Data.Singletons.Sigma*: -->

<!-- ```haskell -->
<!-- data Sigma k :: (k ~> Type) -> Type where -->
<!--     (:&:) :: Sing x -> (f @@ x) -> Sigma k f -->

<!-- type Σ k = Sigma k -->
<!-- ``` -->

<!-- I wrote a small mini-tutorial on `Sigma` [here][sigma], if you need a -->
<!-- refresher.  Basically, if we had `f :: k ~> Type`, then `Sigma k f` contains an -->
<!-- `f @@ x`, for some `x`, along with `Sing x` (to help us recover what `x` was, -->
<!-- once we pattern match).  It's a *dependent pair* or *dependent sum* type.  You -->
<!-- can think of it as `Sigma k f` existentially *wrapping* `x :: k`, to show that -->
<!-- there is at least some `x` somewhere out there such that `f @@ x` exists. -->

<!-- [sigma]: https://blog.jle.im/entry/introduction-to-singletons-4.html#sigma -->

<!-- This makes a lot of sense as a witness to `Found p`.  `Found p @@ x` says that -->
<!-- there is some `y` such that `p x @@ y` is satisfied.  So, what is the witness -->
<!-- of that statement?  The `y` itself! (wrapped in a `Σ`) -->

<!-- So, the witness for `InBounds 'Z @@ '[ 'True, 'False ]` is the -->
<!-- item in the list `'[1,2,3]` at position `'Z` --- `'True`.  Let's see this in -->
<!-- action: -->

<!-- ```haskell -->
<!-- inBoundsTest1 :: InBounds 'Z @@ '[ 'True, 'False ] -->
<!-- inBoundsTest1 = STrue :&: SelZ -->
<!--                        -- ^ Sel 'Z '[ 'True, 'False ] 'True -->
<!-- ``` -->

<!-- Note that we can't put `SFalse` in `inBoundsTest1`, because the second half -->
<!-- `SelZ` would be `Sel :: 'Z '[ 'True, 'False ] 'True` (because `'True` is the -->
<!-- 0th item in the list), so we have to have the first half match `'True`. -->

<!-- And we can write a witness for `InBounds ('S 'Z) @@ '[ 'True, 'False ]`, as -->
<!-- well, by giving the value of the list at index 1, `'False`: -->

<!-- ```haskell -->
<!-- inBoundsTest2 :: InBounds ('S 'Z) @@ '[ 'True, 'False ] -->
<!-- inBoundsTest2 = SFalse :&: SelS SelZ -->
<!--                         -- ^ Sel ('S 'Z) '[ 'True, 'False ] 'False -->
<!-- ``` -->

<!-- With that in mind, let's write our decision function for `InBounds n`.  It's -->
<!-- going to be our actual first dependently typed function! -->

<!-- For the sake of learning, we're going to write it as a standalone function -->
<!-- `inBounds`.  It's going to take `Sing n` (the index) and `Sing xs` (the -->
<!-- list) and produce a decision on `InBounds n @@ xs`.  Like for any Haskell -->
<!-- function on ADTs, we'll start out by just writing all of our case statement -->
<!-- branches (using *-XLambdaCase* for conciseness).  An `N` can either be `Z` or -->
<!-- `S n`, so we match on singletons `SZ` and `SS`.  A `[a]` can either be `[]` or -->
<!-- `x : xs`, so we match on singletons `SNil` and ``x `SCons` xs`` -->

<!-- ```haskell -->
<!-- inBounds :: Sing n -> Sing xs -> Decision (InBounds n @@ xs) -->
<!-- inBounds = \case -->
<!--     SZ -> \case -->
<!--       SNil         -> _ -->
<!--       x `SCons` xs -> _ -->
<!--     SS n -> \case -->
<!--       SNil         -> _ -->
<!--       x `SCons` xs -> _ -->
<!-- ``` -->

<!-- Okay, four cases.  Initially daunting, but we can just handle this one by one. -->
<!-- Again, for learning's sake, ket's split these branches into four helper -->
<!-- functions --- one for each case. -->

<!-- ```haskell -->
<!-- !!!ttt/Part1.hs "inBounds" "inBounds_znil"2 "inBounds_zcons"4 "inBounds_snil"3 "inBounds_scons"5 -->
<!-- ``` -->

<!-- 1.  For the first branch, we have `'Z` and `'[]`.  This should be -->
<!--     false, because there is no item in the zeroth position in `[]`.  But, -->
<!--     also, there is no way to construct the `Sel` necessary for the witness, -->
<!--     since there is no constructor for `Sel` that gives `'[]`. -->

<!--     So we can write this as `Disproved`, which takes a `InBounds 'Z @@ '[] -> -->
<!--     Void`: -->

<!--     ```haskell -->
<!--     !!!ttt/Part1.hs "inBounds_znil" -->
<!--     ``` -->

<!--     We can satisfy that `InBounds 'Z @@ '[] -> Void` by pattern matching on the -->
<!--     `Sel` it *would* contain.  Because there is no `Sel` for an empty list, the -->
<!--     empty pattern match is safe. -->

<!--     Remember to enable *-Werror=incomplete-patterns* to be sure! -->

<!-- 2.  For the second branch, we have `'Z` and `(x ': xs)`.  We want to -->
<!--     prove that there exists an item at position `'Z` in the list `x ': xs`. -->
<!--     The answer is *yes*, there does, and that item is `x`, and the `Sel` is -->
<!--     `SelZ`! -->

<!--     ```haskell -->
<!--     !!!ttt/Part1.hs "inBounds_zcons" -->
<!--     ``` -->

<!-- 3.  For the third branch, we have `'S n` and `'[]`.  Again, this should be -->
<!--     false, because there is no item in the `'S n` position in `'[]`.  We should -->
<!--     be able to use the same strategy for the first branch: -->

<!--     ```haskell -->
<!--     !!!ttt/Part1.hs "inBounds_snil" -->
<!--     ``` -->

<!-- 4.  The fourth branch is the most interesting one.  We have `'S n` and `(x ': -->
<!--     xs)`.  How do we know if the list `x ': xs` has an item in the `'S n` spot? -->

<!--     Well, we can check if the list `xs` has an item in its `n` spot. -->

<!--     *   If it does, then call that item `y`, and we know that `x ': xs` has `y` -->
<!--         in its `'S n` spot. -->

<!--     *   If it doesn't, then we can't have an item at `'S n` spot in `x ': xs` -->
<!--         either!  To show why, we can do a proof by contradiction. -->

<!--         Suppose there *was* an item `y` at the `'S n` spot in `x ': xs`.  If -->
<!--         so, then that means that there would be an item `y` in the `n` spot in -->
<!--         `xs`.  However, this was found to be false.  Therefore, we cannot have -->
<!--         an item in the `'S n` spot in `x ': xs`. -->

<!--     ```haskell -->
<!--     !!!ttt/Part1.hs "inBounds_scons" -->
<!--     ``` -->

<!--     If you have problems understanding this, try playing around with typed -->
<!--     holes in GHC, or trying to guess what types everything has in the -->
<!--     implementation above, until you can figure out what is happening when. -->

<!-- Finally, we can wrap everything up by providing our first ever `Decidable` -->
<!-- instance.  We need to give `inBounds` a `Sing n`, so we can do that using -->
<!-- `sing :: Sing n`, provided that the instance has a `SingI n` constraint. -->

<!-- ````haskell -->
<!-- !!!ttt/Part1.hs "instance SingI n => Decidable (InBounds n)" -->
<!-- ```` -->

<!-- ### Proving Pick -->

<!-- Now that we can decide `InBounds`, let's finally prove `Pick`. -->

<!-- Again, for learning purposes, we'll define `pick` as its own function and then -->
<!-- write an instance for `Provable`. -->

<!-- ```haskell -->
<!-- pick -->
<!--     :: forall i j b. () -->
<!--     => Sing i -->
<!--     -> Sing j -->
<!--     -> Sing b -->
<!--     -> Pick '(i, j, b) -->
<!-- pick Sing Sing b = -->
<!-- ```` -->

<!-- We'll match with the `Sing` constructor for `Sing i` and `Sing j`; the `Sing` -->
<!-- constructor is a pattern synonym that, if matched on, brings `SingI i` and -->
<!-- `SingI j` instances into scope. -->

<!-- Remember, the goal is to try to prove we have a valid pick.  We want to create -->
<!-- something with the `PickValid` constructor if we can: -->

<!-- ```haskell -->
<!-- PickValid  :: Coord '(i, j) b 'Nothing -> Pick '(i, j, b) -->

<!-- (:$:) :: Sel i rows row -->
<!--       -> Sel j row  p -->
<!--       -> Coord '(i, j) rows p -->
<!-- ``` -->

<!-- So we need a `Coord '(i, j) b 'Nothing`, which means we need a `Sel i b row` -->
<!-- and a `Sel j row 'Nothing`.  Let's use our decision functions we wrote to get -->
<!-- these!  In particular, we can use `decide @(InBounds i) b` to get our `Sel i b -->
<!-- row`, and then use `decide @(InBounds j) row` to get our `Sel j row piece`! -->

<!-- ```haskell -->
<!-- pick -->
<!--     :: forall i j b. () -->
<!--     => Sing i -->
<!--     -> Sing j -->
<!--     -> Sing b -->
<!--     -> Pick '(i, j, b) -->
<!-- pick Sing Sing b = case decide @(InBounds i) b of -->
<!--     Proved (row :&: selX) -> case decide @(InBounds j) row of -->
<!--       Proved (p :&: selY) -> -->
<!--         let c = selX :$: selY -->
<!--         in  -- success??? -->
<!-- ``` -->

<!-- Just to clarify what's going on, let's give types to the names above: -->

<!-- ```haskell -->
<!-- b    :: Sing (b   :: [[Maybe Piece]]) -->
<!-- row  :: Sing (row ::  [Maybe Piece] ) -->
<!-- selX :: Sel i b row -->
<!-- p    :: Sing (p   ::   Maybe Piece  ) -->
<!-- selY :: Sel j row p -->
<!-- c    :: Coord '(i, j) b p -->
<!-- ``` -->

<!-- `row` above is the `Sing` that comes attached with all `Σ` constructors, which -->
<!-- is why we can give it to `decide @(InBounds j)`, which expects a singleton of -->
<!-- the list. -->

<!-- So, now we have `Coord '(i, j) b p`.  We know that `i` and `j` are in-bounds. -->
<!-- But, we need to know that `p` is `'Nothing` before we can use it with -->
<!-- `PickValid`.  To do that, we can pattern match on `p`, because it's the -->
<!-- singleton that comes with the `Σ` constructor: -->

<!-- ```haskell -->
<!-- pick -->
<!--     :: forall i j b. () -->
<!--     => Sing i -->
<!--     -> Sing j -->
<!--     -> Sing b -->
<!--     -> Pick '(i, j, b) -->
<!-- pick Sing Sing b = case decide @(InBounds i) b of -->
<!--     Proved (row :&: selX) -> case decide @(InBounds j) row of -->
<!--       Proved (p :&: selY) -> -->
<!--         let c = selX :$: selY -->
<!--         in  case p of -->
<!--               SNothing -> PickValid   c     -- p is 'Nothing -->
<!--               SJust q  -> PickPlayed  c q   -- p is 'Just q -->
<!-- ``` -->

<!-- Finally, knowing that `p` is `'Nothing`, we can create `PickValid`! -->

<!-- As a bonus, if we know that `p` is `'Just p`, we can create `PickPlayed`, -->
<!-- which is the constructor for an in-bounds pick but pointing to a spot that is -->
<!-- already occupied by piece `p'`. -->

<!-- ```haskell -->
<!-- PickPlayed :: Coord '(i, j) b ('Just p) -->
<!--            -> Sing p -->
<!--            -> Pick '(i, j, b) -->
<!-- ``` -->

<!-- We now have to deal with the situations where things are out of bounds. -->

<!-- ```haskell -->
<!-- PickOoBX :: OutOfBounds i @@ b -->
<!--          -> Pick '(i, j, b) -->
<!-- PickOoBY :: Sel i b row -->
<!--          -> OutOfBounds j @@ row -->
<!--          -> Pick '(i, j, b) -->
<!-- ``` -->

<!-- However, thanks to the *[decidable][]* library, things work out nicely.  That's -->
<!-- because `OutOfBounds n` we defined as: -->

<!-- ```haskell -->
<!-- !!!ttt/Part1.hs "type OutOfBounds" -->
<!-- ``` -->

<!-- and `Not`, the predicate combinator, is defined as: -->

<!-- ```haskell -->
<!-- data Not :: Predicate k -> Predicate k -->

<!-- type instance Apply (Not p) x = (p @@ x) -> Void -->
<!-- ``` -->

<!-- That is, a witness of `Not p @@ x` is a function of type `p @@ x -> Void`. -->
<!-- That means that `PickOoBX` expects an `InBounds i @@ b -> Void`, and `PickOoBY` -->
<!-- expects an `InBounds j @@ row -> Void`.  And that's *exactly* what the -->
<!-- `Disproved` branches give! -->

<!-- ```haskell -->
<!-- !!!ttt/Part1.hs "pick" -->
<!-- ``` -->

<!-- And that's it! -->

<!-- Now to just tie it all together with a `Provable` instance, using the `STuple3` -->
<!-- singletons constructor: -->

<!-- ```haskell -->
<!-- !!!ttt/Part1.hs "instance Provable (TyPred Pick)" -->
<!-- ``` -->

<!-- Play Ball -->
<!-- --------- -->

<!-- Bringing it all together, we can write a simple function to take user input and -->
<!-- *play* it. -->

<!-- <1!-- TODO: maybe withhold Decidable until very end --1> -->
