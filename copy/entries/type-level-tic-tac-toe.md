---
title: "Making Illegal States Unrepresentable: Type-Level Tic Tac Toe"
categories: Haskell
tags: functional programming, type safety, haskell, type-level
create-time: 2026/02/22 16:34:36
identifier: type-level-tic-tac-toe
slug: type-level-tic-tac-toe
---

I love the slogan "make illegal states unrepresentable", but it's easy to
repeat it and hard to actually do it. So here's a toy example that forces us
to cash the check: tic tac toe, at the type level.

All of the code here is available below, and `nix develop` will drop you into a
shell with `ghci` ready to go:

!!![code samples]:type-level-tic-tac-toe/flake.nix

```bash
$ cd code-samples/type-level-tic-tac-toe
$ nix develop
$ ghci
ghci> :load TicTacToe.hs
```

The goal: the *type* of a board should encode which moves are legal. If you
try to play in an occupied cell (or play out of turn), the compiler should
refuse to build the program.

A Type-Level Board
------------------

We will represent the board as a length-9 type-level list. Each cell is either
empty or owned by a player.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.TypeLits (TypeError, ErrorMessage(..))

data Player = X | O

data Cell = E | CX | CO

type family ToCell (p :: Player) :: Cell where
  ToCell 'X = 'CX
  ToCell 'O = 'CO
```

The board is just `[Cell]` at the type level. We will assume it has length 9.
(You can make it a length-indexed vector if you want the compiler to enforce
that too.)

```haskell
type Board = [Cell]

type Start =
  '[ 'E, 'E, 'E
   , 'E, 'E, 'E
   , 'E, 'E, 'E
   ]
```

Type-Level Indexing
-------------------

We need a way to read and update a position.

```haskell
type family At (i :: Nat) (xs :: [k]) :: k where
  At 0 (x ': _)  = x
  At n (_ ': xs) = At (n - 1) xs


type family Set (i :: Nat) (x :: k) (xs :: [k]) :: [k] where
  Set 0 x (_ ': xs) = x ': xs
  Set n x (y ': xs) = y ': Set (n - 1) x xs
```

A Legal Move (or a Type Error)
------------------------------

Now the interesting part: placing a mark is only allowed if the target cell
is empty. We can enforce that with a closed type family.

```haskell
type family Place (i :: Nat) (p :: Player) (b :: Board) :: Board where
  Place i p b = Place' (At i b) i p b


type family Place' (c :: Cell) (i :: Nat) (p :: Player) (b :: Board) :: Board where
  Place' 'E  i p b = Set i (ToCell p) b
  Place' c   i p b =
    TypeError
      (     'Text "Illegal move: cell "
      ':<>: 'ShowType i
      ':<>: 'Text " is already occupied."
      )
```

This is already enough to get compile-time rejection of illegal moves.

```haskell
type AfterX = Place 0 'X Start

-- This fails to compile with a clear error message.
type BadMove = Place 0 'O AfterX
```

Turn Order as a Type
--------------------

We can take it one step further and encode whose turn it is in the type as
well. The turn becomes another index that must advance with each move.

```haskell
type family Next (p :: Player) :: Player where
  Next 'X = 'O
  Next 'O = 'X


data Game (p :: Player) (b :: Board) = Game

play :: Game p b -> Proxy i -> Game (Next p) (Place i p b)
play Game _ = Game
```

Now the only way to construct a `Game` with the wrong board or the wrong turn
is to lie to the compiler, which is exactly the point.

Where This Goes
---------------

We could keep going: encode wins as a type-level predicate, reject moves after
the game is over, or prove that the board always has the correct number of Xs
and Os. But the point is already visible: once we move a rule into the type
system, it stops being a convention and starts being a law.

In practice, you rarely want *this* level of type-level acrobatics, but it is
useful to practice on toy problems because it teaches the habit of pushing
invariants into types. When you go back to real code, the same reflex is what
keeps illegal states out of production.
