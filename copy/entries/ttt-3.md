---
title: "Extreme Types: Dependently Typed Tic Tac Toe (Part 3)"
categories: Haskell
series: Type-safe Tic Tac Toe
tags: functional programming, dependent types, haskell, singletons, types, decidable
create-time: 2026/02/22 16:34:36
identifier: ttt-3
slug: extreme-types-dependently-typed-tic-tac-toe-3
---

Recap
-----

TODO: brief summary of Part 2 and why we need victory detection.

AllSame and Lines
-----------------

TODO: AllSame witness, Horiz/Vert/Diag, Line GADT.

```haskell
!!!ttt/TicTacToe.hs "data AllSame" "newtype Horiz" "newtype Vert" "data Diag" "data Line"
```

Victory and NoWinner
--------------------

TODO: Victory witness + NoWinner refutation type.

```haskell
!!!ttt/TicTacToe.hs "data Victory" "type NoWinner"
```

Decision Combinators
--------------------

TODO: decideAny3/decideAll3; how to lift decisions over Triple.

```haskell
!!!ttt/TicTacToe.hs "decideAny3" "decideAll3"
```

decideVictorySing
-----------------

TODO: row/col/diag decisions and combining into Line.

```haskell
!!!ttt/TicTacToe.hs "decideHoriz" "decideVert" "decideDiag" "decideVictorySing"
```

Full Boards and Outcomes
------------------------

TODO: FullCell/Full + decideFull; decideOutcome (win/draw/continue).

```haskell
!!!ttt/TicTacToe.hs "data FullCell" "type Full" "decideFull" "decideOutcome"
```

Game GADT (Final Form)
----------------------

TODO: AddMove requires NoWinner; explain “cannot continue after win/draw.”

```haskell
!!!ttt/TicTacToe.hs "data Game" "AddMove"
```

Finale
------

TODO: the moment “continuing after victory is unconstructable.”
