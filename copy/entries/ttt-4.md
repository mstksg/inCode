---
title: "Extreme Types: Dependently Typed Tic Tac Toe (Part 4)"
categories: Haskell
series: Type-safe Tic Tac Toe
tags: functional programming, dependent types, haskell, singletons, types, decidable
create-time: 2026/02/22 16:34:36
identifier: ttt-4
slug: extreme-types-dependently-typed-tic-tac-toe-4
---

Recap
-----

TODO: brief summary of Part 3 and what’s left (runner + AI).

Rendering
---------

TODO: renderBoard/row/cell; reflect singleton to value.

```haskell
!!!ttt/Main.hs "renderBoard" "renderRow" "renderCell"
```

Parsing Input
-------------

TODO: parseMove + map lookup + withSing (tuple) to get indices.

```haskell
!!!ttt/Main.hs "parseMove" "moveTable"
```

Main Loop
---------

TODO: decideOutcome branching and AddMove construction.

```haskell
!!!ttt/Main.hs "loop ::"
```

```haskell
!!!ttt/TicTacToe.hs "decideOutcome" "data Game" "AddMove"
```

Move Generation
---------------

TODO: allPairs, movesAt, possibleMoves (proof-based move list).

```haskell
!!!ttt/Main.hs "allPairs" "movesAt" "possibleMoves"
```

Negamax AI
----------

TODO: step-by-step negamax; scorePlay, negamax recursion, bestMove.

```haskell
!!!ttt/Main.hs "negamax" "scorePlay" "bestMove"
```

Complexity Notes
----------------

TODO: discuss tradeoffs, why the runner stays boring.

Exercises / Extensions
----------------------

TODO: additional invariants and bigger boards.

Finale
------

TODO: full interactive game + AI that cannot make illegal moves.
