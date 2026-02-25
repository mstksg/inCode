---
title: "Extreme Types: Dependently Typed Tic Tac Toe (Part 2)"
categories: Haskell
series: Type-safe Tic Tac Toe
tags: functional programming, dependent types, haskell, singletons, types, decidable
create-time: 2026/02/22 16:34:36
identifier: ttt-2
slug: extreme-types-dependently-typed-tic-tac-toe-2
---

Recap
-----

TODO: brief summary of Part 1 and why we need decisions/views.

Decision Functions and Views
----------------------------

TODO: introduce Decision/Refute pattern and the need for structured outcomes.

```haskell
!!!ttt/TicTacToe.hs "data Decision"
```

Singleton Bridge
----------------

TODO: SingKind/SingI, withSing, reflection; minimal singleton layer.

```haskell
!!!ttt/TicTacToe.hs "class SingKind" "class SingI"
```

Existentials (DSum) and SomePlay
--------------------------------

TODO: explain why we need existential result boards and how DSum solves it.

```haskell
!!!ttt/TicTacToe.hs "data SomePlay" "type NoWinner"
```

RowReplace and replaceRow
-------------------------

TODO: walk through RowReplace, explicit result row, and DSum return.

```haskell
!!!ttt/TicTacToe.hs "RowReplace" "replaceRow"
```

playAt as a Decision
--------------------

TODO: show playAt workflow and “views” of coordinates.

```haskell
!!!ttt/TicTacToe.hs "playAt"
```

Trying It Out
-------------

TODO: ghci demo of playAt on empty board and a failing move.

```haskell
!!!ttt/TicTacToe.hs "type EmptyBoard" "playAt"
```

Type-Safe Play
--------------

TODO: connect Play witness to Game constructor (without victory yet).

```haskell
!!!ttt/TicTacToe.hs "data Game" "AddMove"
```

Finale
------

TODO: the moment “runtime can’t cheat; either you get a witness or you don’t.”
