---
title: "Extreme Types: Dependently Typed Tic Tac Toe (Part 1)"
categories: Haskell
series: Type-safe Tic Tac Toe
tags: functional programming, dependent types, haskell, singletons, types, decidable
create-time: 2026/02/22 16:34:36
identifier: ttt-1
slug: extreme-types-dependently-typed-tic-tac-toe-1
---

Like a good Haskeller, you always tried your best to "make illegal states
unrepresentable". "Parse, don't validate". Suddenly every "type-unsafety",
every possible illegal state, stands out to you like a repulsive scent. Yet
you tell yourself, "I could probably make that more type-safe, I could probably
prevent that bug...but, I have to be reasonable. There are trade-offs to being
too extreme. Just get good."

But some thought deep inside rises up to whisper to you, "Just do it. Indulge
in that extreme type safety. Build that perfect palace of dependently typed
perfectly type-safe code, no matter how much Haskell (or your coworkers) fight
back. After all, why do the Agda programmers have to have all the fun. What hurt
could it cause?"

Have you ever wondered what it would look like if you took that plunge? Yes, it
will be very painful. But...just how painful? Yes you can build your perfect
castle. But how far could it go? Let's scratch that itch and see today, and
jump into dependently-typed compiler-verified type-safe tic tac toe.

Opening
-------

TODO: set the vibe, state the goal, show the three illegal states we will erase.

Make Illegal States Unrepresentable
-----------------------------------

TODO: quick refresher on the mantra and the exact promises we want.

Safe Indexing
-------------

TODO: explain why lists are the wrong shape, and why Ix + Triple is the right shape.

```haskell
!!!ttt/TicTacToe.hs "data Triple" "data Ix" "data Elem3"
```

Board and Players
-----------------

TODO: introduce Player, Board, and EmptyBoard at the type level.

```haskell
!!!ttt/TicTacToe.hs "data Player" "type Board" "type EmptyBoard"
```

Type-Level Board States
-----------------------

TODO: DataKinds + promoted constructors; show a concrete board type.

```haskell
!!!ttt/TicTacToe.hs "type EmptyBoard"
```

Replace3: A Witness for Updates
-------------------------------

TODO: explain Replace3 and how it proves a single-cell update.

```haskell
!!!ttt/TicTacToe.hs "data Replace3"
```

Play Witness
------------

TODO: compose Replace3 into Play; show why occupied cells are impossible.

```haskell
!!!ttt/TicTacToe.hs "data Play"
```

NextPlayer and Game
-------------------

TODO: show NextPlayer as an injective type family and the Game constructors.

```haskell
!!!ttt/TicTacToe.hs "type family NextPlayer" "data Game"
```

Singletons: The Runtime Bridge
------------------------------

TODO: explain SingKind/SingI and why we need to reflect values to types.

```haskell
!!!ttt/TicTacToe.hs "class SingKind" "class SingI"
```

Singleton Instances
-------------------

TODO: Player/Ix/Maybe/Triple singletons, and the type aliases.

```haskell
!!!ttt/TicTacToe.hs "instance SingKind Player" "instance SingKind Ix" "instance SingKind (Maybe a)" "instance SingKind (Triple a)" "type SPlayer" "type SIx" "type SBoard"
```

Finale
------

TODO: tease the next step: decision functions to turn runtime moves into proofs.

