---
title: "Extreme Types: Dependently Typed Tic Tac Toe (Part 1)"
categories: Haskell
series: Type-safe Tic Tac Toe
tags: functional programming, dependent types, haskell, singletons, types, decidable
create-time: 2026/02/22 16:34:36
identifier: ttt-1
slug: extreme-types-dependently-typed-tic-tac-toe-1
---

One problem with adoption of dependent types in everyday programming, I think,
is that most examples out there are sort of small and self-contained.  There
aren't *too* many larger-scale examples out there showing how dependent types
can permeate your whole program to make everything more robust and error-free.

So, this post will be implementing a type-safe *tic tac toe* game (a
medium-scale Haskell app) that can be played on the console, with a small IO
runner.  We will:

1.  Build up our core game engine, talking about what it really means to be
    type safe
2.  Use our type-safe engine to drive a tiny interactive runner

The slogan "make illegal states unrepresentable" sits right at that edge.  It is
compelling, but it is also easy to say without actually paying the cost.  So in
this post I want to cash the check, at least once, on a small and concrete
example: tic tac toe, at the type level.

The twist is that I will avoid `TypeError` and instead stick to GADTs and
witnesses.  That is the style I actually like, because it forces us to
construct explicit proofs instead of hiding behind failure cases.

This post will also be a mini-tutorial on the *[decidable][]* pattern (even if I
spell it out explicitly instead of importing the package), and a light-weight
version of the *[singletons][]* story.  If you are new to these ideas, I
strongly recommend reading my [Introduction to Singletons][] series, but I will
do my best to explain the concepts in brief as they come up.

[decidable]: https://hackage.haskell.org/package/decidable
[singletons]: https://hackage.haskell.org/package/singletons
[Introduction to Singletons]: https://blog.jle.im/entries/series/+introduction-to-singletons.html

All of the code here is available below, and `nix develop` will drop you into a
shell with `ghci` ready to go:

!!![code samples]:ttt/flake.nix
!!![TicTacToe.hs]:ttt/TicTacToe.hs
!!![Main.hs]:ttt/Main.hs

```bash
$ cd code-samples/ttt
$ nix develop
$ ghci
ghci> :load TicTacToe.hs
```

Type-Safety
Let's make this concrete.  When I say "type safe" here, I mean that the *only*
values you can construct are the ones that satisfy the rules of the game.  If a
state is illegal, it should be uninhabited.  That is a stronger notion than
"we check and throw an error".  It is closer to a proof obligation.

So the first step is not code.  The first step is to decide which facts about
the game must be true, and to decide whether we want the compiler to enforce
those facts or whether we want to leave them as runtime checks.
-----------

First off, we should ask the question: what does it mean to be type-safe?

?????

The Specification
Let's break the specification into implementation steps.

1.  Define a *type* for boards and players.
2.  Define a *type* for legal moves from one board to another.
3.  Define a *type* for valid game states, using (1) and (2).

Everything else in the file is just scaffolding to make those three items
precise.
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
Step by step, the implementation is:

1.  Choose a small index type (`Ix`) so we never talk about "out of range" at
    the type level.
2.  Use a concrete board shape (`Triple (Triple (Maybe Player))`) so that
    indexing and replacement are explicit witnesses rather than arithmetic.
3.  Define a singleton layer so we can carry type-level boards into the value
    level (for the runner).

Each of these steps is there to keep the proof objects simple and readable.
-----------

First, we'll define the types we need to specify our state:

```haskell
!!!ttt/TicTacToe.hs "data Player" "data Triple" "data Ix" "type Board"
```

A `Player` will also represent our player -- either `X` or `O`.  Our `Board`
will be a 3x3 `Triple` of `Maybe Player`.  If the spot contains `Nothing`, the
spot is unplayed; if the spot is `Just p`, then it means the spot has been
played by `p`.

And some values and functions we need to talk about empty boards and state
transformations:

```haskell
!!!ttt/TicTacToe.hs "type EmptyBoard"
```

We'll also use a tiny indexing witness, `Elem3`, which lets us pick out a row
or column by evidence rather than by a number.

```haskell
!!!ttt/TicTacToe.hs "data Elem3"
```

Now we can talk about valid updates.

### Update
Implementation steps for `Update`/`Play`:

1.  Define `Replace3`, a witness that one cell in a `Triple` was replaced.
2.  Use `Replace3` twice: once for the row, once for the board.
3.  Package those witnesses into `Play p board board'`.

At that point, a legal move is literally a proof object.  There is no other way
into the type.

Let's go about what defines a valid update.  Remember, the kind we want is a
witness that a board `board` is updated by player `p` to a new board `board'`.

A move is legal exactly when a `Nothing` becomes a `Just p` at a specific
position.  We encode that as a witness of replacement.  There is no partial
function that "tries" to update a board: either you have the witness or you do
not.

```haskell
!!!ttt/TicTacToe.hs "data Replace3" "data Play"
```

This seems nice, but we're forgetting an important part.  `Play` requires us to
only give valid inputs, and enforces that the inputs are valid.  However, how do
we *create* valid inputs, in a way that satisfies `Play`?

As we'll see, this is one of the core problems that dependently typed
programming gives us tools to solve.

Decision Functions and Views
Step by step, the decision procedure is:

1.  Start with a raw coordinate from the user.
2.  Convert it to `Ix` (bounded indices).
3.  Ask `playAt` for a witness at that coordinate.
4.  If the witness exists, keep it; if not, reject the move.

This is the same pattern you will see over and over: refine, witness, carry.
----------------------------

At this point, we've reached the important part of any "type-safe"
application: *decision functions* and dependent *views*.  *Decision functions*
let you slowly refine your more general values (types) into more specific valid
types.  *Views* let you sort out your values into more "useful" perspectives.

#### Coord

We'll allow the user to pick any coordinate, but only some coordinates are
valid.  The index type `Ix` is our small, finite coordinate system, and it
plays the role of the "bounded" coordinate type.

We are going to allow the user to pick any coordinate (row, column), but only
*some* of those coordinates can become valid plays.  In particular, `playAt`
will only produce a `Play` if the cell is empty.

What are the ways this can go wrong?  There are exactly four outcomes:

1.  We might get a coordinate that is out of bounds in x
2.  We might get a coordinate that is in bounds in x, but out of bounds in y
3.  We might get a coordinate that is in bounds in x, in bounds in y, but
    referencing a position that has already been played.
4.  We might get a coordinate that is in bounds in x, in bounds in y, and
    references a blank position.  This is the only "success" case.

Note that we could also just have a "success or not success" situation, but,
because we might want to provide feedback to the user, it is helpful to not be
"[decision-blind]" (a cousin of [boolean blindness][]).

In the code, this is all funneled through `Decision`:

```haskell
!!!ttt/TicTacToe.hs "data Decision"
```

And the actual refinement is done by `playAt`, which returns either a witness
of legality or a proof that no such witness can exist.

```haskell
!!!ttt/TicTacToe.hs "playAt"
```

We'll call these potential "views" out of `(Ix, Ix)` with respect to some
board `b`.  Let's create a data type representing all of these possibilities by
using the existing witness types in the code (the ones that carry the refined
board and the replacement evidence).

```haskell
!!!ttt/TicTacToe.hs "PlayAt" "RowReplace"
```

If the move is valid, we get a `PlayAt` witness.  If not, we get a proof that
no such witness can exist.

### Trying it out

Let's just try it out in `ghci` to see the shape of things.  We can start with
the empty board and ask for a legal play at a specific coordinate.  If the
coordinate is valid and the cell is empty, we get back a witness.  Otherwise we
get a refutation.

```haskell
!!!ttt/TicTacToe.hs "sEmptyBoard" "playAt"
```

We don't have to *run* the program to know which branch we are in -- the type
system already determines what can and cannot be constructed.

### Type-safe Play

Once we have a witness for a move, we can add it to a `Game`.  The constructor
only accepts a `Play` and a proof that the previous board was not already won,
so you literally cannot advance the game without satisfying the rules.

```haskell
!!!ttt/TicTacToe.hs "data Game" "AddMove"
```

This is the same denotative idea from the specification section: we are just
transcribing the inference rules into constructors.

Lines and Victories
Implementation steps for lines:

1.  Define witnesses for each line orientation.
2.  Bundle them into a single `Line` GADT.
3.  Define `AllSame` and use it to define `Victory`.

This gives us a concrete object we can pass around instead of a boolean flag.
-------------------

We also need a way to talk about winning lines.  At first, this seems like a
simple boolean predicate, but that immediately runs into the same problem we
had before: a predicate tells you *that* something is true, but it does not
give you the *evidence* you can carry forward.

So, again, we use witnesses.  We split the idea of a line into three distinct
pieces: horizontals, verticals (implemented as horizontals on a transposed
board), and diagonals.  Diagonals come in two flavors, so we bundle them into a
sum type.

```haskell
!!!ttt/TicTacToe.hs "newtype Horiz" "newtype Vert" "data Diag" "data Line"
```

Now we need a witness that says a triple is all the same player.  Once we have
that, a victory is just a line whose triple is all `Just p`.  This is one of
those moments where the types do exactly what the English spec says.

```haskell
!!!ttt/TicTacToe.hs "data AllSame" "data Victory"
```

No Type Errors
Implementation steps for decision over booleans:

1.  Define `Decision a = Proved a | Disproved (a -> Void)`.
2.  Use it as the return type of every predicate-like function.
3.  Compose decisions structurally over `Triple` and `Board`.

This is the minimal machinery needed to avoid `TypeError` and keep proofs
explicit.
--------------

The usual type-level style would be to define a type family that throws a
`TypeError` on illegal moves.  I do not like that style because it bakes
failure into the type family and makes reasoning harder.  It is a dead end:
you learn that something went wrong, but you do not get back a structured
witness you can use elsewhere.

Instead, we decide things.  We either produce a witness or we produce a
refutation.  This is the small core of the *decidable* style, but written out
explicitly so you can see it.

```haskell
!!!ttt/TicTacToe.hs "class SingKind" "class SDecide"
```

From there we build decision procedures for rows, columns, and diagonals, and
finally a decision for the entire board.  This is exactly the point of the
exercise: the decision does not just return `True` or `False`, it returns a
*proof* or a *refutation*.

```haskell
!!!ttt/TicTacToe.hs "decideDiagonal" "decideDiag" "decideVictorySing"
```

### Decision Functions

It's worth stressing the difference between a decision procedure and a boolean
predicate.  A boolean can only tell you *that* something happened.  A decision
procedure gives you a structured witness that you can carry forward in the rest
of the program.

That's why `Decision` is so central here, and why we keep lifting decisions
over the board structure instead of collapsing them to `Bool`.

#### Deciding SelFound

One small example of this is the "pick one of three" helper.  We can decide
over a triple by trying the first item, then the second, then the third, and
carry forward which one succeeded as a witness.

```haskell
!!!ttt/TicTacToe.hs "decideAny3"
```

### Existential return types and Dependent Pairs

Sometimes we don't just want to know *that* a move exists; we want the board it
produces as well.  For that we use dependent pairs (`DSum`) to return a board
along with a witness that it was obtained legally.

```haskell
!!!ttt/TicTacToe.hs "DSum" "PlayAt"
```

This is the same idea as the classic dependent pair pattern: a value paired
with a proof that the value has some property.

### Proving functions

Most of the functions in this file are not algorithms so much as proof
generators.  `decideRowAllSame` proves a row is all one player, `decideHoriz`
and `decideVert` lift that to the board, and `decideVictorySing` composes the
results.

```haskell
!!!ttt/TicTacToe.hs "decideRowAllSame" "decideHoriz" "decideVert"
```

### Proving Pick

The other half of the story is picking a legal move.  The `playAt` decision
either constructs a `PlayAt` witness or provides a refutation that no such
move can exist at that coordinate.

```haskell
!!!ttt/TicTacToe.hs "playAt" "Replace3"
```

The Game GADT
Implementation steps for `Game`:

1.  Start with a base constructor `Start` for the empty board.
2.  Add `AddMove` that requires a `Play` witness and a `NoWinner` witness.
3.  Thread the player turn through the type with `NextPlayer`.

The constructor list *is* the specification.  There are no hidden rules.
-------------

Now we can transcribe the spec into a GADT.

```haskell
!!!ttt/TicTacToe.hs "type NoWinner" "data Game"
```

This seems nice, but again, it hides the main point.  The real point is that
the *only* way to build a `Game` is by following the two rules from the spec.
The `Start` constructor is rule (1).  The `AddMove` constructor is rule (2).
The `Play` witness enforces legal moves, and the `NoWinner` witness enforces
that we do not keep playing on a won board.

So the game state is not "type safe" because we said so; it is type safe
because there is literally no other way to construct it.

One way to read this is as a tiny proof system.  A `Game` value is a proof that
the game is valid, and the constructors are the inference rules.  This is the
same mental model as earlier: we do not *check* validity after the fact, we
*construct* validity in the first place.

This is also why I like GADTs for this sort of thing.  You can look at the
constructors and read off the specification directly.  The type itself is the
documentation.

The Runner
Implementation steps for the runner:

1.  Render the board by reflecting the singleton to a value.
2.  Parse the user's move into an `Ix` pair.
3.  Ask `playAt` for a witness and branch on the result.
4.  If you get a witness, build `AddMove` and recurse.

Every step is boring on purpose; the type layer already did the hard work.
----------

The IO runner is intentionally boring.  It does not need to implement the rules
because the rules already live in the types.  It calls `playAt`, and if that
returns a proof, it can advance the game.  Otherwise it is stuck.

Notice the direction of control: the runtime program does not *check* if a move
is valid, it *asks* the type-level logic for a witness.  If it gets one, it can
continue.  If it does not, it simply cannot build the next state.

```haskell
!!!ttt/Main.hs "loop ::" "playAt" "AddMove"
```

This is exactly the kind of situation where "illegal states unrepresentable"
actually means something: the runtime code is small because the type-level code
already did the work.

If you want to play with it, load `Main.hs` and try a few moves.  The boring
part is the point: once the type-level core is correct, the IO layer becomes a
thin shell that just connects user input to proofs.

Where This Goes
If you want to extend this, the natural next step is to add more invariants to
`Game`: move counts, alternating turns across draws, or even a proof that every
move preserves `NoWinner` until the end.  Each extension is just another witness
and another constructor rule.
---------------

This is obviously overkill for tic tac toe, but it is excellent practice for
pushing invariants into types.  It forces you to be precise about your rules,
and it teaches you to carry proofs around as values.

Once you do it for a toy example, you start to see where the same idea can be
applied in real code.  That is the real payoff.

In particular, the lesson is not "always use dependent types".  The lesson is
that when you *do* choose to enforce an invariant, you want the enforcement to
be structural and compositional.  That is exactly what the GADT/witness style
buys you here.

[decision-blind]: https://twitter.com/cattheory/status/887760004622757890
[boolean blindness]: https://existentialtype.wordpress.com/2011/03/15/boolean-blindness/

Full Boards and Outcomes
Implementation steps for draws:

1.  Define `FullCell` as a witness that a cell is `Just`.
2.  Lift it to rows (`Prod3 FullCell`) and boards (`Full`).
3.  Combine `NoWinner` and `Full` into `decideOutcome`.

This gives the runner a three-way decision: win, draw, or continue.
------------------------

So far we have talked about legal moves and winning lines, but a full game also
needs to account for draws.  In normal tic tac toe, a draw means that the board
is full and no player has won.  In the type-level story, that means we need two
separate witnesses:

1.  A witness that there is no winning line
2.  A witness that every cell is full

The first witness is `NoWinner`, which is just a refutation of `Victory Line`.
The second witness is `Full`, which is built by a proof that each cell is
`Just p` for some player.

This is a good example of why we bother with witnesses at all.  If we only had
`Bool`, we would have to remember and manually check that both conditions are
true.  With witnesses, we can carry both proofs at the type level and let the
compiler do the bookkeeping for us.

```haskell
!!!ttt/TicTacToe.hs "data FullCell" "type Full" "decideFull"
```

A full board is just a product of full rows, and a full row is just a product
of full cells.  That means `decideFull` is built by lifting the same small
decision procedure over a `Triple` twice.  It is repetitive, but it is also
compositional, which is the main thing we care about.

Once we have both `decideVictorySing` and `decideFull`, we can define a single
outcome decision that answers the most important question for the runner:

*Is the game over?  If so, is it a win or a draw?*

```haskell
!!!ttt/TicTacToe.hs "decideOutcome"
```

Notice that the outcome decision returns *witnesses*, not booleans.  If the
result is a win, we get the line and the player.  If the result is a draw, we
get a proof that the board is full.  That means the IO runner never needs to
re-check anything: it just branches on the proof object it was given.

From Line to Victory
Implementation steps for victory lifting:

1.  Decide a line in a specific orientation.
2.  Wrap it into `Victory`.
3.  Use `liftHoriz`/`liftVert`/`liftDiag` to lift to the general `Line` form.

This is a pattern for moving from specific witnesses to generic witnesses.
--------------------

Another place where witnesses pay off is in the path from line selection to
victory.  We do not just want to know that a line exists; we want to know *which
line* exists, and for *which player*.

This is why `Victory` is indexed by both the line type and the player.  A
`Victory line board p` is a certificate that there is a line of `Just p` values
on the board.  That line can be horizontal, vertical, or diagonal, and the
`Line` GADT is the value-level witness that remembers which one it was.

```haskell
!!!ttt/TicTacToe.hs "data Line" "data Victory" "liftHoriz" "liftVert" "liftDiag"
```

The helper functions `liftHoriz`, `liftVert`, and `liftDiag` are small, but they
encode a key idea: we can build a generic victory witness from a more specific
line witness.  This is exactly the kind of thing that is awkward with a `Bool`
representation but trivial with GADTs.

Equality at the Type Level
Implementation steps for equality:

1.  Define a singleton for each kind you need.
2.  Give an `SDecide` instance that returns `Refl` or refutation.
3.  Use those instances whenever you need to compare singleton values.

The important part is that equality itself is a witness, not a boolean.
--------------------------

Any time you use witnesses, you eventually need to compare them.  Here, we do
that with a simple `SDecide` class, which lets us decide equality of singleton
values and get back either `Refl` or a refutation.

```haskell
!!!ttt/TicTacToe.hs "class SDecide" "instance SDecide Player" "instance SDecide Ix"
```

The structure is deliberately small and local.  We do not need a giant generic
framework; we only need equality for the few types we care about.  This is the
smallest amount of machinery that makes the rest of the proof story possible.

Singletons, but Small
Implementation steps for the singleton layer:

1.  Define `SingKind` with `Sing`, `fromSing`, and `withSing`.
2.  Provide instances for `Player`, `Ix`, `Maybe`, and `Triple`.
3.  Use `withSing` to bridge from values into the type-indexed world.

This is the smallest possible singleton layer that still supports the runner.
---------------------

The singletons-style approach is usually packaged in a library, but for this
post it is instructive to write the tiny version by hand.  The `SingKind`
class gives us two things:

1.  A singleton type for each kind
2.  A way to reflect a singleton back to a value

```haskell
!!!ttt/TicTacToe.hs "class SingKind" "fromSing" "withSing"
```

The key is that we do not need the full `singletons` machinery.  We only need
just enough to *name* a type at the value level so that we can carry witnesses
through the IO runner.  Everything else can be done with ordinary GADTs.

Rendering the Board
Implementation steps for rendering:

1.  Reflect `Sing board` into a value `Board` with `fromSing`.
2.  Render rows by mapping `Maybe Player` to characters.
3.  Assemble the lines into a grid.

The renderer is intentionally plain, because its correctness is already
inherited from the type-level core.
-------------------

On the IO side, the board is rendered by reflecting the singleton back to a
value and then printing it as a grid.  This is a good example of the value/type
boundary: the type enforces legality, but the value decides how to present it.

```haskell
!!!ttt/Main.hs "renderBoard" "renderRow" "renderCell"
```

The renderer is intentionally boring.  It does not need to be clever because
it cannot be wrong about the game state; the only states it can ever see are
valid ones.

Parsing Input
Implementation steps for parsing:

1.  Normalize user input (uppercase, strip spaces).
2.  Look up the coordinate in a precomputed table.
3.  Convert `Ix` values into singleton indices with `withSomeIx`.
4.  Ask `playAt` for a witness.

The parsing step is untyped; the witness step is typed.  That separation is
exactly what we want.
-------------

Input parsing is another place where the type-level approach shines.  We parse
string input into an `Ix` pair, and then we *ask* for a `PlayAt` witness.  If
we get one, we can move forward.  If we do not, we reject the move.

```haskell
!!!ttt/Main.hs "parseMove" "moveTable" "withSomeIx"
```

Notice that the parser itself does not have to know anything about legality.  It
just maps `"A1"` to `(A, A)` and so on.  The legality comes entirely from the
witness layer.

Minimax (AI)
------------

The runner also includes a tiny AI, just to show that the type-level core does
not get in the way of normal programming.  We can generate all possible moves
and score them with a basic minimax search.

Let's walk through the implementation step by step, because this is one of the
few places where the value-level logic and the proof machinery meet.

### Step 1: Enumerate candidate coordinates

The board is 3x3, so the space of possible coordinates is small and fixed.  We
just generate all pairs of indices.

```haskell
!!!ttt/Main.hs "allPairs"
```

At this point we have *candidates*, not legal moves.  We still need to ask the
type-level layer whether a given coordinate is playable.

### Step 2: Turn a coordinate into a legal move (or not)

This is the key: we call `playAt` with a singleton board and singleton indices.
If it succeeds, it returns a witness and the resulting board.  If it fails, we
discard the coordinate.

```haskell
!!!ttt/Main.hs "movesAt" "possibleMoves"
```

Notice how this mirrors the decision-view story earlier.  We do not *check* a
move; we *ask* for a witness.  If we get one, we keep it.  If we do not, that
coordinate is illegal.

### Step 3: Score a move by looking ahead

Minimax works by assuming both players are optimal and recursively exploring
future moves.  The scoring function is defined by three cases:

1.  If the board is a win for the current player, score `+1`.
2.  If the board is a win for the other player, score `-1`.
3.  If the board is a draw, score `0`.

That logic lives in `minimax` itself, which calls `decideOutcome` to determine
which case it is in.

```haskell
!!!ttt/Main.hs "minimax" "scorePlay"
```

The important part is that the board we are scoring is still a singleton, so
the decision procedure returns structured proofs rather than ad-hoc booleans.

### Step 4: Pick the best move

Once we can score a move, choosing the best move is just a fold over the legal
moves.

```haskell
!!!ttt/Main.hs "better" "bestMove"
```

This is intentionally boring.  The algorithm is plain minimax, and the only
special twist is that the move generator is proof-based.  That means the AI can
never choose an illegal move, because it cannot even *see* one.

```haskell
!!!ttt/Main.hs "possibleMoves" "minimax" "bestMove"
```

This is not meant to be clever AI.  It is just a simple minimax search that
tries all legal moves.  The important part is that the move generator is also
proof-based: it cannot produce an illegal move, because it uses `playAt` under
the hood.

Complexity Notes
Implementation steps for keeping complexity reasonable:

1.  Keep the board shape fixed and small.
2.  Use direct witnesses instead of generic type-level arithmetic.
3.  Push complexity into a few reusable decision combinators (`decideAny3`,
    `decideAll3`).

This is how you keep the proof burden manageable in practice.
----------------

This approach is not "free".  A pure witness-based system can be verbose, and
it can be slower to write than a boolean-based system.  But the tradeoff is
that once the witnesses exist, the rest of the program becomes much simpler.

That is the theme of this post: pay up front in the core logic, and then let the
rest of the program be boring.

Exercises
If you try the exercises, do them one at a time and keep the witness types
explicit.  The trick is always the same: write down the rule in English, then
turn it into a constructor or a decision procedure.
---------

If you want to explore further, here are some natural extensions:

1.  Add a proof that the board always has the right number of Xs and Os.
2.  Add a proof that the player alternation is preserved even across draws.
3.  Add a proof that you cannot make a move after a win or a draw.
4.  Replace the `Triple` with a length-indexed vector and see how much more
    machinery you need.
5.  Extend the board to 4x4 and see how the witness types scale.

Each of these forces you to push the same ideas a little further, which is the
best way to internalize them.
