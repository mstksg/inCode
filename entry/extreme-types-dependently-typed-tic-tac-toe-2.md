Extreme Types: Dependently Typed Tic Tac Toe (Part 2)
=====================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/extreme-types-dependently-typed-tic-tac-toe-2.html)

## Recap

TODO: brief summary of Part 1 and why we need decisions/views.

## Decision Functions and Views

TODO: introduce Decision/Refute pattern and the need for structured outcomes.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/TicTacToe.hs#L133-L133

data Decision a = Proved a | Disproved (a -> Void)
```

## Singleton Bridge

TODO: SingKind/SingI, withSing, reflection; minimal singleton layer.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/TicTacToe.hs#L27-L487

class SingKind k where
    data Sing (a :: k) :: Type
    fromSing :: Sing (a :: k) -> k
    withSing :: k -> (forall a. Sing (a :: k) -> r) -> r

class SingI a where
    sing :: Sing a
```

## Existentials (DSum) and SomePlay

TODO: explain why we need existential result boards and how DSum solves it.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/TicTacToe.hs#L108-L175

data SomePlay :: Player -> Board -> Board -> Type where
    SomePlay
        :: forall p r c board board'.
           Sing r
        -> Sing c
        -> Play p r c board board'
        -> SomePlay p board board'

type NoWinner board = DSum Sing (Victory Line board) -> Void
```

## RowReplace and replaceRow

TODO: walk through RowReplace, explicit result row, and DSum return.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/TicTacToe.hs#L184-L339

    RowReplace
        :: !(Replace3 row row' c 'Nothing ('Just p))
        -> RowReplace p c row row'

replaceRow
    :: SPlayer p
    -> SIx c
    -> Sing row
    -> Decision (DSum Sing (RowReplace p c row))
replaceRow sp sc (ST a b c) =
    case sc of
        SA -> case a of
            SNothing -> Proved (ST (SJust sp) b c :=> RowReplace RepA)
            SJust _ -> Disproved (\(_ :=> r) -> case r of {})
        SB -> case b of
            SNothing -> Proved (ST a (SJust sp) c :=> RowReplace RepB)
            SJust _ -> Disproved (\(_ :=> r) -> case r of {})
        SC -> case c of
            SNothing -> Proved (ST a b (SJust sp) :=> RowReplace RepC)
            SJust _ -> Disproved (\(_ :=> r) -> case r of {})
```

## playAt as a Decision

TODO: show playAt workflow and "views" of coordinates.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/TicTacToe.hs#L291-L322

playAt
    :: SPlayer p
    -> SBoard board
    -> SIx r
    -> SIx c
    -> Decision (DSum Sing (Play p r c board))
playAt sp (ST r1 r2 r3) sr sc =
    case sr of
        SA ->
            case replaceRow sp sc r1 of
                Disproved r -> Disproved (\case
                    _ :=> Play RepA rep ->
                        r (replaceRowSing sp r1 rep :=> RowReplace rep))
                Proved (r1' :=> RowReplace repRow) ->
                    let board' = ST r1' r2 r3
                    in Proved (board' :=> Play RepA repRow)
        SB ->
            case replaceRow sp sc r2 of
                Disproved r -> Disproved (\case
                    _ :=> Play RepB rep ->
                        r (replaceRowSing sp r2 rep :=> RowReplace rep))
                Proved (r2' :=> RowReplace repRow) ->
                    let board' = ST r1 r2' r3
                    in Proved (board' :=> Play RepB repRow)
        SC ->
            case replaceRow sp sc r3 of
                Disproved r -> Disproved (\case
                    _ :=> Play RepC rep ->
                        r (replaceRowSing sp r3 rep :=> RowReplace rep))
                Proved (r3' :=> RowReplace repRow) ->
                    let board' = ST r1 r2 r3'
                    in Proved (board' :=> Play RepC repRow)
```

## Trying It Out

TODO: ghci demo of playAt on empty board and a failing move.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/TicTacToe.hs#L57-L322

type EmptyBoard =
  'T
    ('T 'Nothing 'Nothing 'Nothing)
    ('T 'Nothing 'Nothing 'Nothing)
    ('T 'Nothing 'Nothing 'Nothing)

playAt
    :: SPlayer p
    -> SBoard board
    -> SIx r
    -> SIx c
    -> Decision (DSum Sing (Play p r c board))
playAt sp (ST r1 r2 r3) sr sc =
    case sr of
        SA ->
            case replaceRow sp sc r1 of
                Disproved r -> Disproved (\case
                    _ :=> Play RepA rep ->
                        r (replaceRowSing sp r1 rep :=> RowReplace rep))
                Proved (r1' :=> RowReplace repRow) ->
                    let board' = ST r1' r2 r3
                    in Proved (board' :=> Play RepA repRow)
        SB ->
            case replaceRow sp sc r2 of
                Disproved r -> Disproved (\case
                    _ :=> Play RepB rep ->
                        r (replaceRowSing sp r2 rep :=> RowReplace rep))
                Proved (r2' :=> RowReplace repRow) ->
                    let board' = ST r1 r2' r3
                    in Proved (board' :=> Play RepB repRow)
        SC ->
            case replaceRow sp sc r3 of
                Disproved r -> Disproved (\case
                    _ :=> Play RepC rep ->
                        r (replaceRowSing sp r3 rep :=> RowReplace rep))
                Proved (r3' :=> RowReplace repRow) ->
                    let board' = ST r1 r2 r3'
                    in Proved (board' :=> Play RepC repRow)
```

## Type-Safe Play

TODO: connect Play witness to Game constructor (without victory yet).

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/TicTacToe.hs#L111-L116

data Game :: Board -> Player -> Type where
    Start :: Game EmptyBoard X
    AddMove :: !(Play p r c board board')
            -> !(NoWinner board)
            -> !(Game board p)
            -> Game board' (NextPlayer p)

    AddMove :: !(Play p r c board board')
            -> !(NoWinner board)
            -> !(Game board p)
            -> Game board' (NextPlayer p)
```

## Finale

TODO: the moment "runtime can't cheat; either you get a witness or you don't."

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

