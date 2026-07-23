Extreme Types: Dependently Typed Tic Tac Toe (Part 4)
=====================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/extreme-types-dependently-typed-tic-tac-toe-4.html)

## Recap

TODO: brief summary of Part 3 and what's left (runner + AI).

## Rendering

TODO: renderBoard/row/cell; reflect singleton to value.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Main.hs#L78-L94

renderBoard :: Board -> String
renderBoard (T r1 r2 r3) =
    unlines
        [ renderRow r1
        , renderRow r2
        , renderRow r3
        ]

renderRow :: Triple (Maybe Player) -> String
renderRow (T a b c) =
    unwords [renderCell a, renderCell b, renderCell c]

renderCell :: Maybe Player -> String
renderCell cell = case cell of
    Nothing -> "."
    Just X -> "X"
    Just O -> "O"
```

## Parsing Input

TODO: parseMove + map lookup + withSing (tuple) to get indices.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Main.hs#L64-L75

parseMove :: String -> Maybe (Ix, Ix)
parseMove input = Map.lookup key moveTable
  where
    key = filter (/= ' ') (map toUpper input)

moveTable :: Map.Map String (Ix, Ix)
moveTable =
    Map.fromList
        [ ([r, n], (row, col))
        | (r, row) <- [('A', A), ('B', B), ('C', C)]
        , (n, col) <- [('1', A), ('2', B), ('3', C)]
        ]
```

## Main Loop

TODO: decideOutcome branching and AddMove construction.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Main.hs#L25-L52

loop :: SBoard board -> SPlayer p -> Game board p -> IO ()
loop sboard sp g = do
    putStrLn $ renderBoard $ fromSing sboard
    case decideOutcome sboard of
        Proved (Left (spw :=> Victory _)) ->
            putStrLn ("Winner: " ++ showSPlayer spw)
        Proved (Right _) ->
            putStrLn "Draw."
        Disproved nw ->
            case sp of
                SX -> do
                    move <- promptMove sp
                    case move of
                        Nothing -> loop sboard sp g
                        Just (r, c) ->
                            withSing (r, c) \(STuple sr sc) ->
                                case playAt sp sboard sr sc of
                                    Disproved _ -> do
                                        putStrLn "That cell is already taken. Try again."
                                        loop sboard sp g
                                    Proved (sboard' :=> play) ->
                                        loop sboard' (nextSPlayer sp) (AddMove play (nw . Left) g)
                SO ->
                    case bestMove sp sboard of
                        Nothing -> putStrLn "No moves."
                        Just (sboard' :=> SomePlay sr sc play) -> do
                            putStrLn ("AI plays " ++ showMove (fromSing sr) (fromSing sc))
                            loop sboard' (nextSPlayer sp) (AddMove play (nw . Left) g)
```

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/TicTacToe.hs#L111-L289

decideOutcome
    :: SBoard board
    -> Decision (Either (DSum Sing (Victory Line board)) (Full board))
decideOutcome board =
    case decideVictorySing board of
        Proved v -> Proved (Left v)
        Disproved nv ->
            case decideFull board of
                Proved f -> Proved (Right f)
                Disproved nf -> Disproved \case
                    Left v -> nv v
                    Right f -> nf f

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

## Move Generation

TODO: allPairs, movesAt, possibleMoves (proof-based move list).

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Main.hs#L159-L179

allPairs :: [(Ix, Ix)]
allPairs = (,) <$> [A, B, C] <*> [A, B, C]

movesAt
    :: SPlayer p
    -> Sing board
    -> (Ix, Ix)
    -> Maybe (DSum Sing (SomePlay p board))
movesAt sp sboard pos =
    withSing pos \(STuple sr sc) ->
        case playAt sp sboard sr sc of
            Proved (board' :=> play) ->
                Just (board' :=> SomePlay sr sc play)
            Disproved _ -> Nothing

possibleMoves
    :: SPlayer p
    -> Sing board
    -> [DSum Sing (SomePlay p board)]
possibleMoves sp sboard =
    mapMaybe (movesAt sp sboard) allPairs
```

## Negamax AI

TODO: step-by-step negamax; scorePlay, negamax recursion, bestMove.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/Main.hs#L119-L157

negamax :: SPlayer p -> SBoard board -> Int
negamax sp sboard =
    case decideOutcome sboard of
        Proved (Left (spw :=> Victory _)) ->
            case sp %~ spw of
                Proved Refl -> 1
                Disproved _ -> -1
        Proved (Right _) -> 0
        Disproved _ ->
            case possibleMoves sp sboard of
                [] -> 0
                m:ms ->
                    maximum
                        [ -negamax (nextSPlayer sp) board'
                        | board' :=> SomePlay _ _ _ <- m :| ms
                        ]

scorePlay
    :: SPlayer p
    -> DSum Sing (SomePlay p board)
    -> Int
scorePlay sp (board' :=> SomePlay _ _ _) = negate (negamax (nextSPlayer sp) board')

bestMove
    :: SPlayer p
    -> SBoard board
    -> Maybe (DSum Sing (SomePlay p board))
bestMove sp sboard = case possibleMoves sp sboard of
    [] -> Nothing
    m:ms -> Just (foldl' (better sp) m ms)
```

## Complexity Notes

TODO: discuss tradeoffs, why the runner stays boring.

## Exercises / Extensions

TODO: additional invariants and bigger boards.

## Finale

TODO: full interactive game + AI that cannot make illegal moves.

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

