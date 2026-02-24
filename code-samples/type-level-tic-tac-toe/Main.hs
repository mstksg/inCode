{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Char (toUpper)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import Data.Dependent.Sum (DSum((:=>)))
import Data.Type.Equality ((:~:)(Refl))
import Data.List.NonEmpty (NonEmpty(..))
import TicTacToe

main :: IO ()
main = do
    putStrLn "Type-Level Tic-Tac-Toe (IO runner)"
    putStrLn "Enter moves as A1/B2/C3."
    putStrLn ""
    loop sEmptyBoard SX Start

loop :: Sing (board :: Board) -> SPlayer (p :: Player) -> Game board p -> IO ()
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
                            withSomeIx r \sr ->
                                withSomeIx c \sc ->
                                    case playAt sp sboard sr sc of
                                        Disproved _ -> do
                                            putStrLn "That cell is already taken. Try again."
                                            loop sboard sp g
                                        Proved (sboard' :=> PlayAt repRow repBoard) ->
                                            loop sboard' (nextSPlayer sp) (AddMove (Play repRow repBoard) (nw . Left) g)
                SO ->
                    case bestMove sp sboard of
                        Nothing -> putStrLn "No moves."
                        Just (p, sboard' :=> Play repRow repBoard) -> do
                            putStrLn ("AI plays " ++ showMove p)
                            loop sboard' (nextSPlayer sp) (AddMove (Play repRow repBoard) (nw . Left) g)

promptMove :: SPlayer (p :: Player) -> IO (Maybe (Ix, Ix))
promptMove sp = do
    putStr ("Player " ++ showSPlayer sp ++ " move: ")
    input <- getLine
    case parseMove input of
        Just mv -> pure (Just mv)
        Nothing -> do
            putStrLn "Invalid move. Use A1/B2/C3."
            pure Nothing

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

nextSPlayer :: SPlayer p -> SPlayer (NextPlayer p)
nextSPlayer SX = SO
nextSPlayer SO = SX

showSPlayer :: SPlayer p -> String
showSPlayer SX = "X"
showSPlayer SO = "O"

showIx :: Ix -> Char
showIx ix = case ix of
    A -> 'A'
    B -> 'B'
    C -> 'C'

showIxNum :: Ix -> Char
showIxNum ix = case ix of
    A -> '1'
    B -> '2'
    C -> '3'

showMove :: (Ix, Ix) -> String
showMove (r, c) = [showIx r, showIxNum c]

withSomeIx :: Ix -> (forall i. SIx i -> r) -> r
withSomeIx ix f = case ix of
    A -> f SA
    B -> f SB
    C -> f SC

bestMove
    :: SPlayer p
    -> SBoard board
    -> Maybe ((Ix, Ix), DSum Sing (Play p board))
bestMove sp sboard = case possibleMoves sp sboard of
    [] -> Nothing
    m:ms -> Just (foldl' (better sp) m ms)

better
    :: SPlayer p
    -> ((Ix, Ix), DSum Sing (Play p board))
    -> ((Ix, Ix), DSum Sing (Play p board))
    -> ((Ix, Ix), DSum Sing (Play p board))
better sp m1@(_, b1) m2@(_, b2)
  | scorePlay sp b2 > scorePlay sp b1 = m2
  | otherwise = m1

scorePlay
    :: SPlayer p
    -> DSum Sing (Play p board)
    -> Int
scorePlay sp (board' :=> _) = negate (minimax (nextSPlayer sp) board')

minimax :: SPlayer p -> SBoard board -> Int
minimax sp sboard =
    case decideOutcome sboard of
        Proved (Left (spw :=> Victory _)) ->
            case decidePlayerEq sp spw of
                Proved Refl -> 1
                Disproved _ -> -1
        Proved (Right _) -> 0
        Disproved _ ->
            case possibleMoves sp sboard of
                [] -> 0
                m:ms -> maximum $ scorePlay sp . snd <$> (m :| ms)

possibleMoves
    :: SPlayer p
    -> Sing board
    -> [((Ix, Ix), DSum Sing (Play p board))]
possibleMoves sp sboard =
    mapMaybe (movesAt sp sboard) allPairs

movesAt
    :: SPlayer p
    -> Sing board
    -> (Ix, Ix)
    -> Maybe ((Ix, Ix), DSum Sing (Play p board))
movesAt sp sboard p@(r, c) =
    withSomeIx r \sr ->
        withSomeIx c \sc ->
            case playAt sp sboard sr sc of
                Proved (board' :=> PlayAt repRow repBoard) ->
                    Just (p, board' :=> Play repRow repBoard)
                Disproved _ -> Nothing

allPairs :: [(Ix, Ix)]
allPairs = (,) <$> [A, B, C] <*> [A, B, C]
