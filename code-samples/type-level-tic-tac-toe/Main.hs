{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Char (toUpper)
import Data.Dependent.Sum (DSum((:=>)))
import Data.Void (Void)
import TicTacToe

main :: IO ()
main = do
    putStrLn "Type-Level Tic-Tac-Toe (IO runner)"
    putStrLn "Enter moves as A1/B2/C3."
    putStrLn ""
    loop sEmptyBoard SX Start

loop :: Sing (board :: Board) -> SPlayer (p :: Player) -> Game board p -> IO ()
loop sboard sp g = do
    putStrLn (renderBoard sboard)
    case decideOutcome sboard of
        Proved (Left (spw :=> Victory _)) ->
            putStrLn ("Winner: " ++ showSPlayer spw)
        Proved (Right _) ->
            putStrLn "Draw."
        Disproved nw -> do
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

promptMove :: SPlayer (p :: Player) -> IO (Maybe (Ix, Ix))
promptMove sp = do
    putStr ("Player " ++ showSPlayer sp ++ " move: ")
    input <- getLine
    case parseMove input of
        Just mv -> pure (Just mv)
        Nothing -> do
            putStrLn "Invalid move. Use AA/AB/AC or A1/B2/C3."
            pure Nothing

parseMove :: String -> Maybe (Ix, Ix)
parseMove input =
    case filter (/= ' ') (map toUpper input) of
        [r, c] -> (,) <$> parseIx r <*> parseIxAlt c
        _ -> Nothing

parseIx :: Char -> Maybe Ix
parseIx c = case c of
    'A' -> Just A
    'B' -> Just B
    'C' -> Just C
    _ -> Nothing

parseIxAlt :: Char -> Maybe Ix
parseIxAlt c = case parseIx c of
    Just ix -> Just ix
    Nothing -> case c of
        '1' -> Just A
        '2' -> Just B
        '3' -> Just C
        _ -> Nothing


renderBoard :: Sing (board :: Board) -> String
renderBoard (ST r1 r2 r3) =
    unlines
        [ renderRow r1
        , renderRow r2
        , renderRow r3
        ]

renderRow :: Sing (row :: Triple (Maybe Player)) -> String
renderRow (ST a b c) =
    unwords [renderCell a, renderCell b, renderCell c]

renderCell :: Sing (cell :: Maybe Player) -> String
renderCell cell = case cell of
    SNothing -> "."
    SJust SX -> "X"
    SJust SO -> "O"

nextSPlayer :: SPlayer (p :: Player) -> SPlayer (NextPlayer p)
nextSPlayer SX = SO
nextSPlayer SO = SX

showSPlayer :: SPlayer (p :: Player) -> String
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

withSomeIx :: Ix -> (forall (i :: Ix). Sing i -> r) -> r
withSomeIx ix f = case ix of
    A -> f SA
    B -> f SB
    C -> f SC

sEmptyBoard :: Sing EmptyBoard
sEmptyBoard =
    ST
        (ST SNothing SNothing SNothing)
        (ST SNothing SNothing SNothing)
        (ST SNothing SNothing SNothing)
