{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}

module Impossible where

import TicTacToe

-- EXAMPLE 1: testOccupiedCell
-- Demonstrates: Replace3 witnesses are impossible for occupied cells
-- Shows: Empty case when trying to replace an occupied cell
-- Type: IO ()

testOccupiedCell
    :: RowReplace p c ('T ('Just 'X) ('Just 'O) ('Just 'X)) row'
    -> a
testOccupiedCell = \case {}

-- EXAMPLE 2: whoStartsGame
-- Demonstrates: Start is the only constructor for EmptyBoard
-- Shows: With strict fields, GHC knows AddMove is impossible for EmptyBoard
-- Type: Game EmptyBoard p -> String
-- Note: Uses error due to redundant pattern warning

whoStartsGame :: Game EmptyBoard p -> String
whoStartsGame Start = "X starts the game"

-- EXAMPLE 3: whatWasReplacedA/B/C
-- Demonstrates: Replace3 proves the index
-- Shows: Only one Rep constructor is possible for each index type

whatWasReplacedA :: Replace3 ('T x b c) ('T y b c) 'A x y -> String
whatWasReplacedA RepA = "A"

whatWasReplacedB :: Replace3 ('T a x c) ('T a y c) 'B x y -> String
whatWasReplacedB RepB = "B"

whatWasReplacedC :: Replace3 ('T a b x) ('T a b y) 'C x y -> String
whatWasReplacedC RepC = "C"

-- EXAMPLE 4: replaceEmptyWithX
-- Demonstrates: Replace3 proves the old and new values
-- Shows: All three Rep constructors work when types match

replaceEmptyWithX :: Replace3 row row' c 'Nothing ('Just 'X) -> String
replaceEmptyWithX RepA = "A"
replaceEmptyWithX RepB = "B"
replaceEmptyWithX RepC = "C"

-- EXAMPLE 5: Empty cases for truly impossible types

impossibleEmptyCell :: FullCell 'Nothing -> a
impossibleEmptyCell = \case {}

impossibleAllSameEmpty
    :: AllSame ('T 'Nothing 'Nothing 'Nothing) p
    -> a
impossibleAllSameEmpty = \case {}

impossibleAllSameMixed
    :: AllSame ('T ('Just 'X) ('Just 'O) ('Just 'X)) p
    -> a
impossibleAllSameMixed = \case {}

-- EXAMPLE 6: checkNextPlayer
-- Demonstrates: NextPlayer is injective
-- Shows: Only 2 cases needed (SX-SO and SO-SX), others are impossible

checkNextPlayer :: SPlayer p -> SPlayer (NextPlayer p) -> String
checkNextPlayer SX SO = "X goes to O"
checkNextPlayer SO SX = "O goes to X"

-- EXAMPLE 7: describePlay
-- Demonstrates: Play witness structure
-- Shows: Play can only exist for legal moves

describePlay :: Play p r c board board' -> String
describePlay (Play _ _) = "Play"

-- EXAMPLE 8: describeVictory
-- Demonstrates: Victory structure
-- Shows: Only three ways to win (horizontal, vertical, diagonal)

describeVictory :: Victory Line board p -> String
describeVictory (Victory line) = case line of
    LineHoriz _ -> "horizontal"
    LineVert _ -> "vertical"
    LineDiag _ -> "diagonal"

-- EXAMPLE 9: describeDiagonal
-- Demonstrates: Diagonal has only one constructor

describeDiagonal :: Diagonal board diag -> String
describeDiagonal Diagonal = "diagonal"

-- EXAMPLE 10: whichElement
-- Demonstrates: Elem3 is exhaustive
-- Shows: Three constructors for three positions

whichElement :: Elem3 ('T a b c) x -> String
whichElement E3_A = "A"
whichElement E3_B = "B"
whichElement E3_C = "C"

-- EXAMPLE 11: whichDiag
-- Demonstrates: Diag has two constructors
-- Shows: Main diagonal and anti-diagonal

whichDiag :: Diag board l -> String
whichDiag (Diag1 _) = "main"
whichDiag (Diag2 _) = "anti"
