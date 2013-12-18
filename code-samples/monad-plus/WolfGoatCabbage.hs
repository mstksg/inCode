import Control.Monad (guard)
import Control.Applicative ((<$>))

-- Our types
data Character = Farmer | Wolf | Goat | Cabbage
        deriving (Show, Eq, Enum)

data Move = Move Character
        deriving (Eq)

instance Show Move where
    show (Move Farmer)  = "F"
    show (Move Wolf)    = "W"
    show (Move Goat)    = "G"
    show (Move Cabbage) = "C"

type Solution = [Move]

data Position = West | East
    deriving (Show, Eq, Ord, Enum)

startingSol :: Solution
startingSol = []

positionOf :: Solution -> Character -> Position
positionOf s c = case c of
    Farmer  -> countToPosition $ length s
    -- c       -> countToPosition $ length [ c' | Move c' <- s, c == c' ]
    c       -> countToPosition $ length $ filter (== Move c) s
    where
        countToPosition n | even n    = West
                          | otherwise = East

moveLegal :: Solution -> Move -> Bool
moveLegal s (Move Farmer) = True
moveLegal s (Move t) = positionOf s t == positionOf s Farmer

safeSolution :: Solution -> Bool
safeSolution s = safeGoat && safeCabbage || goatPos == farmerPos
    where
        goatPos     = positionOf s Goat
        farmerPos   = positionOf s Farmer
        safeGoat    = goatPos /= positionOf s Wolf
        safeCabbage = positionOf s Cabbage /= goatPos

makeMove :: Solution -> [Solution]
makeMove s = do
    next <- Move <$> [Farmer .. Cabbage]
    guard $ moveLegal s next
    let
        s' = s ++ [next]
    guard $ safeSolution s'
    return s'

-- finalState :: Solution -> [Position]
-- finalState s = map (positionOf s) [Farmer .. Cabbage]

isFinalSol :: Solution -> Bool
isFinalSol s = all (== East) positions
    where
        positions = map (positionOf s) [Farmer .. Cabbage]

findSolutions :: Int -> [Solution]
findSolutions n = do
    s <- makeNMoves
    guard $ isFinalSol s
    return s
    where
        makeNMoves = iterate (>>= makeMove) (return startingSol) !! n

main = print $ findSolutions 7
