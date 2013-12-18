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

type Plan = [Move]

data Position = West | East
    deriving (Show, Eq, Ord, Enum)

startingPlan :: Plan
startingPlan = []

positionOf :: Plan -> Character -> Position
positionOf p c = case c of
    Farmer  -> countToPosition $ length p
    c       -> countToPosition $ length $ filter (== Move c) p
    where
        countToPosition n | even n    = West
                          | otherwise = East

moveLegal :: Plan -> Move -> Bool
moveLegal p (Move Farmer) = True
moveLegal p (Move t) = positionOf p t == positionOf p Farmer

safePlan :: Plan -> Bool
safePlan p = safeGoat && safeCabbage || goatPos == farmerPos
    where
        goatPos     = positionOf p Goat
        farmerPos   = positionOf p Farmer
        safeGoat    = goatPos /= positionOf p Wolf
        safeCabbage = positionOf p Cabbage /= goatPos

makeMove :: Plan -> [Plan]
makeMove p = do
    next <- Move <$> [Farmer .. Cabbage]
    guard $ moveLegal p next
    let
        p' = p ++ [next]
    guard $ safePlan p'
    return p'

-- finalState :: Plan -> [Position]
-- finalState s = map (positionOf s) [Farmer .. Cabbage]

isSolution :: Plan -> Bool
isSolution p = all (== East) positions
    where
        positions = map (positionOf p) [Farmer .. Cabbage]

findSolutions :: Int -> [Plan]
findSolutions n = do
    p <- makeNMoves
    guard $ isSolution p
    return p
    where
        makeNMoves = iterate (>>= makeMove) (return startingPlan) !! n

main = print $ findSolutions 7
