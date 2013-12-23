import Control.Monad (guard)
import Control.Applicative ((<$>))

-- | Our types
data Character = Farmer | Wolf | Goat | Cabbage
    deriving (Show, Eq, Enum)

newtype Move = MoveThe Character
    deriving (Eq)

-- (just for debugging)
instance Show Move where
    show (MoveThe Farmer)  = "F"
    show (MoveThe Wolf)    = "W"
    show (MoveThe Goat)    = "G"
    show (MoveThe Cabbage) = "C"

type Plan = [Move]

data Position = West | East
    deriving (Show, Eq)

-- | Starting plan
startingPlan :: Plan
startingPlan = []

-- | The full journey
findSolutions :: Int -> [Plan]
findSolutions n = do
    p <- makeNMoves
    guard $ isSolution p
    return p
    where
        makeNMoves = iterate (>>= makeMove) (return startingPlan) !! n

-- | One step of the journey: add a move.
makeMove :: Plan -> [Plan]
makeMove p = do
    next <- MoveThe <$> [Farmer .. Cabbage]
    guard       $ moveLegal p next
    guard . not $ moveRedundant p next
    let
        p' = p ++ [next]
    guard $ safePlan p'
    return p'

-- | Helper functions
positionOf :: Plan -> Character -> Position
positionOf p c = case c of
    Farmer  -> positionFromCount . length $ p
    c       -> positionFromCount . length $ filter (== MoveThe c) p
    where
        positionFromCount n | even n    = West
                            | otherwise = East

moveLegal :: Plan -> Move -> Bool
moveLegal p (MoveThe Farmer)  = True
moveLegal p (MoveThe c)       = positionOf p c == positionOf p Farmer

moveRedundant :: Plan -> Move -> Bool
moveRedundant [] m' = False
moveRedundant p m   = last p == m

safePlan :: Plan -> Bool
safePlan p = goatPos == farmerPos || safeGoat && safeCabbage
    where
        goatPos     = positionOf p Goat
        farmerPos   = positionOf p Farmer
        safeGoat    = goatPos /= positionOf p Wolf
        safeCabbage = positionOf p Cabbage /= goatPos

isSolution :: Plan -> Bool
isSolution p = all (== East) positions
    where
        positions = map (positionOf p) [Farmer .. Cabbage]

-- | Main
main :: IO ()
main = do
    print $ findSolutions 7
    print $ findSolutions 13
