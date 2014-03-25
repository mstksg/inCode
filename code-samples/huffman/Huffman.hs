module Huffman where

data PreTree a = PTLeaf a
               | PTNode (PreTree a) (PreTree a)
               deriving (Show, Eq)

makePT :: a -> PreTree a
makePT = PLeaf

mergePT :: PreTree a -> PreTree a -> PreTree a
mergePT = PNode

data Weighted a = WPair Int a
                deriving Show

type WPreTree a = Weighted (PreTree a)

makeWPT :: Int -> a -> WPreTree a
makeWPT w = WPair w . makePT

mergeWPT :: WPreTree a -> WPreTree a -> WPreTree a
mergeWPT (WPair w1 pt1) (WPair w2 pt2)
    = WPair (w1 + w2) (mergePT pt1 p2)

instance Ord (Weighted a) where
    compare (WPair w1 _) (WPair w2 _) = compare w1 w2

data SkewHeap a = SEmpty
                | SNode a (SkewHeap a) (SkewHeap a)
                deriving (Show, Eq)

makeSH :: a -> SkewHeap a
makeSH x = SNode x SEmpty SEmpty

popSH :: Ord a => SkewHeap a -> (Maybe a, SkewHeap a)
popSH SEmpty          = (Nothing, SEmpty)
popSH (SNode r h1 h2) = (Just r , mergeSH h1 h2)

mergeSH :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
mergeSH SEmpty h = h
mergeSH h SEmpty = h
mergeSH l@(SNode nl ll rl) r@(SNode nr lr rr)
    | nl < nr    = SNode nl (mergeSH lr r) ll
    | otherwise  = SNode nr (mergeSH rr l) lr

newtype PQueue a = PQ (SkewHeap a)

emptyPQ :: PQueue a
emptyPQ = PQ SEmpty

insertPQ :: Ord a => a -> PQueue a -> PQueue a
insertPQ x (PQ h) = PQ (mergeSH h (makeSH x))

popPQ :: Ord a => PQueue a -> (Maybe a, PQeueue a)
popPQ (PQ h) = (res, PQ h')
  where
    (res, h') = popSH h

sizePQ :: PQueue a -> Int
sizePQ (PQ h) = sizeSH h
  where
    sizeSH SEmpty  = 0
    sizeSH _ h1 h2 = 1 + sizeSH h1 + sizeSH h2
