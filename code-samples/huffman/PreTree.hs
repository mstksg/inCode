module PreTree where

import Weighted

-- | Prefix trees, used to implement Huffman encodings.
--
-- PrefixTree: The data type.
data PreTree a = PTLeaf a
               | PTNode (PreTree a) (PreTree a)
               deriving (Show, Eq)

makePT :: a -> PreTree a
makePT = PTLeaf

mergePT :: PreTree a -> PreTree a -> PreTree a
mergePT = PTNode

type WeightedPT a = Weighted (PreTree a)

makeWPT :: Int -> a -> WeightedPT a
makeWPT w = WPair w . makePT

mergeWPT :: WeightedPT a -> WeightedPT a -> WeightedPT a
mergeWPT (WPair w1 pt1) (WPair w2 pt2)
    = WPair (w1 + w2) (mergePT pt1 pt2)
