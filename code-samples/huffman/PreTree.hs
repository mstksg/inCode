module PreTree where

import Weighted

data PreTree a = PTLeaf a
               | PTNode (PreTree a) (PreTree a)
               deriving (Show, Eq)

makePT :: a -> PreTree a
makePT = PTLeaf

mergePT :: PreTree a -> PreTree a -> PreTree a
mergePT = PTNode

type WPreTree a = Weighted (PreTree a)

makeWPT :: Int -> a -> WPreTree a
makeWPT w = WPair w . makePT

mergeWPT :: WPreTree a -> WPreTree a -> WPreTree a
mergeWPT (WPair w1 pt1) (WPair w2 pt2)
    = WPair (w1 + w2) (mergePT pt1 pt2)
