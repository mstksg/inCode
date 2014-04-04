{-# LANGUAGE DeriveGeneric #-}
-- http://blog.jle.im/entry/implementing-huffman-compression-encoding-in-haskell

module PreTree where

import GHC.Generics
import Data.Binary
import Control.Applicative ((<$>),(<*>))
import Weighted

-- | Prefix trees, used to implement Huffman encodings.
--
-- PrefixTree: The data type.
data PreTree a = PTLeaf a
               | PTNode (PreTree a) (PreTree a)
               deriving (Show, Eq, Generic)

-- instance Binary a => Binary (PreTree a)

instance Binary a => Binary (PreTree a) where
    put = putPT
    get = getPT

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

putPT :: Binary a => PreTree a -> Put
putPT (PTLeaf x) = do
    put True                    -- signify we have a leaf
    put x
putPT (PTNode pt1 pt2) = do
    put False                   -- signify we have a node
    put pt1
    put pt2

getPT :: Binary a => Get (PreTree a)
getPT = do
    isLeaf <- get
    if isLeaf
      then PTLeaf <$> get
      else PTNode <$> get <*> get
