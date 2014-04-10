{-# LANGUAGE DeriveGeneric #-}
-- http://blog.jle.im/entry/implementing-huffman-compression-encoding-in-haskell

module PreTree where

import Control.Applicative               ((<$>),(<*>),(<|>))
import Control.Monad.Trans.Writer.Strict
import Data.List (unfoldr)
import Data.Binary
import Data.Map.Strict                   (Map)
import GHC.Generics
import Weighted
import qualified Data.Map.Strict         as M

-- | Prefix trees, used to implement Huffman encodings.
--
-- PrefixTree: The data type.
data PreTree a = PTLeaf a
               | PTNode (PreTree a) (PreTree a)
               deriving (Show, Eq, Generic)

data Direction = DLeft
               | DRight
               deriving (Show, Eq)

type Encoding = [Direction]

instance Binary a => Binary (PreTree a) where
    put = putPT
    get = getPT

-- instance Binary a => Binary (PreTree a)

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

findPT :: Eq a => PreTree a -> a -> Maybe Encoding
findPT pt0 x = go pt0 []
  where
    go (PTLeaf y      ) enc | x == y    = Just (reverse enc)
                            | otherwise = Nothing
    go (PTNode pt1 pt2) enc = go pt1 (DLeft  : enc) <|>
                              go pt2 (DRight : enc)

ptTable :: Ord a => PreTree a -> Map a Encoding
ptTable pt = execWriter (go pt [])
  where
    go (PTLeaf x) enc       = tell (M.singleton x (reverse enc))
    go (PTNode pt1 pt2) enc = go pt1 (DLeft  : enc) >>
                              go pt2 (DRight : enc)

lookupPTTable :: Ord a => Map a Encoding -> a -> Maybe Encoding
lookupPTTable = flip M.lookup

encodeAll :: Ord a => PreTree a -> [a] -> Maybe Encoding
encodeAll pt xs = concat <$> sequence (map (lookupPTTable tb) xs)
  where
    tb = ptTable pt

decodePT :: PreTree a -> Encoding -> Maybe (a, Encoding)
decodePT (PTLeaf x)       ds     = Just (x, ds)
decodePT (PTNode pt1 pt2) (d:ds) = case d of
                                     DLeft  -> decodePT pt1 ds
                                     DRight -> decodePT pt2 ds
decodePT (PTNode _ _)     []     = Nothing

decodeAll :: PreTree a -> Encoding -> [a]
decodeAll pt = unfoldr (decodePT pt)

decodeAll' :: PreTree a -> Encoding -> Maybe [a]
decodeAll' (PTLeaf _) = const Nothing
decodeAll' pt         = Just . unfoldr (decodePT pt)

