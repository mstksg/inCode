{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
-- http://blog.jle.im/entry/streaming-huffman-compression-in-haskell-part-1-trees

module PQueue
  ( PQueue
  , emptyPQ
  , insertPQ
  , popPQ
  , sizePQ
  -- normally we would not export these; here just for testing.
  , SkewHeap(..), makeSH, popSH, mergeSH
  ) where

import Data.Foldable

-- | (Internal) SkewHeap
--
-- SkewHeap: The data type
data SkewHeap a = SEmpty
                | SNode a (SkewHeap a) (SkewHeap a)
                deriving (Show, Eq, Foldable)

-- makeSH: Make a SkewHeap from a single item.
makeSH :: a -> SkewHeap a
makeSH x = SNode x SEmpty SEmpty

-- popSH: Pops the root (highest priority item) out of a SkewHeap and
--      returns `Just r` if the SkewHeap is not empty, and `Nothing` if it
--      is. Also returns the resulting modified SkewHeap.
popSH :: Ord a => SkewHeap a -> (Maybe a, SkewHeap a)
popSH SEmpty          = (Nothing, SEmpty)
popSH (SNode r h1 h2) = (Just r , mergeSH h1 h2)

-- mergeSH: Merges two SkewHeaps, preserving heap ordering.  Lower weight
-- means higher priority.
mergeSH :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
mergeSH SEmpty h = h
mergeSH h SEmpty = h
mergeSH hA@(SNode xA lA rA) hB@(SNode xB lB rB)
    | xA < xB    = SNode xA (mergeSH rA hB) lA
    | otherwise  = SNode xB (mergeSH rB hA) lB

-- | External PQueue (Priority Queue) interface
--
-- PQueue: Newtype wrapper around a SkewHeap, for the purposes of keeping
--      the implementaiton opaque.
newtype PQueue a = PQ (SkewHeap a) deriving Show

-- emptyPQ: Create a new empty priority queue.
emptyPQ :: PQueue a
emptyPQ = PQ SEmpty

-- insertPQ: Insert an item into a prioritiy queue, and return the new
--      priority queue.
insertPQ :: Ord a => a -> PQueue a -> PQueue a
insertPQ x (PQ h) = PQ (mergeSH h (makeSH x))

-- popPQ: Attempts to pop the highest-priority element out of the priority
--      queue.  Returns `Nothing` if the queue is empty and `Just x`, with
--      the highest priority element, if it is not.  Also returns the
--      modified queue.
popPQ :: Ord a => PQueue a -> (Maybe a, PQueue a)
popPQ (PQ h) = (res, PQ h')
  where
    (res, h') = popSH h

-- sizePQ: Returns the size of the given priority queue.  Uses `toList`
--      from Data.Foldable
sizePQ :: PQueue a -> Int
sizePQ (PQ h) = length (toList h)
