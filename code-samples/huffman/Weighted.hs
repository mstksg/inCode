{-# LANGUAGE DeriveFunctor #-}
-- http://blog.jle.im/entry/streaming-huffman-compression-in-haskell-part-1-trees

module Weighted
  ( Weighted(..)
  ) where

-- | Weighted data
--
-- Weighted: Represents weighted data.  It's basically a glorified tuple
--      `(Int, a)`, with special Ord and Eq instances that compare only the
--      `Int`, and ignores the stored data.
data Weighted a = WPair { _wWeight :: Int
                        , _wItem   :: a
                        } deriving (Show, Functor)

instance Ord (Weighted a) where
    compare (WPair w1 _) (WPair w2 _) = compare w1 w2

instance Eq (Weighted a) where
    WPair w1 _ == WPair w2 _ = w1 == w2

