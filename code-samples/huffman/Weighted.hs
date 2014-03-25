module Weighted where

data Weighted a = WPair Int a
                deriving Show

instance Ord (Weighted a) where
    compare (WPair w1 _) (WPair w2 _) = compare w1 w2

