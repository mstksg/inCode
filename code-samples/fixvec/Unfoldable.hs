module Unfoldable where

import Data.Traversable -- required before GHC 7.10

class Unfoldable v where
    unfold :: (b -> (a, b)) -> b -> v a

-- always produces an infinite list
instance Unfoldable [] where
    unfold f x0 = let (y, x1) = f x0
                  in  y : unfold f x1

replicateU :: Unfoldable v => a -> v a
replicateU = unfold (\x -> (x, x))

iterateU :: Unfoldable v => (a -> a) -> a -> v a
iterateU f = unfold (\x -> (x, f x))

fromListMaybes :: Unfoldable v => [a] -> v (Maybe a)
fromListMaybes = unfold $ \l -> case l of
                                  []   -> (Nothing, [])
                                  x:xs -> (Just x , xs)

fromListU :: (Unfoldable v, Traversable v) => [a] -> Maybe (v a)
fromListU = sequence . fromListMaybes
