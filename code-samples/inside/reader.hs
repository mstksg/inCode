-- Load it up inside GHCI and give follow along with the post!
--
-- λ: :l reader.hs
--
-- http://blog.jle.im/entry/inside-my-world

module InsideReader where

import Control.Monad.Trans.Reader

futureLength :: (Reader [a]) Int
futureLength = reader length

futureHead   :: (Reader [a]) a
futureHead   = reader head

futureOdd    :: (Reader Int) Bool
futureOdd    = reader odd

futureShorterThan :: Int -> (Reader [a]) Bool
futureShorterThan n = fmap (< n) futureLength

futureShorterThan5 :: (Reader [a]) Bool
futureShorterThan5 = futureShorterThan 5

futureShorterThanHead :: (Reader [Int]) Bool
futureShorterThanHead = futureShorterThan =<< futureHead
