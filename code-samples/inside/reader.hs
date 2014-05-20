-- Load it up inside GHCI and give follow along with the post!
--
-- ghci> :l reader.hs
--
-- Before starting anything:
--
-- ghci> :set -XNoMonomorphismRestriction
--
-- to get rid of many potential frustrations.
--
-- http://blog.jle.im/entry/inside-my-world

module InsideReader where

import Control.Monad.Trans.Reader

-- futureLength: A future `Int` that will be the length of whatever the
--      list it is waiting for will be.
futureLength :: (Reader [a]) Int

-- futureHead: An future `a` that will be the first element of whatever the
--      list it is waiting for will be.
futureHead   :: (Reader [a]) a

-- futureOdd: A future `Bool` that will be whether the `Int` it is waiting
--      for is odd or not.
futureOdd    :: (Reader Int) Bool

-- secret implementations!
futureLength = reader length
futureHead   = reader head
futureOdd    = reader odd

futureShorterThan :: Int -> (Reader [a]) Bool
futureShorterThan n = fmap (< n) futureLength

futureShorterThan5 :: (Reader [a]) Bool
futureShorterThan5 = futureShorterThan 5

futureShorterThanHead :: (Reader [Int]) Bool
futureShorterThanHead = futureShorterThan =<< futureHead
