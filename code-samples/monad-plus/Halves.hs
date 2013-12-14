-- All of these versions of halve return a successful halving if the number
-- is even, or a failure otherwise.
--
-- * `halve` returns it in a Maybe container, where success is Just
--   x and failure is Nothing
-- * `halve'` returns it in a List, where success is [x] and failure is []
-- * `genericHalve` returns it whatever container you want, provided it is
--   a MonadPlus.  You can view all MonadPlus's currently loaded by typing
--   `:info MonadPlus` on ghci.
--
--
-- To load:
-- $ ghci
-- λ: :l Halves.hs
--
-- Some things to play around with:
-- λ: halve 8
-- λ: halve' 8
-- λ: genericHalve 8 :: Maybe Int
-- λ: genericHalve 8 :: [Int]
-- λ: halve 8                           -- and halve' and genericHalve
-- λ: halve 6 >>= halve                 -- etc.
-- λ: halve 32 >>= halve >>= halve >>= halve                -- etc.
-- λ: halve 32 >> mzero >>= halve >>= halve >>= halve       -- etc.
--
-- http://blog.jle.im/entry/practical-fun-with-monads-the-list-monadplus#starting-on-the-list-monad

import Control.Monad (guard, MonadPlus)

halve :: Int -> Maybe Int
halve n = do
    guard $ even n
    return $ n `div` 2

halve' :: Int -> [Int]
halve' n = do
    guard $ even n
    return $ n `div` 2

genericHalve :: MonadPlus m => Int -> m Int
genericHalve n = do
    guard $ even n
    return $ n `div` 2
