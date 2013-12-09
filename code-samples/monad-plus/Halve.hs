-- `halve` returns a successful halving in a Just if the number is even, or
-- a Nothing otherwise.  This is a naive implementation that does not take
-- advantage of MonadPlus-ness.
--
-- To load:
-- $ ghci
-- λ: :l Halve.hs
-- 
-- Some things to play around with:
-- λ: halve 7
-- λ: halve 8 >>= halve
-- λ: halve 7 >>= halve
-- λ: halve 6 >>= halve
-- λ: halve 6 >>= halve >>= halve
-- λ: halve 32 >> Nothing
-- λ: halve 32 >>= halve >>= halve >>= halve
-- λ: halve 32 >> Nothing >>= halve >>= halve >>= halve
--
-- http://blog.jle.im/entry/practical-fun-with-monads-introducing-monadplus#maybe-maybe-not

halve :: Int -> Maybe Int
halve x | even x    = Just (x `div` 2)
        | otherwise = Nothing

