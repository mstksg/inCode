-- `halve` returns a successful halving in a Just if the number is even, or
-- a Nothing otherwise.  This is an alternative implementation using
-- `guard` and mzero, taking advantage of MonadPlus-ness.
--
-- To load:
-- $ ghci
-- λ: :i HalveGuard.hs
--
-- Some things to play around with:
-- λ: halve 7
-- λ: halve 8 >>= halve
-- λ: halve 7 >>= halve
-- λ: halve 6 >>= halve
-- λ: halve 6 >>= halve >>= halve
-- λ: halve 32 >> mzero
-- λ: halve 32 >>= halve >>= halve >>= halve
-- λ: halve 32 >> mzero >>= halve >>= halve >>= halve
--
-- http://blog.jle.im/entry/practical-fun-with-monads-introducing-monadplus#guards

halve :: Int -> Maybe Int
halve n = do
    guard $ even n
    return $ n `div` 2

