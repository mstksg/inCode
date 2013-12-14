-- `halveOrDouble` tries to halve or double a number, and presents the list
-- of successes.
--
-- Includes some compositions as well for playing around with.
--
-- To load:
-- $ ghci
-- λ: :l HalveOrDouble.hs
--
-- Some things to play around with:
-- λ: halveOrDouble 6
-- λ: halveOrDouble 7
-- λ: halveOrDouble 8
-- λ: halveOrDouble 6 >>= halveOrDouble
-- λ: hod2PlusOne 6
--
-- http://blog.jle.im/entry/practical-fun-with-monads-the-list-monadplus#introducing-monadplus#halveordouble

halveOrDouble :: Int -> [Int]
halveOrDouble n | even n    = [n `div` 2, n * 2]
                | otherwise = [n * 2]


halveOrDoubleTwice :: Int -> [Int]
halveOrDoubleTwice n = do
    x <- halveOrDouble n
    halveOrDouble x

hod2PlusOne :: Int -> [Int]
hod2PlusOne n = do
    x <- halveOrDouble n
    halveOrDouble x
    return $ x + 1
