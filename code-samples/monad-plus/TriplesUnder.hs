-- "What triples of numbers under n are the sides of a right triangle?"
--
-- To test/run:
-- $ ghci
-- λ: :i TriplesUnder.hs
-- λ: triplesUnder 10
--
-- http://blog.jle.im/entry/practical-fun-with-monads-the-list-monadplus#finding-the-right-combinations

import Control.Monad (guard)

triplesUnder :: Int -> [Int]
triplesUnder n = do
    a <- [1..n]
    b <- [a..n]
    c <- [b..n]
    guard $ a^2 + b^2 == c^2
    return (a,b,c)

