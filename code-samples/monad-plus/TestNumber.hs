-- "What operations on a number will make it a multiple of three?"
--
-- To test/run:
-- $ ghci
-- λ: :i TestNumber.hs
-- λ: testNumber 9
--
-- http://blog.jle.im/entry/practical-fun-with-monads-the-list-monadplus#testing-multiple-paths

import Control.Monad (guard)

isMultThree :: Int -> Bool
isMultThree a = a `mod` 3 == 0

testNumber :: Int -> [String]
testNumber n = do
    (f, fName)  <-  [ ((*2)         , "times two")
                    , ((*3)         , "times three")
                    , ((+2)         , "plus two")
                    , ((+3)         , "plus three")
                    , ((^2)         , "square")
                    , ((+1).(^2)    , "square plus 1")
                    , ((+1).(^3)    , "cube plus 1")
                    , (id           , "stay the same")
                    ]
    let
        z = f n

    guard $ isMultThree z
    return fName

