import Control.Monad

-- "What operations on a number will make it a multiple of three?"
--
-- To test/run:
-- $ ghci
-- λ: :i TestNumber.hs
-- λ: testNumber (number you want to test)

isMultThree :: Int -> Bool
isMultThree a = a `mod` 3 == 0

testNumber :: Int -> [String]
testNumber n = do
    x <- return n
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
        z = f x

    guard $ isMultThree z
    return fName

