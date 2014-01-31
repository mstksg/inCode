module Stream where

import Data.Char (chr)

data List a = Cons (a, List a) | Nil
  deriving (Show)

newtype Stream b = SCons { runStream :: (b, Stream b) }

-- | Stream testers
--
-- streamToList: Straightforward conversion to infinite list
streamToList :: Stream b -> [b]
streamToList (SCons (x, xs)) = x : streamToList xs

-- testStream: Steps the stream n times, returning the result as a list
--      with the modified stream.
testStream :: Stream b -> Int -> ([b], Stream b)
testStream strm 0 = ([]  , strm )
testStream strm n = (y:ys, final)
  where
    (y , next )   = runStream  strm
    (ys, final)   = testStream next (n-1)

-- testStream_: Steps the stream n times and throws away the modified
--      stream.
testStream_ :: Stream b -> Int -> [b]
testStream_ = (fst .) . testStream


-- | Sample List
--
-- our custom datatype List
myList :: List Int
myList = Cons ( 1, Cons ( 2, Cons (3, Nil) ) )

-- a "normal" Haskell list
myList' :: [Int]
myList' = 1:(2:(3:[]))

-- | Sample Streams
--
-- myStream: counts upwards from 1
myStream :: Stream Int
myStream = streamFrom 1
  where
    streamFrom :: Int -> Stream Int
    streamFrom n = SCons ( n, streamFrom (n+1) )

-- charStream: the Stream is of Chars, but the state is an Int
charStream :: Stream Char
charStream = charStreamFrom 65
  where
    charStreamFrom :: Int -> Stream Char
    charStreamFrom n = SCons ( chr n, charStreamFrom (n+1) )

-- wackyStateStream: The type of the state varys dynamically
wackyStateStream :: Stream (Maybe Int)
wackyStateStream = wackyStateBool True
  where
    wackyStateBool :: Bool -> Stream (Maybe Int)
    wakcyStateBool False  = SCons (Nothing , wackyStateBool True)
    wackyStateBool True   = SCons (Just 100, wackyStateInt 8)

    wackyStateInt :: Int -> Stream (Maybe Int)
    wackyStateInt n
        | n `mod` 7 == 0  = SCons (Just n, wackyStateBool True)
        | otherwise       = SCons (Just (n+2), wackyStateInt (n+3))
