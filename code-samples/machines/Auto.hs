{-# LANGUAGE ScopedTypeVariables #-}
-- http://blog.jle.im/entry/intro-to-machines-arrows-part-1-stream-and

module Auto where

import Data.List (genericLength)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid, mappend, mempty)
import qualified Data.Map as Map

newtype Auto a b = ACons { runAuto :: a -> (b, Auto a b) }


-- | Auto testers
--
testAuto :: Auto a b -> [a] -> ([b], Auto a b)
testAuto auto []      = ([]  , auto )
testAuto auto (x:xs)  = (y:ys, final)
  where
    (y,  next ) = runAuto  auto x
    (ys, final) = testAuto next xs

testAuto_ :: Auto a b -> [a] -> [b]
testAuto_ a = fst . testAuto a

interactAuto :: (Read a, Show b) => Auto a b -> IO ()
interactAuto a0 = do
    inp <- getLine
    let (x,a1) = runAuto a0 (read inp)
    print x
    interactAuto a1



-- | Sample autos
--
-- myStreamAuto: identical to `myStream`, just counts upwards from
--      1, ignoring its input.
myStreamAuto :: Auto a Int
myStreamAuto = streamAutoFrom 1
  where
    streamAutoFrom :: Int -> Auto a Int
    streamAutoFrom n = ACons $ \_ -> ( n, streamAutoFrom (n+1) )

-- settableAuto: counts upwards from 1 if input is `Nothing`.  sets the
--      counter to `m` if input is `Just m`.
settableAuto :: Auto (Maybe Int) Int
settableAuto = counterFrom 1
  where
    counterFrom :: Int -> Auto (Maybe Int) Int
    counterFrom n = ACons $ \reset ->
      let c = fromMaybe n reset
      in  ( c, counterFrom (c + 1) )

-- isEvenAuto: identical to `settableAuto`, except just returns whether or
--      not the internal counter is even.  Demonstrates opaque state.
isEvenAuto :: Auto (Maybe Int) Bool
isEvenAuto = isEvenAutoFrom 1
  where
    isEvenAutoFrom :: Int -> Auto (Maybe Int) Bool
    isEvenAutoFrom n = ACons $ \reset ->
      let c = fromMaybe n reset
      in  ( even c, isEvenAutoFrom (c + 1) )

-- summer: outputs the result of all the integer inputs it has received.
summer :: Num a => Auto a a
summer = sumFrom 0
  where
    sumFrom :: Num a => a -> Auto a a
    sumFrom n = ACons $ \input ->
      let s = n + input
      in  ( s , sumFrom s )

-- autoFold: an Auto version of `foldl`; "folds in" all input into its
--      state with a given folding funcion and outputs the accumulated
--      result.
autoFold :: forall a b. (b -> a -> b) -> b -> Auto a b
autoFold op init = foldFrom init
  where
    foldFrom :: b -> Auto a b
    foldFrom x = ACons $ \input ->
      let y = x `op` input
      in  ( y, foldFrom y )

-- summer': `summer` re-implemented with `autoFold`.
summer' :: Num a => Auto a a
summer' = autoFold (+) 0

-- accumulateIntoList: adds all input into an ongoing list and re-outputs
--      that list at every step.
accumulateIntoList :: Auto a [a]
accumulateIntoList = autoFold (flip (:)) []

-- productor: like `summer`, but multiplies instead of adds.
productor :: Num a => Auto a a
productor = autoFold (*) 1

-- accumulateStrings: returns at every step the concatenation of all
--      strings it has received so far.
accumulateStrings :: Auto String String
accumulateStrings = autoFold (++) ""

-- monoidAccum: returns the total `mconcat` of every monoid item received
--      so far.
monoidAccum :: Monoid a => Auto a a
monoidAccum = autoFold mappend mempty

-- | Intermediate examples
--
-- rollingAverage: Outputs the average of the past `window` inputs.
rollingAverage :: forall a. Fractional a
    => Int          -- length of the window
    -> Auto a a     -- an Auto taking an `a` and returning an average `a`
rollingAverage window = roll []
  where
    roll :: [a] -> Auto a a
    roll xs = ACons $ \val ->
      let xs' = take window $ val:xs  -- pop on the new value, drop all
                                      --   values past the window
          ave = sum xs' / genericLength xs'  -- the average
      in  ( ave, roll xs' )


-- onFor: normally False (off), but when triggered against the given
--      predicate, is True for the given amount of time afterwards.
onFor :: forall a.
     (a -> Bool)  -- test to see if an input 'triggers'
  -> Int          -- amount of time to stay True for
  -> Auto a Bool  -- An Auto that takes an `a` and returns a `Bool`
onFor p hold = wait
  where
    wait :: Auto a Bool                 -- the "waiting" state
    wait = ACons $ \input ->
      if p input                        -- if triggered,
        then (True, countdown hold)     -- jump to "countdown" state
        else (False, wait)              -- otherwise, stay waiting

    countdown :: Int -> Auto a Bool     -- the "countdown" state
    countdown n = ACons $ \input ->
      if p input                        -- if re-triggered
        then (True, countdown hold)     -- countdown all over again
        else
          if n == 1
            then (False, wait)          -- If counted down, go wait again
            else (True, countdown (n-1))  -- otherwise, count down.


-- Command: Commands for working w/ an autoMap with keys k and values v
data Command k v = Insert k v | Lookup k | Delete k

-- autoMap: A thin wrapper around a Map enforcing a maximum size.
--      `Insert key val` is `Just val` on success, `Nothing` on a full Map
--      `Lookup key` is `Just val` on success, `Nothing` if not in Map
--      `Delete key` is `Just val` on success, `Nothing` if not in Map
autoMap :: forall k v. Ord k
    => Int              -- the maximum capacity of the map
    -> Auto (Command k v) (Maybe v)
autoMap cap = go Map.empty
  where
    go :: Map.Map k v -> Auto (Command k v) (Maybe v)
    go m = ACons $ \command ->
      case command of
        Insert key val ->
          if Map.size m >= cap && key `Map.notMember` m
            then
              ( Nothing, go m )                 -- Map is full, no go!
            else
              let m' = Map.insert key val m     -- go for it!
              in  ( Just val, go m' )
        Lookup key ->
          ( key `Map.lookup` m, go m )
        Delete key ->
          let result  = key `Map.lookup` m
              m'      = key `Map.delete` m
          in  ( result, go m' )



-- | Sample "normal" function
maybeIsEven :: (->) (Maybe Int) Bool
maybeIsEven = even . fromMaybe 1
