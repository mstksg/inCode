{-# LANGUAGE Arrows #-}

module Auto2 where

import Auto
import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Function       (fix)
import Prelude hiding      ((.), id)

-- | Instances
instance Category Auto where
    id    = ACons $ \x -> (x, id)
    g . f = ACons $ \x ->
              let (y, f') = runAuto f x
                  (z, g') = runAuto g y
              in  (z, g' . f')

instance Functor (Auto r) where
    fmap f a = ACons $ \x ->
                 let (y, a') = runAuto a x
                 in  (f y, fmap f a')

instance Applicative (Auto r) where
    pure y    = ACons $ \_ -> (y, pure y)
    af <*> ay = ACons $ \x ->
                  let (f, af') = runAuto af x
                      (y, ay') = runAuto ay x
                  in  (f y, af' <*> ay')

instance Arrow Auto where
    arr f     = ACons $ \x -> (f x, arr f)
    first a   = ACons $ \(x, z) ->
                  let (y, a') = runAuto a x
                  in  ((y, z), first a')
    second a  = ACons $ \(z, x) ->
                  let (y, a') = runAuto a x
                  in  ((z, y), second a')
    a1 *** a2 = ACons $ \(x1, x2) ->
                  let (y1, a1') = runAuto a1 x1
                      (y2, a2') = runAuto a2 x2
                  in  ((y1, y2), a1' *** a2')
    a1 &&& a2 = ACons $ \x ->
                  let (y1, a1') = runAuto a1 x
                      (y2, a2') = runAuto a2 x
                  in  ((y1, y2), a1' &&& a2')

instance ArrowChoice Auto where
    left a = ACons $ \x ->
                 case x of
                   Left l  ->
                     let (l', a') = runAuto a l
                     in  (Left l', left a')
                   Right r ->
                     (Right r, left a)

instance ArrowLoop Auto where
    loop a = ACons $ \x ->
               let ((y, d), a') = runAuto a (x, d)
               in  (y, loop a')


-- | Helpers

-- (~.~): Auto composition
(~.~) :: Auto b c -> Auto a b -> Auto a c
g ~.~ f = ACons $ \x -> let (y, f') = runAuto f x
                            (z, g') = runAuto g y
                        in  (z, g' ~.~ f')

-- toAuto: turns a normal function into a stateless Auto that just performs
--      that function on incoming items.
toAuto :: (a -> b) -> Auto a b
toAuto f = ACons $ \x -> (f x, toAuto f)

-- idA: a stateless Auto that just returns its input.
idA :: Auto a a
idA = ACons $ \x -> (x, idA)

-- doubleA: a stateless Auto that just doubles its input.
doubleA :: Num a => Auto a a
doubleA = toAuto (*2)

-- succA: a stateless Auto that just increments its input.
succA :: Num a => Auto a a
succA = toAuto (+1)

-- | Category functions

-- doTwice: turns a morphism into a morphism that "repeats" itself twice.
doTwice :: Category r => r a a -> r a a
doTwice f = f . f

-- | dualCounters
--
-- The dualCounters both contain two counters that are incremented by an
-- incoming Either Int Int; a Left value increments the left counter and
-- a Right value increments the right counter.

-- dualCounterR: Explicit recursion
dualCounterR :: Auto (Either Int Int) (Int, Int)
dualCounterR = dualCounterWith (0, 0)
  where
    dualCounterWith (x, y) = ACons $ \inp ->
                               let newC = case inp of
                                            Left i  -> (x + i, y)
                                            Right i -> (x, y + 1)
                               in  (newC, dualCounterWith newC)

-- dualCounterC: Composition with Arrow combinators
dualCounterC :: Auto (Either Int Int) (Int, Int)
dualCounterC = (summer *** summer) . arr wrap
  where
    wrap (Left i)  = (i, 0)
    wrap (Right i) = (0, i)

-- dualCounterP: using proc notation
dualCounterP :: Auto (Either Int Int) (Int, Int)
dualCounterP = proc inp -> do
    let (add1, add2) = case inp of Left i  -> (i, 0)
                                   Right i -> (0, i)

    sum1 <- summer -< add1
    sum2 <- summer -< add2

    id -< (sum1, sum2)

-- | dualCounterSkips
--
-- The dualCounterSkips act the same way; except, every other Left value is
-- ignored.  The first, third, fifth, etc. Left values contribute to the
-- counter, but the rest are just ignored.

-- dualCounterSkipR: explicit recursion
dualCounterSkipR :: Auto (Either Int Int) (Int, Int)
dualCounterSkipR = counterFrom ((0, 0), 1)
  where
    counterFrom ((x, y), s) =
      ACons $ \inp ->
        let newCS = case inp of
                      Left i  | odd s     -> ((x + i, y), s + 1)
                              | otherwise -> ((x    , y), s + 1)
                      Right i             -> ((x, y + i), s    )
        in  (fst newCS, counterFrom newCS)

-- dualCounterSkipP: using proc notation
dualCounterSkipP :: Auto (Either Int Int) (Int, Int)
dualCounterSkipP = proc inp -> do
    (add1, add2) <- case inp of
                      Left i -> do
                        count <- summer -< 1
                        id -< (if odd count then i else 0, 0)
                      Right i ->
                        id -< (0, i)

    sum1 <- summer -< add1
    sum2 <- summer -< add2

    id -< (sum1, sum2)

