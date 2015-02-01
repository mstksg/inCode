{-# LANGUAGE Arrows #-}

module AutoState where

import Auto
import Auto3
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Trans.State
import Data.Maybe
import Prelude hiding            ((.), id)

limit :: Int -> Auto a b -> AutoM (State Int) a (Maybe b)
limit cost a = proc x -> do
    fuel <- arrM (\_ -> get) -< ()
    if fuel >= cost
      then do
        arrM (\_ -> modify (subtract cost)) -< ()
        y <- autoM a -< x
        id -< Just y
      else
        id -< Nothing

sumSqDiff :: AutoM (State Int) Int Int
sumSqDiff = proc x -> do
  sums   <- fromMaybe 0 <$> limit 3 summer -< x
  sumSqs <- fromMaybe 0 <$> limit 1 summer -< x^2
  id -< sumSqs - sums

stuff :: AutoM (State Int) Int (Maybe Int, Maybe Int, Int)
stuff = proc x -> do
    doubled <- limit 1 id -< x * 2
    tripled <- if even x
                 then limit 2 id -< x * 3
                 else id         -< Just (x * 3)
    sumSqD  <- sumSqDiff -< x
    id -< (doubled, tripled, sumSqD)

runStateAuto :: AutoM (State s) a b -> Auto (a, s) (b, s)
runStateAuto a = ACons $ \(x, s) ->
                   let ((y, a'), s') = runState (runAutoM a x) s
                   in  ((y, s'), runStateAuto a')
