{-# LANGUAGE Arrows #-}

module AutoReader where

import Auto
import Auto3
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Maybe
import Prelude hiding             ((.), id)

delay :: a -> Auto a a
delay x0 = ACons $ \x -> (x0, delay x)

integral :: Double -> AutoM (Reader Double) Double Double
integral x0 = proc x -> do
    dt <- arrM (\_ -> ask)  -< ()
    autoM (autoFold (+) x0) -< x * dt

derivative :: Double -> AutoM (Reader Double) Double Double
derivative d0 = proc x -> do
    dt   <- arrM (\_ -> ask) -< ()
    last <- autoM (delay Nothing) -< Just x
    id   -< maybe d0 (\lst -> (x - lst) / dt) last

fancyCalculus :: AutoM (Reader Double) Double (Double, Double)
fancyCalculus = proc x -> do
    deriv  <- derivative 0 -< x
    deriv2 <- derivative 0 -< deriv
    intdev <- integral 0   -< deriv
    id -< (deriv2, intdev)

runReaderAuto :: AutoM (Reader r) a b -> Auto (a, r) b
runReaderAuto a = ACons $ \(x, r) ->
                    let (y, a') = runReader (runAutoM a x) r
                    in  (y, runReaderAuto a')
