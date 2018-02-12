#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver lts-10.4 --package hmatrix --package lens -- -Wall

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Lens hiding          ((<.>))
import           Numeric.LinearAlgebra.Static

data Net = N { _weights1 :: L 250 784
             , _bias1    :: R 250
             , _weights2 :: L 10 250
             , _bias2    :: R 10
             }
makeLenses ''Net

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

softMax
    :: R 10
    -> R 10
softMax x = expx / konst (norm_1 expx)
  where
    expx = exp x

crossEntropy
    :: R 10
    -> R 10
    -> Double
crossEntropy targ res = -(log res <.> targ)

runNet
    :: Net
    -> R 784
    -> R 10
runNet n x = z
  where
    y = logistic $ (n ^. weights1) #> x + (n ^. bias1)
    z = softMax  $ (n ^. weights2) #> y + (n ^. bias2)

netErr
    :: R 784
    -> R 10
    -> Net
    -> Double
netErr x targ n = crossEntropy targ (runNet n x)

main :: IO ()
main = return ()
