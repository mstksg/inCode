{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.Random
import Data.List
import Numeric.LinearAlgebra

data Weights = W { wBiases  :: !(Vector Double)
                 , wWeights :: !(Matrix Double)
                 }
  deriving (Show, Eq)

data Network = O !Weights
             | !Weights :&~ !Network
  deriving (Show, Eq)
infixr 5 :&~

logistic :: Double -> Double
logistic x = 1 / (1 + exp (-x))

logistic' :: Double -> Double
logistic' x = logix * (1 - logix)
  where
    logix = logistic x

runLayer :: Weights -> Vector Double -> Vector Double
runLayer (W wB wW) v = wB + (wW #> v)

runNet :: Network -> Vector Double -> Vector Double
runNet (O w)      !v = logistic `cmap` runLayer w v
runNet (w :&~ n') !v = let v' = logistic `cmap` runLayer w v
                       in  runNet n' v'

randomWeights :: MonadRandom m => Int -> Int -> m Weights
randomWeights i o = do
    s1 <- getRandom
    s2 <- getRandom
    let wBiases  = randomVector s1 Uniform o * 2 - 1
        wWeights = uniformSample s2 o (replicate i (-1, 1))
    return W{..}

randomNet :: MonadRandom m => Int -> [Int] -> Int -> m Network
randomNet i [] o     =     O <$> randomWeights i o
randomNet i (h:hs) o = (:&~) <$> randomWeights i h <*> randomNet h hs o

train :: Double -> Vector Double -> Vector Double -> Network -> Network
train rate x0 targ = snd . go x0
  where
    go :: Vector Double -> Network -> (Vector Double, Network)
    go !x (O w@(W wB wW))
        = let y     = runLayer w x
              o     = logistic  `cmap` y
              dEdy  = (logistic' `cmap` y) * (o - targ)
              delWs = tr wW #> dEdy
              wB'   = wB - scale rate dEdy
              wW'   = wW - scale rate (dEdy `outer` x)
          in  (delWs, O (W wB' wW'))
    go !x (w@(W wB wW) :&~ n)
        = let y            = runLayer w x
              o            = logistic `cmap` y
              (delWs', n') = go o n
              dEdy         = (logistic' `cmap` y) * delWs'
              delWs        = tr wW #> dEdy
              wB'          = wB - scale rate dEdy
              wW'          = wW - scale rate (dEdy `outer` x)
          in  (delWs, W wB' wW' :&~ n')

netTest :: MonadRandom m => Int -> m String
netTest n = do
    inps <- replicateM n $ do
      s <- getRandom
      return $ randomVector s Uniform 2 * 2 - 1
    let outs = flip map inps $ \v ->
                 if norm_2 (v - 0.33) <= 0.33
                      || norm_2 (v + 0.33) <= 0.33
                   then konst 1 1
                   else konst 0 1
    net0 <- randomNet 2 [8,8] 1
    let trained = foldl' trainEach net0 (zip inps outs)
          where
            trainEach :: Network -> (Vector Double, Vector Double) -> Network
            trainEach nt (i, o) = train 0.1 i o nt

        outMat = [ [ render (norm_2 (runNet trained (vector [x / 50 - 1,y / 25 - 1])))
                   | x <- [0..100] ]
                 | y <- [0..50] ]
        render n | n <= 0.2  = ' '
                 | n <= 0.4  = '.'
                 | n <= 0.6  = '-'
                 | n <= 0.8  = '='
                 | otherwise = '#'

    return $ unlines outMat

main :: IO ()
main = putStrLn =<< evalRandIO (netTest 1000000)
