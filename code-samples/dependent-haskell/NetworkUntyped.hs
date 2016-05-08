{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.Random
import Data.List
import Data.Maybe
import Numeric.LinearAlgebra
import System.Environment
import Text.Read

data Weights = W { wBiases :: !(Vector Double)
                 , wNodes  :: !(Matrix Double)
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
runLayer (W wB wN) v = wB + wN #> v

runNet :: Network -> Vector Double -> Vector Double
runNet (O w)      !v = logistic `cmap` runLayer w v
runNet (w :&~ n') !v = let v' = logistic `cmap` runLayer w v
                       in  runNet n' v'

randomWeights :: MonadRandom m => Int -> Int -> m Weights
randomWeights i o = do
    s1 <- getRandom
    s2 <- getRandom
    let wB = randomVector s1 Uniform o * 2 - 1
        wN = uniformSample s2 o (replicate i (-1, 1))
    return $ W wB wN

randomNet :: MonadRandom m => Int -> [Int] -> Int -> m Network
randomNet i [] o     =     O <$> randomWeights i o
randomNet i (h:hs) o = (:&~) <$> randomWeights i h <*> randomNet h hs o

train :: Double -> Vector Double -> Vector Double -> Network -> Network
train rate x0 targ = snd . go x0
  where
    go :: Vector Double -> Network -> (Vector Double, Network)
    go !x (O w@(W wB wN))
        = let y     = runLayer w x
              o     = cmap logistic y
              dEdy  = cmap logistic' y * (o - targ)
              delWs = tr wN #> dEdy
              wB'   = wB - scale rate dEdy
              wN'   = wN - scale rate (dEdy `outer` x)
          in  (delWs, O (W wB' wN'))
    go !x (w@(W wB wN) :&~ n)
        = let y            = runLayer w x
              o            = cmap logistic y
              (delWs', n') = go o n
              dEdy         = cmap logistic' y * delWs'
              delWs        = tr wN #> dEdy
              wB'          = wB - scale rate dEdy
              wN'          = wN - scale rate (dEdy `outer` x)
          in  (delWs, W wB' wN' :&~ n')

netTest :: MonadRandom m => Double -> Int -> m String
netTest rate n = do
    inps <- replicateM n $ do
      s <- getRandom
      return $ randomVector s Uniform 2 * 2 - 1
    let outs = flip map inps $ \v ->
                 if v `inCircle` (fromRational 0.33, 0.33)
                      || v `inCircle` (fromRational (-0.33), 0.33)
                   then fromRational 1
                   else fromRational 0
    net0 <- randomNet 2 [16,8] 1
    let trained = foldl' trainEach net0 (zip inps outs)
          where
            trainEach :: Network -> (Vector Double, Vector Double) -> Network
            trainEach nt (i, o) = train rate i o nt

        outMat = [ [ render (norm_2 (runNet trained (vector [x / 50 - 1,y / 25 - 1])))
                   | x <- [0..100] ]
                 | y <- [0..50] ]
        render n | n <= 0.2  = ' '
                 | n <= 0.4  = '.'
                 | n <= 0.6  = '-'
                 | n <= 0.8  = '='
                 | otherwise = '#'

    return $ unlines outMat
  where
    inCircle :: Vector Double -> (Vector Double, Double) -> Bool
    v `inCircle` (o, r) = norm_2 (v - o) <= r

main :: IO ()
main = do
    args <- getArgs
    let n    = readMaybe =<< (args !!? 0)
        rate = readMaybe =<< (args !!? 1)
    putStrLn =<< evalRandIO (netTest (fromMaybe 0.25   rate)
                                     (fromMaybe 500000 n   )
                            )

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _ = Nothing
(x:_ ) !!? 0 = Just x
(x:xs) !!? n = xs !!? (n - 1)
