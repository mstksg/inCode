#!/usr/bin/env stack
-- stack --resolver lts-5.15 --install-ghc runghc --package hmatrix --package MonadRandom --package typelits-witnesses

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

import Control.Monad
import Control.Monad.Random
import Data.List
import Data.Maybe
import GHC.TypeLits
import GHC.TypeLits.List
import Numeric.LinearAlgebra.Static
import System.Environment
import Text.Read

data Weights i o = W { wBiases :: !(R o)
                     , wNodes  :: !(L o i)
                     }

data Network :: Nat -> [Nat] -> Nat -> * where
    O     :: !(Weights i o)
          -> Network i '[] o
    (:&~) :: KnownNat h
          => !(Weights i h)
          -> !(Network h hs o)
          -> Network i (h ': hs) o
infixr 5 :&~

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

logistic' :: Floating a => a -> a
logistic' x = logix * (1 - logix)
  where
    logix = logistic x

runLayer :: (KnownNat i, KnownNat o)
         => Weights i o
         -> R i
         -> R o
runLayer (W wB wN) v = wB + wN #> v

runNet :: (KnownNat i, KnownNat o)
       => Network i hs o
       -> R i
       -> R o
runNet (O w)      !v = logistic (runLayer w v)
runNet (w :&~ n') !v = let v' = logistic (runLayer w v)
                       in  runNet n' v'

randomWeights :: (MonadRandom m, KnownNat i, KnownNat o)
              => m (Weights i o)
randomWeights = do
    s1 <- getRandom
    s2 <- getRandom
    let wB = randomVector s1 Uniform * 2 - 1
        wN = uniformSample s2 (-1) 1
    return $ W wB wN

randomNet :: forall m i hs o. (MonadRandom m, KnownNat i, KnownNats hs, KnownNat o)
          => m (Network i hs o)
randomNet = case natsList :: NatList hs of
              ØNL     -> O     <$> randomWeights
              _ :<# _ -> (:&~) <$> randomWeights <*> randomNet

train :: forall i hs o. (KnownNat i, KnownNat o)
      => Double           -- ^ learning rate
      -> R i              -- ^ input vector
      -> R o              -- ^ target vector
      -> Network i hs o   -- ^ network to train
      -> Network i hs o
train rate x0 target = fst . go x0
  where
    go  :: forall j js. KnownNat j
        => R j              -- ^ input vector
        -> Network j js o   -- ^ network to train
        -> (Network j js o, R j)
    go !x (O w@(W wB wN))
        = let y    = runLayer w x
              o    = logistic y
              -- the gradient (how much y affects the error)
              --   (logistic' is the derivative of logistic)
              dEdy = logistic' y * (o - target)
              -- new bias weights and node weights
              wB'  = wB - konst rate * dEdy
              wN'  = wN - konst rate * (dEdy `outer` x)
              w'   = W wB' wN'
              -- bundle of derivatives for next step
              dWs  = tr wN #> dEdy
          in  (O w', dWs)
    go !x (w@(W wB wN) :&~ n)
        = let y          = runLayer w x
              o          = logistic y
              -- get dWs', bundle of derivatives from rest of the net
              (n', dWs') = go o n
              -- the gradient (how much y affects the error)
              dEdy       = logistic' y * dWs'
              -- new bias weights and node weights
              wB'  = wB - konst rate * dEdy
              wN'  = wN - konst rate * (dEdy `outer` x)
              w'   = W wB' wN'
              -- bundle of derivatives for next step
              dWs  = tr wN #> dEdy
          in  (w' :&~ n', dWs)

netTest :: MonadRandom m => Double -> Int -> m String
netTest rate n = do
    inps <- replicateM n $ do
      s <- getRandom
      return $ randomVector s Uniform * 2 - 1
    let outs = flip map inps $ \v ->
                 if v `inCircle` (fromRational 0.33, 0.33)
                      || v `inCircle` (fromRational (-0.33), 0.33)
                   then fromRational 1
                   else fromRational 0
    net0 :: Network 2 '[16, 8] 1 <- randomNet
    let trained = foldl' trainEach net0 (zip inps outs)
          where
            trainEach :: (KnownNat i, KnownNat o)
                      => Network i hs o
                      -> (R i, R o)
                      -> Network i hs o
            trainEach nt (i, o) = train rate i o nt

        outMat = [ [ render (norm_2 (runNet trained (vector [x / 25 - 1,y / 10 - 1])))
                   | x <- [0..50] ]
                 | y <- [0..20] ]
        render n | n <= 0.2  = ' '
                 | n <= 0.4  = '.'
                 | n <= 0.6  = '-'
                 | n <= 0.8  = '='
                 | otherwise = '#'

    return $ unlines outMat
  where
    inCircle :: KnownNat n => R n -> (R n, Double) -> Bool
    v `inCircle` (o, r) = norm_2 (v - o) <= r

main :: IO ()
main = do
    args <- getArgs
    let n    = readMaybe =<< (args !!? 0)
        rate = readMaybe =<< (args !!? 1)
    putStrLn "Training network..."
    putStrLn =<< evalRandIO (netTest (fromMaybe 0.25   rate)
                                     (fromMaybe 500000 n   )
                            )

(!!?) :: [a] -> Int -> Maybe a
xs !!? i = listToMaybe (drop i xs)
