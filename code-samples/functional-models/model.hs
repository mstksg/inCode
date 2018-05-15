#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver lts-11.9 --package backprop-0.2.2.0 --package random --package hmatrix-backprop-0.1.2.1 --package statistics --package lens --package one-liner-instances --package split -- -Wall -O2

{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE LambdaCase                               #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE PartialTypeSignatures                    #-}
{-# LANGUAGE PatternSynonyms                          #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TypeApplications                         #-}
{-# LANGUAGE TypeInType                               #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE ViewPatterns                             #-}
{-# OPTIONS_GHC -fno-warn-orphans                     #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures     #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_GHC -fwarn-redundant-constraints          #-}

import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Foldable
import           Data.List
import           Data.List.Split
import           Data.Tuple
import           Data.Type.Option
import           GHC.Generics                          (Generic)
import           GHC.TypeNats
import           Lens.Micro hiding                     ((&))
import           Numeric.Backprop
import           Numeric.LinearAlgebra.Static.Backprop
import           Numeric.LinearAlgebra.Static.Vector
import           Numeric.OneLiner
import           System.Random
import qualified Data.Vector.Storable.Sized            as SVS
import qualified Numeric.LinearAlgebra                 as HU
import qualified Numeric.LinearAlgebra.Static          as H
import qualified Prelude.Backprop                      as B

data a :& b = !a :& !b
  deriving (Show, Generic)
infixr 2 :&

type Model p a b = forall z. Reifies z W
                => BVar z p
                -> BVar z a
                -> BVar z b

linReg :: Model (Double :& Double) Double Double
linReg (a :&& b) x = b * x + a

squaredErrorGrad
    :: (Backprop p, Backprop b, Num b)
    => Model p a b      -- ^ Model
    -> a                -- ^ Observed input
    -> b                -- ^ Observed output
    -> p                -- ^ Parameter guess
    -> p                -- ^ Gradient
squaredErrorGrad f x targ = gradBP $ \p ->
    (f p (auto x) - auto targ) ^ 2

trainModel
    :: (Fractional p, Backprop p, Num b, Backprop b)
    => Model p a b      -- ^ model to train
    -> p                -- ^ initial parameter guess
    -> [(a,b)]          -- ^ list of observations
    -> p                -- ^ updated parameter guess
trainModel f = foldl' $ \p (x,y) -> p - 0.1 * squaredErrorGrad f x y p

trainModelIO
    :: (Fractional p, Backprop p, Num b, Backprop b, Random p)
    => Model p a b      -- ^ model to train
    -> [(a,b)]          -- ^ list of observations
    -> IO p             -- ^ parameter guess
trainModelIO m xs = do
    p0 <- (/ 10) . subtract 0.5 <$> randomIO
    return $ trainModel m p0 xs

testTrainLinReg :: IO (Double :& Double)
testTrainLinReg = trainModelIO linReg (concat (replicate 1000 samps))
  where
    samps = [(1,1),(2,3),(3,5),(4,7),(5,9)]

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

feedForward
    :: (KnownNat i, KnownNat o)
    => Model (L o i :& R o) (R i) (R o)
feedForward (w :&& b) x = w #> x + b

feedForwardLog
    :: (KnownNat i, KnownNat o)
    => Model (L o i :& R o) (R i) (R o)
feedForwardLog (w :&& b) x = logistic (w #> x + b)

testTrainPerceptron :: IO [R 1]
testTrainPerceptron = do
    trained <- trainModelIO feedForwardLog $ take 10000 (cycle samps)
    print trained
    return [ evalBP2 feedForwardLog trained r | (r, _) <- samps ]
  where
    samps = [ (H.vec2 0 0, 0)
            , (H.vec2 1 0, 0)
            , (H.vec2 0 1, 0)
            , (H.vec2 1 1, 1)
            ]

logReg :: Model (Double :& Double) Double Double
logReg ab = logistic . linReg ab

feedForwardLog'
    :: (KnownNat i, KnownNat o)
    => Model (L o i :& R o) (R i) (R o)
feedForwardLog' wb = logistic . feedForward wb

softMax :: (Reifies z W, KnownNat n) => BVar z (R n) -> BVar z (R n)
softMax x = konst (1 / sumElements expx) * expx
  where
    expx = exp x

feedForwardSoftMax
    :: (KnownNat i, KnownNat o)
    => Model (L o i :& R o) (R i) (R o)
feedForwardSoftMax wb = logistic . feedForward wb

(<~)
    :: (Backprop p, Backprop q)
    => Model  p       b c
    -> Model       q  a b
    -> Model (p :& q) a c
(f <~ g) (p :&& q) = f p . g q
infixr 8 <~

testTrainTwoLayer :: IO [R 1]
testTrainTwoLayer = do
    trained <- trainModelIO model (take 10000 (cycle samps))
    print trained
    return [ evalBP2 model trained r | (r, _) <- samps ]
  where
    model :: Model _ (R 2) (R 1)
    model = feedForwardLog' @4 @1 <~ feedForwardLog' @2 @4
    samps = [ (H.vec2 0 0, 0)
            , (H.vec2 1 0, 1)
            , (H.vec2 0 1, 1)
            , (H.vec2 1 1, 0)
            ]

type ModelS p s a b = forall z. Reifies z W
                   => BVar z p
                   -> BVar z a
                   -> BVar z s
                   -> (BVar z b, BVar z s)

ar2 :: ModelS (Double :& (Double :& Double)) Double Double Double
ar2 (c :&& (φ1 :&& φ2)) yLast yLastLast =
    ( c + φ1 * yLast + φ2 * yLastLast, yLast )

fcrnn
    :: (KnownNat i, KnownNat o)
    => ModelS ((L o i :& L o o) :& R o) (R o) (R i) (R o)
fcrnn ((wX :&& wS) :&& b) x s = ( y, logistic y )
  where
    y  = (wX #> x) + (wS #> s) + b

(<*~*)
  :: (Backprop p, Backprop q, Backprop s, Backprop t)
    => ModelS  p        s       b c
    -> ModelS       q        t  a b
    -> ModelS (p :& q) (s :& t) a c
(f <*~* g) (p :&& q) x (s :&& t) = (z, s' :&& t')
  where
    (y, t') = g q x t
    (z, s') = f p y s
infixr 8 <*~*

mapS
    :: (forall z. Reifies z W => BVar z b -> BVar z c)
    -> ModelS p s a b
    -> ModelS p s a c
mapS f g p x = first f . g p x

toS :: Model  p   a b
    -> ModelS p s a b
toS f p x s = (f p x, s)

(<*~)
  :: (Backprop p, Backprop q)
    => Model   p         b c
    -> ModelS       q  s a b
    -> ModelS (p :& q) s a c
(f <*~ g) (p :&& q) x = first (f p) . g q x
infixr 8 <*~

unroll
    :: (Backprop a, Backprop b)
    => ModelS p s  a   b
    -> ModelS p s [a] [b]
unroll f p xs s0 = swap $ B.mapAccumL f' s0 xs
  where
    -- we have to re-arrange the order of arguments and tuple a bit to
    -- match what `mapAccumL` expects
    f' s x = swap (f p x s)

unrollLast
    :: (Backprop a, Backprop b)
    => ModelS p s  a  b
    -> ModelS p s [a] b
unrollLast f = mapS (last . sequenceVar) (unroll f)

unrollLast'
    :: Backprop a
    => ModelS p s  a  b
    -> ModelS p s [a] b
unrollLast' f p xs s0 = foldl' go (undefined, s0) (sequenceVar xs)
  where
    go (_, s) x = f p x s


fixState
    :: s
    -> ModelS p s a b
    -> Model  p   a b
fixState s0 f p x = fst $ f p x (auto s0)

zeroState
    :: Num s
    => ModelS p s a b
    -> Model  p   a b
zeroState = fixState 0

trainState
    :: (Backprop p, Backprop s)
    => ModelS  p    s  a b
    -> Model  (p :& s) a b
trainState f (p :&& s) x = fst $ f p x s

prime
    :: Foldable t
    => ModelS p s a b     -- ^ model
    -> p                  -- ^ parameterization
    -> s                  -- ^ initial state
    -> t a                -- ^ priming input
    -> s                  -- ^ primed state
prime f p = foldl' $ evalBP2 (\s x -> snd $ f (auto p) x s)

feedback
    :: (Backprop a, Backprop s)
    => ModelS p s a a     -- ^ model
    -> p                  -- ^ parameterization
    -> s                  -- ^ initial state
    -> a                  -- ^ initial input
    -> [a]                -- ^ inifinite feedback loop
feedback f p s0 x0 = unfoldr go (x0, s0)
  where
    go (x, s) = Just (x, (y, s'))
      where
        (y, s') = evalBP (uncurry T2 . f (auto p) (auto x)) s

testAR2 :: IO [Double]
testAR2 = do
    trained <- trainModelIO model $ take 10000 samps
    let primed = prime    model0 trained 0      (take 19 series)
        output = feedback model0 trained primed (series !! 19)
    print trained
    return $ take 200 output
  where
    -- sine wave with period 25
    series :: [Double]
    series = [ sin (2 * pi * t / 25) | t <- [0..]              ]
    samps  = [ (init c, last c)      | c <- chunksOf 19 series ]
    model0 :: ModelS _ _ Double Double
    model0 = ar2
    model  :: Model  _   [Double] Double
    model  = zeroState $ unrollLast model0

testRNN :: IO [R 1]
testRNN = do
    trained <- trainModelIO model $ take 100000 samps
    let primed = prime   model0  trained 0      (take 19 series)
        output = feedback model0 trained primed (series !! 19)
    return $ take 200 output
  where
    -- sine wave with period 25
    series :: [H.R 1]
    series = [ H.konst (sin (2 * pi * t / 25)) | t <- [0..]              ]
    samps  = [ (init c, last c)                | c <- chunksOf 19 series ]
    model0 :: ModelS _ _ (R 1) (R 1)
    model0 = feedForward @30 @1
         <*~ mapS logistic (fcrnn @1 @30)
    model  :: Model  _   [R 1] (R 1)
    model  = zeroState $ unrollLast model0

recurrently
    :: (Backprop a, Backprop b)
    => Model  p   (a :& b) b
    -> ModelS p b  a       b
recurrently f p x yLast = (y, y)
  where
    y = f p (x :&& yLast)

recurrentlyWith
    :: (Backprop a, Backprop b)
    => (forall z. Reifies z W => BVar z c -> BVar z b)
    -> Model  p   (a :& b) c
    -> ModelS p b  a       c
recurrentlyWith store f p x yLast = (y, store y)
  where
    y = f p (x :&& yLast)

ffOnSplit
    :: forall i o. (KnownNat i, KnownNat o)
    => Model _ (R i :& R o) (R o)
ffOnSplit p (rI :&& rO) = feedForward p (rI # rO)

fcrnn'
    :: (KnownNat i, KnownNat o)
    => ModelS _ (R o) (R i) (R o)
fcrnn' = recurrentlyWith logistic (\p -> feedForward p . uncurryT (#))

lagged
    :: (KnownNat n, 1 <= n)
    => Model  p       (R (n + 1)) b
    -> ModelS p (R n) Double      b
lagged f p x xLasts = (y, xLasts')
  where
    fullLasts    = xLasts & x
    y            = f p fullLasts
    (_, xLasts') = headTail fullLasts

ar :: (KnownNat n, 1 <= n)
   => ModelS _ (R n) Double Double
ar = lagged (\p -> fst . headTail . feedForward @_ @1 p)

ar2' :: ModelS _ (R 2) Double Double
ar2' = ar @2

main :: IO ()
main = do
    putStrLn "Linear regression"
    print =<< testTrainLinReg

    putStrLn "Single layer perceptron learning AND"
    mapM_ print =<< testTrainPerceptron

    putStrLn "Two-layer ANN learning XOR"
    mapM_ print =<< testTrainTwoLayer

    putStrLn "Sine (AR2)"
    ar2Test <- testAR2
    mapM_ print (take 30 ar2Test)
    writeFile "ar2sin.dat" $ unlines (show <$> ar2Test)

    putStrLn "Sine (RNN)"
    rnnTest <- testRNN
    mapM_ print (take 30 rnnTest)
    writeFile "rnnsin.dat" $ unlines (show . HU.sumElements . H.extract <$> rnnTest)

pattern (:&&)
    :: ( Backprop a
       , Backprop b
       , Reifies z W
       )
    => BVar z a
    -> BVar z b
    -> BVar z (a :& b)
pattern x :&& y <- (\xy -> (xy ^^. t1, xy ^^. t2)->(x, y))
  where
    (:&&) = isoVar2 (:&) (\case x :& y -> (x, y))
{-# COMPLETE (:&&) #-}

t1 :: Lens (a :& b) (a' :& b) a a'
t1 f (x :& y) = (:& y) <$> f x

t2 :: Lens (a :& b) (a :& b') b b'
t2 f (x :& y) = (x :&) <$> f y

instance (Num a, Num b) => Num (a :& b) where
    (+)         = gPlus
    (-)         = gMinus
    (*)         = gTimes
    negate      = gNegate
    abs         = gAbs
    signum      = gSignum
    fromInteger = gFromInteger

instance (Fractional a, Fractional b) => Fractional (a :& b) where
    (/) = gDivide
    recip = gRecip
    fromRational = gFromRational

instance (Random a, Random b) => Random (a :& b) where
    random g0 = (x :& y, g2)
      where
        (x, g1) = random g0
        (y, g2) = random g1
    randomR (x0 :& y0, x1 :& y1) g0 = (x :& y, g2)
      where
        (x, g1) = randomR (x0, x1) g0
        (y, g2) = randomR (y0, y1) g1

instance (Backprop a, Backprop b) => Backprop (a :& b)

uncurryT
    :: (Backprop a, Backprop b, Reifies z W)
    => (BVar z a -> BVar z b -> BVar z c)
    -> BVar z (a :& b)
    -> BVar z c
uncurryT f x = f (x ^^. t1) (x ^^. t2)

instance (KnownNat n, KnownNat m) => Random (L n m) where
    random = runState . fmap vecL $ SVS.replicateM (state random)
    randomR (xs,ys) = runState . fmap vecL $ SVS.zipWithM (curry (state . randomR))
        (lVec xs) (lVec ys)

instance (KnownNat n) => Random (R n) where
    random = runState $ vecR <$> SVS.replicateM (state random)
    randomR (xs,ys) = runState . fmap vecR $ SVS.zipWithM (curry (state . randomR))
        (rVec xs) (rVec ys)

