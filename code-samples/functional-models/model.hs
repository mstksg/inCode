#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver lts-11.8 --package backprop-0.2.1.0 --package random --package hmatrix-backprop-0.1.2.1 --package statistics --package lens --package one-liner-instances --package microlens-th --package split -- -Wall -O2

{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE KindSignatures                           #-}
{-# LANGUAGE LambdaCase                               #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE PartialTypeSignatures                    #-}
{-# LANGUAGE PatternSynonyms                          #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE TypeApplications                         #-}
{-# LANGUAGE TypeInType                               #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE ViewPatterns                             #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures     #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fwarn-redundant-constraints          #-}

import           Control.Monad.Primitive
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Foldable
import           Data.Kind
import           Data.List hiding                      (mapAccumL)
import           Data.List.Split
import           Data.Semigroup
import           Data.Tuple
import           GHC.Generics                          (Generic)
import           GHC.TypeNats
import           Lens.Micro
import           Lens.Micro.TH
import           Numeric.Backprop
import           Numeric.LinearAlgebra.Static.Backprop
import           Numeric.LinearAlgebra.Static.Vector
import           Numeric.OneLiner
import           Statistics.Distribution
import           System.Random
import qualified Data.Traversable                      as T
import qualified Data.Vector.Sized                     as SV
import qualified Data.Vector.Storable.Sized            as SVS
import qualified Numeric.LinearAlgebra                 as HU
import qualified Numeric.LinearAlgebra.Static          as H
import qualified Prelude.Backprop                      as B

data a :& b = !a :& !b
  deriving (Show, Generic)
infixr 2 :&

type Model p a b = forall z. Reifies z W
                => BVar z p -> BVar z a -> BVar z b

linReg :: Model (Double :& Double) Double Double
linReg ab x = b * x + a
  where
    a = ab ^^. t1
    b = ab ^^. t2

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
    -> IO p             -- ^ updated parameter guess
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
feedForward wb x = w #> x + b
  where
    w = wb ^^. t1
    b = wb ^^. t2

feedForwardLog
    :: (KnownNat i, KnownNat o)
    => Model (L o i :& R o) (R i) (R o)
feedForwardLog wb x = logistic (w #> x + b)
  where
    w = wb ^^. t1
    b = wb ^^. t2

testTrainPerceptron :: IO [R 1]
testTrainPerceptron = do
    trained <- trainModelIO feedForwardLog $ take 10000 (cycle samps)
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
(f <~ g) pq = f p . g q
  where
    p = pq ^^. t1
    q = pq ^^. t2
infixr 8 <~

testTrainTwoLayer :: IO [R 1]
testTrainTwoLayer = do
    trained <- trainModelIO model (take 10000 (cycle samps))
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
ar2 cφ yLast yLastLast = ( c + φ1 * yLast + φ2 * yLastLast, yLast )
  where
    c  = cφ ^^. t1
    φ  = cφ ^^. t2
    φ1 = φ  ^^. t1
    φ2 = φ  ^^. t2

fcrnn
    :: (KnownNat i, KnownNat o)
    => ModelS ((L o i :& L o o) :& R o) (R o) (R i) (R o)
fcrnn wb x s = ( y, logistic y )
  where
    y  = (wX #> x) + (wS #> s) + b
    w  = wb ^^. t1
    b  = wb ^^. t2
    wX = w  ^^. t1
    wS = w  ^^. t2

(<*~*)
  :: (Backprop p, Backprop q, Backprop s, Backprop t)
    => ModelS  p        s       b c
    -> ModelS       q        t  a b
    -> ModelS (p :& q) (s :& t) a c
(f <*~* g) pq x st = let (y, t') = g q x t
                         (z, s') = f p y s
                     in  (z, reTup s' t')
  where
    p = pq ^^. t1
    q = pq ^^. t2
    s = st ^^. t1
    t = st ^^. t2
infixr 8 <*~*

mapS
    :: (forall s. Reifies s W => BVar s b -> BVar s c)
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
(f <*~ g) pq x = first (f p) . g q x
  where
    p = pq ^^. t1
    q = pq ^^. t2
infixr 8 <*~

unroll
    :: (Traversable t, Backprop a, Backprop b, Backprop (t b))
    => ModelS p s    a     b
    -> ModelS p s (t a) (t b)
unroll f p xs s0 = swap $ mapAccumL f' s0 xs
  where
    -- we have to re-arrange the order of arguments and tuple a bit to
    -- match what `mapAccumL` expects
    f' s x = swap (f p x s)

unrollLast
    :: (Backprop a, Backprop b)
    => ModelS p s  a  b
    -> ModelS p s [a] b
unrollLast f = mapS (last . sequenceVar) (unroll f)
-- TODO: switch to (last . toList)

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
trainState f ps x = fst $ f p x s
  where
    p = ps ^^. t1
    s = ps ^^. t2

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
    trained <- trainModelIO model $ take 10000 samps
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
    mapM_ print (take 50 ar2Test)
    writeFile "ar2sin.dat" $ unlines (show <$> ar2Test)

    putStrLn "Sine (RNN)"
    rnnTest <- testRNN
    mapM_ print (take 50 rnnTest)
    writeFile "rnnsin.dat" $ unlines (show . HU.sumElements . H.extract <$> rnnTest)


reTup
    :: (Backprop a, Backprop b, Reifies z W)
    => BVar z a
    -> BVar z b
    -> BVar z (a :& b)
reTup = isoVar2 (:&) (\case x :& y -> (x, y))

instance Backprop a => Backprop (SV.Vector n a) where
    zero = fmap zero
    add = SV.zipWith add
    one = fmap one

instance (KnownNat n, KnownNat m) => Random (L n m) where
    random = runState . fmap vecL $ SVS.replicateM (state random)
    randomR (xs,ys) = runState . fmap vecL $ SVS.zipWithM (curry (state . randomR))
        (lVec xs) (lVec ys)

instance (KnownNat n) => Random (R n) where
    random = runState $ vecR <$> SVS.replicateM (state random)
    randomR (xs,ys) = runState . fmap vecR $ SVS.zipWithM (curry (state . randomR))
        (rVec xs) (rVec ys)

mapAccumL
    :: (Traversable t, Backprop b, Backprop c, Backprop (t c), Reifies s W)
    => (BVar s a -> BVar s b -> (BVar s a, BVar s c))
    -> BVar s a
    -> BVar s (t b)
    -> (BVar s a, BVar s (t c))
mapAccumL f s = second collectVar
              . T.mapAccumL f s
              . sequenceVar

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

data NoState = NoState
  deriving (Show, Generic)

instance Num NoState where
    (+)         = gPlus
    (-)         = gMinus
    (*)         = gTimes
    negate      = gNegate
    abs         = gAbs
    signum      = gSignum
    fromInteger = gFromInteger

instance Fractional NoState where
    (/) = gDivide
    recip = gRecip
    fromRational = gFromRational

instance Backprop NoState

instance Random NoState where
    random g = (NoState, g)
    randomR _ g = (NoState, g)

