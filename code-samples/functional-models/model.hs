#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver lts-11.8 --package backprop-0.2.1.0 --package random --package hmatrix-backprop-0.1.2.1 --package statistics --package lens --package one-liner-instances --package microlens-th -- -Wall -O2

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
import           Data.List
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
import qualified Data.Vector.Sized                     as SV
import qualified Data.Vector.Storable.Sized            as SVS
import qualified Numeric.LinearAlgebra.Static          as H

data a :& b = !a :& !b
  deriving (Show, Generic)

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
    (f p (constVar x) - constVar targ) ^ 2

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
    => Model     p    b c
    -> Model       q  a b
    -> Model (p :& q) a c
(f <~ g) pq = f p . g q
  where
    p = pq ^^. t1
    q = pq ^^. t2
infixr 8 <~

testTrainTwoLayer :: IO [R 1]
testTrainTwoLayer = do
    trained <- trainModelIO model (take 50000 (cycle samps))
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

unroll
    :: Backprop a
    => ModelS p s a b
    -> ModelS p s (SV.Vector n a) b
unroll f p xs s0 = foldl' (\(_, s) x -> f p x s)
                     (undefined, s0)
                     (sequenceVar xs)

unrollTrace
    :: (Backprop a, Backprop b)
    => ModelS p s a b
    -> ModelS p s (SV.Vector n a) (SV.Vector n b)
unrollTrace f p xs s0 = first collectVar
                      . swap
                      $ mapAccumL (\s x -> swap (f p x s))
                          s0
                          (sequenceVar xs)

fixState
    :: s
    -> ModelS p s a b
    -> Model p a b
fixState s0 f p x = fst $ f p x (constVar s0)

trainState
    :: (Backprop p, Backprop s)
    => ModelS p s a b
    -> Model (p :& s) a b
trainState f ps x = fst $ f p x s
  where
    p = ps ^^. t1
    s = ps ^^. t2

(<&~)
    :: (Backprop p, Backprop q, Backprop s, Backprop t)
    => ModelS     p        s    b c
    -> ModelS       q        t  a b
    -> ModelS (p :& q) (s :& t) a c
(f <&~ g) pq x st = let (y, t') = g q x t
                        (z, s') = f p y s
                    in  (z, reTup s' t')
  where
    p = pq ^^. t1
    q = pq ^^. t2
    s = st ^^. t1
    t = st ^^. t2
infixr 8 <&~

main :: IO ()
main = do
    putStrLn "Linear regression"
    print =<< testTrainLinReg
    putStrLn "Single layer perceptron learning AND"
    mapM_ print =<< testTrainPerceptron
    putStrLn "Two-layer ANN learning XOR"
    mapM_ print =<< testTrainTwoLayer


-- data Model state param a b = Model
--     { initParam :: forall m. PrimMonad m
--                 => MWC.Gen (PrimState m)
--                 -> m param
--     , runModel  :: forall s. Reifies s W
--                 => BVar s param
--                 -> BVar s a
--                 -> BVar s state
--                 -> (BVar s b, BVar s state)
--     }

-- type NoParam = T0
-- type NoState = T0

-- type StatelessModel = Model NoState

-- funcModel
--     :: (forall s. BVar s a -> BVar s b)
--     -> Model state NoParam a b
-- funcModel f = Model { initParam = \_ -> pure T0
--                     , runModel  = \_ x s -> (f x, s)
--                     }

-- data FCP i o = FCP { _fcWeights :: L o i
--                    , _fcBias    :: R o
--                    }
--   deriving (Generic)

-- makeLenses ''FCP

-- fullyConnected
--     :: (ContGen d, KnownNat i, KnownNat o)
--     => d
--     -> Model NoState (FCP i o) (R i) (R o)
-- fullyConnected d = Model
--     { initParam = \g ->
--           FCP <$> (vecL <$> SVS.replicateM (genContVar d g))
--               <*> (vecR <$> SVS.replicateM (genContVar d g))
--     , runModel  = \p x s ->
--           ( (p ^^. fcWeights) #> x + (p ^^. fcBias)
--           , s
--           )
--     }

-- dimap
--     :: (forall s. Reifies s W => BVar s a -> BVar s b)
--     -> (forall s. Reifies s W => BVar s c -> BVar s d)
--     -> Model param state b c
--     -> Model param state a d
-- dimap f g m = m { runModel = \p x -> first g . runModel m p (f x) }

-- lmap
--     :: (forall s. Reifies s W => BVar s a -> BVar s b)
--     -> Model param state b c
--     -> Model param state a c
-- lmap f = dimap f id

-- rmap
--     :: (forall s. Reifies s W => BVar s b -> BVar s c)
--     -> Model param state a b
--     -> Model param state a c
-- rmap = dimap id

-- (.%)
--     :: (Num p, Num q, Num s, Num t)
--     => Model p s a b
--     -> Model q t b c
--     -> Model (Tup p q) (Tup s t) a c
-- f .% g = Model { initParam = \gen -> Tup <$> initParam f gen
--                                         <*> initParam g gen
--                , runModel  = \pq x st ->
--                    let (y, s) = runModel f (pq ^^. t1) x (st ^^. t1)
--                        (z, t) = runModel g (pq ^^. t2) y (st ^^. t2)
--                    in  (z, reTup s t)
--                }

-- logistic :: Floating a => a -> a
-- logistic x = 1 / (1 + exp (-x))

-- softmax :: (Reifies s W, KnownNat n) => BVar s (R n) -> BVar s (R n)
-- softmax x = konst (1 / sumElements expx) * expx
--   where
--     expx = exp x

-- neural
--     :: (ContGen d, KnownNat i, KnownNat o)
--     => d
--     -> Model _ (Tup (FCP i o) (FCP i o)) (R i) (R o)
-- neural d = rmap logistic (fullyConnected d)
--         .% rmap softmax  (fullyConnected d)

-- unroll
--     :: (KnownNat n, Num a, Num b)
--     => Model state param a b
--     -> Model state param (SV.Vector n a) (SV.Vector n b)
-- unroll m = m
--     { runModel  = \p xs s -> first collectVar
--                            . flip runState s
--                            . traverse (state . runModel m p)
--                            . sequenceVar
--                            $ xs
--     }

-- unrollFinal
--     :: (KnownNat n, Num a, Num b, 1 <= n)
--     => Model state param a b
--     -> Model state param (SV.Vector n a) b
-- unrollFinal m = m
--     { runModel  = \p xs s0 ->
--           foldl' (\(_, s) x -> runModel m p x s)
--                  (undefined, s0)
--                  (sequenceVar xs)
--     }

-- trainState
--     :: (Num state, Num param)
--     => (forall m. MWC.Gen (PrimState m) -> m state)
--     -> Model state param a b
--     -> Model NoState (Tup param state) a b
-- trainState initState m = Model
--     { initParam = \g -> Tup <$> initParam m g
--                            <*> initState g
--     , runModel  = \ps x n -> ( fst $ runModel m (ps ^^. t1) x (ps ^^. t2)
--                              , n
--                              )
--     }

-- deState
--     :: state
--     -> Model state   param a b
--     -> Model NoState param a b
-- deState s m = m
--     { runModel  = \p x n -> (fst $ runModel m p x (constVar s), n)
--     }


-- instance (KnownNat i, KnownNat o) => Num (FCP i o) where
--     (+)         = gPlus
--     (-)         = gMinus
--     (*)         = gTimes
--     negate      = gNegate
--     abs         = gAbs
--     signum      = gSignum
--     fromInteger = gFromInteger

-- instance Field1 (Tup a b) (Tup a' b) a a' where
--     _1 = t2_1

-- instance Field2 (Tup a b) (Tup a b') b b' where
--     _2 = t2_2

reTup :: (Backprop a, Backprop b) => Reifies z W => BVar z a -> BVar z b -> BVar z (a :& b)
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
