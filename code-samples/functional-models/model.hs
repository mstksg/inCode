#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver lts-11.3 --package backprop --package type-combinators --package mwc-random --package hmatrix-backprop --package statistics --package lens --package one-liner-instances --package ghc-typelits-knownnat --package hmatrix-vector-sized -- -Wall -O2

{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE FlexibleInstances                        #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE KindSignatures                           #-}
{-# LANGUAGE MultiParamTypeClasses                    #-}
{-# LANGUAGE PartialTypeSignatures                    #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE TemplateHaskell                          #-}
{-# LANGUAGE TypeApplications                         #-}
{-# LANGUAGE TypeInType                               #-}
{-# LANGUAGE TypeOperators                            #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures     #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fwarn-redundant-constraints          #-}

-- import           Control.Lens hiding                (Profunctor(..))
import           Control.Monad.Primitive
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Foldable
import           Data.Kind
import           Data.List
import           Data.Semigroup
import           GHC.Generics                          (Generic)
import           GHC.TypeNats
import           Lens.Micro
import           Numeric.Backprop
import           Numeric.Backprop.Tuple
import           Numeric.LinearAlgebra.Static.Backprop
import           Numeric.LinearAlgebra.Static.Vector
import           Numeric.OneLiner
import           Statistics.Distribution
import qualified Data.Vector.Sized                     as SV
import qualified Data.Vector.Storable.Sized            as SVS
import qualified Numeric.LinearAlgebra.Static          as H
import qualified System.Random.MWC                     as MWC

type Model p a b = forall s. Reifies s W
                 => BVar s p -> BVar s a -> BVar s b

linReg :: Model (T2 Double Double) Double Double
linReg ab x = b * x + a
  where
    a = ab ^^. _1
    b = ab ^^. _2

squaredErrorGrad
    :: (Num p, Num b)
    => Model p a b      -- ^ Model
    -> a                -- ^ Observed input
    -> b                -- ^ Observed output
    -> p                -- ^ Parameter guess
    -> p                -- ^ Gradient
squaredErrorGrad f x targ = gradBP $ \p ->
    (f p (constVar x) - constVar targ) ^ 2

trainModel
    :: (Fractional p, Num b)
    => Model p a b      -- ^ model to train
    -> p                -- ^ initial parameter guess
    -> [(a,b)]          -- ^ list of observations
    -> p                -- ^ updated parameter guess
trainModel f = foldl' $ \p (x,y) -> p - 0.1 * squaredErrorGrad f x y p

testTrainLinReg :: T2 Double Double
testTrainLinReg = trainModel linReg (T2 0 0) (concat (replicate 1000 samps))
  where
    samps = [(1,1),(2,3),(3,5),(4,7),(5,9)]


logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

feedForwardLog
    :: (KnownNat i, KnownNat o)
    => Model (T2 (L o i) (R o)) (R i) (R o)
feedForwardLog wb x = logistic (w #> x + b)
  where
    w = wb ^^. _1
    b = wb ^^. _2

testTrainPerceptron :: [R 1]
testTrainPerceptron = evalBP2 feedForwardLog trained <$> [ H.vec2 0 0
                                                         , H.vec2 1 0
                                                         , H.vec2 0 1
                                                         , H.vec2 1 1
                                                         ]
  where
    trained = trainModel feedForwardLog (T2 0 0) (concat (replicate 10000 samps))
    samps = [ (H.vec2 0 0, 0)
            , (H.vec2 1 0, 0)
            , (H.vec2 0 1, 0)
            , (H.vec2 1 1, 1)
            ]

logReg :: Model (T2 Double Double) Double Double
logReg ab = logistic . linReg ab

feedForward
    :: (KnownNat i, KnownNat o)
    => Model (T2 (L o i) (R o)) (R i) (R o)
feedForward wb x = w #> x + b
  where
    w = wb ^^. _1
    b = wb ^^. _2

feedForwardLog'
    :: (KnownNat i, KnownNat o)
    => Model (T2 (L o i) (R o)) (R i) (R o)
feedForwardLog' wb = logistic . feedForward wb

softMax :: (Reifies s W, KnownNat n) => BVar s (R n) -> BVar s (R n)
softMax x = konst (1 / sumElements expx) * expx
  where
    expx = exp x

feedForwardSoftMax
    :: (KnownNat i, KnownNat o)
    => Model (T2 (L o i) (R o)) (R i) (R o)
feedForwardSoftMax wb = logistic . feedForward wb

(.<)
    :: (Num p, Num q)
    => Model p b c
    -> Model q a b
    -> Model (T2 p q) a c
(f .< g) pq = f p . g q
  where
    p = pq ^^. _1
    q = pq ^^. _2
infixr 8 .<

testTrainTwoLayer :: [R 1]
testTrainTwoLayer = evalBP2 model trained <$> [ H.vec2 0 0
                                              , H.vec2 1 0
                                              , H.vec2 0 1
                                              , H.vec2 1 1
                                              ]
  where
    model :: Model _ (R 2) (R 1)
    model = feedForwardLog' @4 @1 .< feedForwardLog' @2 @4
    p0 = T2 (T2 (H.gaussianSample 914232 0 (H.sym H.eye)) (H.randomVector 81232 H.Gaussian))
            (T2 (H.gaussianSample 742934 0 (H.sym H.eye)) (H.randomVector 37249 H.Gaussian))
    trained = trainModel model p0 (concat (replicate 10000 samps))
    samps = [ (H.vec2 0 0, 0)
            , (H.vec2 1 0, 1)
            , (H.vec2 0 1, 1)
            , (H.vec2 1 1, 0)
            ]


main :: IO ()
main = do
    putStrLn "Linear regression"
    print testTrainLinReg
    putStrLn "Single layer perceptron learning AND"
    mapM_ print testTrainPerceptron
    putStrLn "Two-layer ANN learning XOR"
    mapM_ print testTrainTwoLayer


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
--     -> Model (T2 p q) (T2 s t) a c
-- f .% g = Model { initParam = \gen -> T2 <$> initParam f gen
--                                         <*> initParam g gen
--                , runModel  = \pq x st ->
--                    let (y, s) = runModel f (pq ^^. _1) x (st ^^. _1)
--                        (z, t) = runModel g (pq ^^. _2) y (st ^^. _2)
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
--     -> Model _ (T2 (FCP i o) (FCP i o)) (R i) (R o)
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
--     -> Model NoState (T2 param state) a b
-- trainState initState m = Model
--     { initParam = \g -> T2 <$> initParam m g
--                            <*> initState g
--     , runModel  = \ps x n -> ( fst $ runModel m (ps ^^. _1) x (ps ^^. _2)
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

-- instance Field1 (T2 a b) (T2 a' b) a a' where
--     _1 = t2_1

-- instance Field2 (T2 a b) (T2 a b') b b' where
--     _2 = t2_2

-- reTup :: (Num a, Num b) => Reifies s W => BVar s a -> BVar s b -> BVar s (T2 a b)
-- reTup = isoVar2 T2 t2Tup
