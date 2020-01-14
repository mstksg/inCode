#!/usr/bin/env stack
-- stack --install-ghc ghci --package foldl --package adjunctions --package profunctors --package distributive --package comonad

{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

import           Control.Comonad
import           Control.Foldl           (Fold(..))
import           Control.Monad
import           Data.Bifunctor
import           Data.Distributive
import           Data.Functor
import           Data.Functor.Adjunction
import           Data.Functor.Rep
import           Data.Profunctor
import qualified Control.Foldl           as F

variance :: Fractional a => Fold a a
variance = do
    x  <- F.mean
    x2 <- lmap (^2) F.mean     -- the mean of squared items
    pure (x2 - x*x)

varianceTooBig :: (Fractional a, Ord a) => Fold a Bool
varianceTooBig = (> 3) <$> variance

newtype SameEither a = SE (Either a a)
  deriving (Show, Eq, Ord)

newtype SameTuple  a = ST (a, a)
  deriving (Show, Eq, Ord)

instance Functor SameEither where
    fmap f (SE e) = SE (bimap f f e)

instance Functor SameTuple where
    fmap f (ST e) = ST (bimap f f e)

instance Distributive SameTuple where
    distribute t = ST (fst . getST <$> t, snd . getST <$> t)
      where
        getST (ST xy) = xy

instance Representable SameTuple where
    type Rep SameTuple = Bool
    tabulate f = ST (f False, f True)
    index (ST (x, y)) = \case
      False -> x
      True  -> y

seST :: (Either a a -> b) -> (a -> (b, b))
seST f x = (f (Left x), f (Right x))

stSE :: (a -> (b, b)) -> (Either a a -> b)
stSE f (Left  x) = fst (f x)
stSE f (Right y) = snd (f y)

instance Adjunction SameEither SameTuple where
    unit x = ST (SE (Left x), SE (Right x))
    counit = \case
        SE (Left  (ST (x, _))) -> x
        SE (Right (ST (_, y))) -> y

    leftAdjunct f x = ST (f (SE (Left x)), f (SE (Right x)))
    rightAdjunct f = \case
        SE (Left  x) -> case f x of ST (a, _) -> a
        SE (Right x) -> case f x of ST (_, b) -> b

data EnvList r a = EnvList [r] a
  deriving (Functor, Show, Eq, Ord)

indexFold :: Fold r b -> EnvList r () -> b
indexFold fld (EnvList rs _) = F.fold fld rs

tabulateFold :: (EnvList r () -> b) -> Fold r b
tabulateFold f = F.foldMap (:[]) (\rs -> f (EnvList rs ()))

instance Distributive (Fold r) where
    distribute x = Fold (\q r -> go r <$> q) x (fmap extract)
      where
        go x (Fold step init extr) = Fold step (step init x) extr

instance Representable (Fold r) where
    type Rep (Fold r) = [r]
    tabulate = F.foldMap (:[])
    index    = F.fold

instance Adjunction (EnvList r) (Fold r) where
    unit x = F.foldMap (:[]) (`EnvList` x)
    counit (EnvList rs fld) = F.fold fld rs

    leftAdjunct f x = F.foldMap (:[]) (\rs -> f (EnvList rs x))
    rightAdjunct f (EnvList rs x) = F.fold (f x) rs

newtype FoldEnv r a = FE { getFE :: Fold r (EnvList r a) }
  deriving Functor

instance Applicative (FoldEnv r) where
    pure  = return
    (<*>) = ap

instance Monad (FoldEnv r) where
    return x = FE $ F.foldMap (:[]) (`EnvList` x)
    FE x >>= f = FE $
      x <&> \(EnvList rs y) -> F.fold (getFE (f y)) rs

newtype EnvFold r a = EF { getEF :: EnvList r (Fold r a) }
  deriving Functor

instance Comonad (EnvFold r) where
    extract (EF (EnvList rs fld)) = F.fold fld rs
    extend f (EF (EnvList rs fld)) = EF $
      EnvList rs $ F.foldMap (:[]) (f . EF . (`EnvList` fld))
