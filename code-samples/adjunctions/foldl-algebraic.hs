#!/usr/bin/env stack
-- stack --install-ghc ghci --package foldl --package adjunctions --package profunctors --package distributive --package comonad

{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

import           Control.Comonad
import           Control.Foldl            (Fold(..))
import           Data.Distributive
import           Data.Functor.Adjunction
import           Data.Functor.Rep
import qualified Control.Foldl            as F

data EnvList r a = EnvList [r] a
  deriving (Functor, Show, Eq, Ord)

overList :: ([r] -> [s]) -> EnvList r a -> EnvList s a
overList f (EnvList rs x) = EnvList (f rs) x

newtype EL r a = EL {
      runEL :: forall x. (a -> x) -> (x -> r -> x) -> x
    }
  deriving Functor

toOld :: EL r a -> EnvList r a
toOld el = overList reverse $
    runEL el (EnvList []) (\ol r -> overList (r:) ol)

fromOld :: EnvList r a -> EL r a
fromOld (EnvList rs x) = EL $ \nil snoc ->
      let go []     = nil x
          go (y:ys) = go ys `snoc` y
      in  go (reverse rs)

instance Distributive (Fold r) where
    distribute x = Fold (\q r -> go r <$> q) x (fmap extract)
      where
        go x (Fold step init extr) = Fold step (step init x) extr

instance Representable (Fold r) where
    type Rep (Fold r) = [r]
    tabulate = F.foldMap (:[])
    index    = F.fold

foldEL :: Fold r b -> EL r a -> b
foldEL (Fold step init extr) el = extr (runEL el (const init) step)

tabulateEL :: (EL r () -> b) -> Fold r b
tabulateEL = Fold (\el r -> EL $ \nil snoc -> runEL el nil snoc `snoc` r)
                  (EL $ \nil _ -> nil ())

instance Adjunction (EL r) (Fold r) where
    unit x = Fold (\el r -> EL $ \nil snoc -> runEL el nil snoc `snoc` r)
                  (EL $ \nil _ -> nil x)
                  id

    counit el = extract $ runEL el id $ \(Fold step x extr) r ->
        Fold step (step x r) extr

    leftAdjunct f x = Fold (\el r -> EL $ \nil snoc -> runEL el nil snoc `snoc` r)
                           (EL $ \nil _ -> nil x)
                           f

    rightAdjunct f el = extract @(Fold r) $ runEL el f $ \(Fold step x extr) r ->
        Fold step (step x r) extr
