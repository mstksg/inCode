#! /usr/bin/env -S nix develop --command runghc -Wall

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Level7 (main) where

import Data.Bifunctor
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Type.Equality
import Data.Type.Ord
import GHC.TypeNats
import Level6 (Entry)
import Unsafe.Coerce
import Prelude hiding (Bounded (..))

data Bounded :: Nat -> Nat -> Type -> Type where
  BNil :: Bounded lim 0 a
  BCons ::
    forall n m lim a.
    (KnownNat m, n + m <= lim) =>
    Entry n a ->
    Bounded lim m a ->
    Bounded lim (n + m) a

deriving instance (Show a, KnownNat n) => Show (Bounded lim n a)

reBounded :: forall lim lim' n a. n <= lim' => Bounded lim n a -> Bounded lim' n a
reBounded = \case
  BNil -> BNil
  BCons x xs -> BCons x $ reBounded xs

withBoundedWit :: Bounded lim n a -> (n <= lim => r) -> r
withBoundedWit = \case
  BNil -> \x -> x
  BCons _ _ -> \x -> x

bCons :: KnownNat m => Entry n a -> Bounded lim m a -> Bounded (n + lim) (n + m) a
bCons x xs = withBoundedWit xs $ BCons x (reBounded xs)

concatBounded ::
  forall n m lim a.
  (KnownNat n, KnownNat m, KnownNat lim, n + m <= lim) =>
  Bounded lim n a ->
  Bounded lim m a ->
  Bounded lim (n + m) a
concatBounded = \case
  BNil -> id
  BCons @x @xs x xs -> BCons x . concatBounded xs

solveLte ::
  forall a b c n r.
  (KnownNat a, KnownNat c, KnownNat n, a + b <= n, c <= b) =>
  (a + c <= n => r) ->
  r
solveLte x = case cmpNat (Proxy @(a + c)) (Proxy @n) of
  LTI -> x
  EQI -> x
  GTI -> error "absurd"

reverseBounded ::
  forall lim n a. (n <= lim, KnownNat lim, KnownNat n) => Bounded lim n a -> Bounded lim n a
reverseBounded = go BNil
  where
    go ::
      forall m q.
      (KnownNat m, KnownNat q, m <= lim, m + q <= lim) =>
      Bounded lim m a ->
      Bounded lim q a ->
      Bounded lim (m + q) a
    go res = \case
      BNil -> res
      BCons @x @xs x xs ->
        solveLte @m @q @x @lim $
          go @(x + m) @xs (BCons @x @m x res) xs

data SplitBounded :: Nat -> Nat -> Nat -> Type -> Type where
  SplitBounded ::
    forall q lim lim' n a.
    (KnownNat q, q <= n) =>
    Bounded lim' q a ->
    Bounded lim (n - q) a ->
    SplitBounded lim lim' n a

splitBounded ::
  forall lim lim' n a.
  (KnownNat lim, KnownNat lim', KnownNat n) =>
  Bounded lim n a ->
  SplitBounded lim lim' n a
splitBounded = \case
  BNil -> SplitBounded BNil BNil
  BCons @x @xs x xs -> case cmpNat (Proxy @x) (Proxy @lim') of
    LTI -> case splitBounded @lim @(lim' - x) @xs xs of
      SplitBounded @q ys zs ->
        SplitBounded (bCons x ys) zs
    EQI -> SplitBounded (BCons x BNil) xs
    GTI -> SplitBounded BNil (BCons x xs)

data TakeBounded :: Nat -> Nat -> Type -> Type where
  TakeBounded ::
    forall q lim n a.
    (KnownNat q, q <= n) =>
    Bounded lim q a ->
    TakeBounded lim n a

takeBounded ::
  forall lim lim' n a.
  (KnownNat lim, KnownNat lim', KnownNat n) =>
  Bounded lim n a ->
  TakeBounded lim' n a
takeBounded = \case
  BNil -> TakeBounded BNil
  BCons @x @xs x xs -> case cmpNat (Proxy @x) (Proxy @lim') of
    LTI -> case takeBounded @lim @(lim' - x) xs of
      TakeBounded @q ys -> TakeBounded @(x + q) (bCons x ys)
    EQI -> TakeBounded (BCons x BNil)
    GTI -> TakeBounded BNil

data SomeBounded :: Nat -> Type -> Type where
  SomeBounded :: KnownNat n => Bounded lim n a -> SomeBounded lim a

insertSomeBounded ::
  forall lim n a.
  (KnownNat lim, KnownNat n) =>
  Entry n a ->
  SomeBounded lim a ->
  Maybe (SomeBounded lim a)
insertSomeBounded x (SomeBounded @m xs) = case cmpNat (Proxy @(n + m)) (Proxy @lim) of
  LTI -> Just $ SomeBounded (BCons x xs)
  EQI -> Just $ SomeBounded (BCons x xs)
  GTI -> Nothing

insertReBounded ::
  forall lim n a.
  (KnownNat lim, KnownNat n) =>
  Entry n a ->
  SomeBounded lim a ->
  SomeBounded (lim + n) a
insertReBounded x (SomeBounded @m xs) =
  withBoundedWit xs $
    SomeBounded (BCons x (reBounded xs))

main :: IO ()
main = pure ()
