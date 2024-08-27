#! /usr/bin/env -S nix develop --command runghc -Wall

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Level7 () where

import Data.Bifunctor
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Type.Equality
import Data.Type.Ord
import GHC.TypeNats
import Level6 (Entry (..))
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

-- data InsertBounded :: Nat -> Nat -> Nat -> Type -> Type where
--   IBSuccess :: n + m <= lim => Bounded lim (n + m) a -> InsertBounded lim n m a
--   IBFailure :: n + m > lim => InsertBounded lim n m a

reBounded :: forall lim lim' n a. n <= lim' => Bounded lim n a -> Bounded lim' n a
reBounded = \case
  BNil -> BNil
  BCons x xs -> BCons x $ reBounded xs

withBoundedWit :: Bounded lim n a -> (n <= lim => r) -> r
withBoundedWit = \case
  BNil -> \x -> x
  BCons _ _ -> \x -> x

-- lteSum1 :: forall a b c r. (a + b <= c) => (a <= c => r) -> r
-- lteSum1 x = x

-- lteSum2 :: forall a b c r. (a + b <= c) => (b <= c => r) -> r
-- lteSum2 x = x

concatBounded ::
  forall n m lim a.
  (KnownNat n, KnownNat m, KnownNat lim, n + m <= lim) =>
  Bounded lim n a ->
  Bounded lim m a ->
  Bounded lim (n + m) a
concatBounded = \case
  BNil -> id
  BCons @x @xs x xs -> BCons x . concatBounded xs

reverseBounded ::
  forall lim n a. (n <= lim, KnownNat lim, KnownNat n) => Bounded lim n a -> Bounded lim n a
reverseBounded = go BNil
  where
    go ::
      forall m q.
      (KnownNat m, KnownNat q, m <= lim, q <= lim, (m + q) <= lim) =>
      Bounded lim m a ->
      Bounded lim q a ->
      Bounded lim (m + q) a
    go res = \case
      BNil -> res
      BCons @x @xs x xs -> case cmpNat (Proxy @(x + m)) (Proxy @lim) of
        LTI -> go @(x + m) @xs (BCons @x @m x res) xs
        EQI -> go @(x + m) @xs (BCons @x @m x res) xs

data SplitBounded :: Nat -> Nat -> Nat -> Type -> Type where
  SplitBounded ::
    forall q lim lim' n a.
    KnownNat q =>
    Bounded lim' q a ->
    Bounded lim (n - q) a ->
    SplitBounded lim lim' n a

addLTE :: forall a b c r. (KnownNat a, KnownNat b, KnownNat c, a <= c - b) => (a + b <= c => r) -> r
addLTE x = case cmpNat (Proxy @(a + b)) (Proxy @c) of
  LTI -> x
  EQI -> x
  GTI -> error "huh"

-- huh :: (a <= c - b, c >= b, c - b >= 0) => (a + b <= c => r) -> r
-- huh x = x

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
        withBoundedWit ys $
          addLTE @q @x @lim' $
            SplitBounded (BCons x $ reBounded ys) $ _ zs
    EQI -> SplitBounded (BCons x BNil) xs
    GTI -> SplitBounded BNil (BCons x xs)

-- splitBounded :: Bounded lim n a -> (forall q u. (Bounded lim' q a, Bounded (lim - lim') u a) -> r) -> r
-- splitBounded x f = _

-- BNil -> BNil
-- BCons x xs ->

-- BNil -> \ys -> withBoundedWit ys $ IBSuccess ys
-- BCons x xs -> \ys -> case concatBounded xs ys of
--   IBSuccess xsys -> case cmpNat (Proxy @(n + m)) (Proxy @lim) of
--     LTI -> IBSuccess $ BCons x xsys
--     EQI -> IBSuccess $ BCons x xsys
--     GTI -> IBFailure
--   IBFailure -> case cmpNat (Proxy @(n + m)) (Proxy @lim) of
--     LTI -> case cmpNat (Proxy @lim) (Proxy @(n + m)) of
--       LTI -> undefined
--       GTI -> undefined
--     GTI -> undefined

-- IBSuccess $ BCons x xsys

-- LTI -> IBSuccess $ BCons x xs
-- EQI -> IBSuccess $ BCons x xs
-- GTI -> IBFailure

-- * split into two legal bounded queues

-- * reverse

-- * safe head i guess? nah that's dumb

-- * concat

-- * relax

--
-- -- is this dumb?
-- takeBounded
--   :: Bounded lim n a
--   -> Bounded lim
