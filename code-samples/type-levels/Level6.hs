#! /usr/bin/env -S nix develop --command runghc -Wall

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeAbstractions #-}

module Level6 () where

import Data.Bifunctor
import Data.Proxy
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import GHC.TypeNats
import Data.Type.Ord
-- import Level5 (LTE (..), Nat (..), SNat (..), isLTE)

insertSortedList :: Ord a => a -> [a] -> [a]
insertSortedList x = \case
  [] -> [x]
  y : ys
    | x <= y -> x : y : ys
    | otherwise -> y : insertSortedList x ys

data Sorted :: Nat -> Type -> Type where
  SSingle :: KnownNat n => a -> Sorted n a
  SCons :: forall n m a. (KnownNat m, n <= m) => a -> Sorted m a -> Sorted n a

insertSorted :: forall n m a. (KnownNat n) => a -> Sorted m a -> Sorted (Min n m) a
insertSorted x = \case
  ys@(SSingle y) -> case cmpNat (Proxy @n) (Proxy @m) of
    LTI -> SCons @n x ys
    EQI -> SCons @n x ys
    GTI -> case cmpNat (Proxy @m) (Proxy @n) of
     LTI -> SCons @m y (SSingle @n x)
  ys@(SCons @_ @q z zs) -> case cmpNat (Proxy @n) (Proxy @m) of
    LTI -> SCons @n x ys
    EQI -> SCons @n x ys
    GTI -> case cmpNat (Proxy @m) (Proxy @n) of
     LTI -> case cmpNat (Proxy @n) (Proxy @q) of 
       LTI -> SCons z $ insertSorted @n x zs
       EQI -> SCons z $ insertSorted @n x zs
       GTI -> SCons z $ insertSorted @n x zs

mergeSorted ::
  Sorted n a ->
  Sorted m a ->
  Sorted (Min n m) a
mergeSorted = \case
  -- SNil x -> \ys f -> insertSorted x ys f
  -- SCons xmin x xs -> \case
  --   SNil y -> \f -> compNat x y \case
  --     MinL xly -> mergeSorted xs (SNil y) \case
  --       MinL{} -> \ys -> f (MinL xly) (SCons xmin x ys)
  --       MinR{} -> \ys -> f (MinL xly) (SCons xly x ys)
  --     MinR ylx -> f (MinR ylx) (SCons ylx y (SCons xmin x xs))
  --   SCons ymin y ys -> \f -> compNat x y \case
  --     MinL xly -> mergeSorted xs (SCons ymin y ys) \case
  --       MinL{} -> \zs -> f (MinL xly) (SCons xmin x zs)
  --       MinR{} -> \zs -> f (MinL xly) (SCons xly x zs)
  --     MinR ylx -> mergeSorted (SCons xmin x xs) ys \case
  --       MinL{} -> \zs -> f (MinR ylx) (SCons ylx y zs)
  --       MinR{} -> \zs -> f (MinR ylx) (SCons ymin y zs)


-- -- | TODO: actaully yeah this can be a priority queue! always pop the lowest
-- -- number.
-- data Sorted :: Nat -> Type where
--   SNil :: SNat n -> Sorted n
--   SCons :: LTE n m -> SNat n -> Sorted m -> Sorted n

-- data Min :: Nat -> Nat -> Nat -> Type where
--   MinL :: LTE n m -> Min n m n
--   MinR :: LTE m n -> Min n m m

-- succMin :: Min n m q -> Min (S n) (S m) (S q)
-- succMin = \case
--   MinL l -> MinL (LTES l)
--   MinR l -> MinR (LTES l)

-- compNat :: SNat n -> SNat m -> (forall q. Min n m q -> r) -> r
-- compNat = \case
--   SZ -> \_ -> \f -> f (MinL LTEZ)
--   SS x -> \case
--     SZ -> \f -> f (MinR LTEZ)
--     SS y -> \f -> compNat x y (f . succMin)

-- insertSorted :: SNat n -> Sorted m -> (forall q. Min n m q -> Sorted q -> r) -> r
-- insertSorted x = \case
--   SNil y -> \f -> compNat x y \case
--     MinL xly -> f (MinL xly) $ SCons xly x (SNil y)
--     MinR ylx -> f (MinR ylx) $ SCons ylx y (SNil x)
--   SCons ymin y ys -> \f -> compNat x y \case
--     MinL xly -> f (MinL xly) $ SCons xly x (SCons ymin y ys)
--     MinR ylx -> insertSorted x ys \case
--       MinL{} -> \zs -> f (MinR ylx) (SCons ylx y zs)
--       MinR{} -> \zs -> f (MinR ylx) (SCons ymin y zs)

-- mergeSorted ::
--   Sorted n ->
--   Sorted m ->
--   (forall q. Min n m q -> Sorted q -> r) ->
--   r
-- mergeSorted = \case
--   SNil x -> \ys f -> insertSorted x ys f
--   SCons xmin x xs -> \case
--     SNil y -> \f -> compNat x y \case
--       MinL xly -> mergeSorted xs (SNil y) \case
--         MinL{} -> \ys -> f (MinL xly) (SCons xmin x ys)
--         MinR{} -> \ys -> f (MinL xly) (SCons xly x ys)
--       MinR ylx -> f (MinR ylx) (SCons ylx y (SCons xmin x xs))
--     SCons ymin y ys -> \f -> compNat x y \case
--       MinL xly -> mergeSorted xs (SCons ymin y ys) \case
--         MinL{} -> \zs -> f (MinL xly) (SCons xmin x zs)
--         MinR{} -> \zs -> f (MinL xly) (SCons xly x zs)
--       MinR ylx -> mergeSorted (SCons xmin x xs) ys \case
--         MinL{} -> \zs -> f (MinR ylx) (SCons ylx y zs)
--         MinR{} -> \zs -> f (MinR ylx) (SCons ymin y zs)

-- withSNat :: Nat -> (forall n. SNat n -> r) -> r
-- withSNat = \case
--   Z -> \f -> f SZ
--   S n -> \f -> withSNat n (f . SS)

-- data SomeSorted = forall n. SomeSorted (Sorted n)

-- insertionSort :: NonEmpty Nat -> SomeSorted
-- insertionSort (x0 :| xs0) = withSNat x0 \x -> go xs0 (SomeSorted (SNil x))
--   where
--     go :: [Nat] -> SomeSorted -> SomeSorted
--     go = \case
--       [] -> id
--       x : xs -> \case
--         SomeSorted ys -> withSNat x \x' ->
--           go xs (insertSorted x' ys \_ -> SomeSorted)

main :: IO ()
main = pure ()
