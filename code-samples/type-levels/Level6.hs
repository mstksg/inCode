#! /usr/bin/env -S nix develop --command runghc -Wall

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Level6 () where

import Data.Bifunctor
import Data.Kind
import Level5 (LTE (..), Nat (..), SNat (..), isLTE)

insertSortedList :: Ord a => a -> [a] -> [a]
insertSortedList x = \case
  [] -> [x]
  y : ys
    | x <= y -> x : y : ys
    | otherwise -> y : insertSortedList x ys

data Sorted :: Nat -> Type where
  SNil :: SNat n -> Sorted n
  SCons :: LTE n m -> SNat n -> Sorted m -> Sorted n

data Min :: Nat -> Nat -> Nat -> Type where
  MinL :: LTE n m -> Min n m n
  MinR :: LTE m n -> Min n m m

succMin :: Min n m q -> Min (S n) (S m) (S q)
succMin = \case
    MinL l -> MinL (LTES l)
    MinR l -> MinR (LTES l)

compNat :: SNat n -> SNat m -> (forall q. Min n m q -> r) -> r
compNat = \case
  SZ -> \_ -> \f -> f (MinL LTEZ)
  SS n -> \case
    SZ -> \f -> f (MinR LTEZ)
    SS m -> \f -> compNat n m (f . succMin)

insertSorted :: SNat n -> Sorted m -> (forall q. Min n m q -> Sorted q -> r) -> r
insertSorted n = \case
  SNil m -> \f -> compNat n m \case
    MinL l -> f (MinL l) $ SCons l n (SNil m)
    MinR l -> f (MinR l) $ SCons l m (SNil n)
  SCons l m xs -> \f -> compNat n m \case
    MinL l' -> f (MinL l') $ SCons l' n (SCons l m xs)
    MinR l' -> insertSorted n xs \case
      MinL l'' -> \ys -> f (MinR l') $ SCons l' m ys
      MinR l'' -> \ys -> f (MinR l') (SCons l m ys)

-- mergeSorted
--     :: Sorted n
--     -> Sorted m
--     -> (forall q. Min n m q -> Sorted q -> r)
--     -> r
-- mergeSorted = _

-- case compNat n m of
-- Left l  -> \f -> f l (SCons l n $ SNil m)
-- Right r ->
-- Left $ SCons l n $ SNil m
--  Nothing -> Right $ SCons _ m $ SNil n

-- isLTE :: SNat n -> SNat m -> Maybe (LTE n m)

main :: IO ()
main = pure ()
