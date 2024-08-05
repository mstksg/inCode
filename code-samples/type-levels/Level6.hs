#! /usr/bin/env -S nix develop --command runghc -Wall

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

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
      MinL _ -> \ys -> f (MinR l') (SCons l' m ys)
      MinR _ -> \ys -> f (MinR l') (SCons l m ys)

mergeSorted ::
  Sorted n ->
  Sorted m ->
  (forall q. Min n m q -> Sorted q -> r) ->
  r
mergeSorted = \case
  SNil n -> \ys f -> insertSorted n ys f
  SCons l n xs -> \case
    SNil m -> \f -> compNat n m \case
      MinL l' -> mergeSorted xs (SNil m) \case
        MinL _ -> \ys -> f (MinL l') (SCons l n ys)
        MinR _ -> \ys -> f (MinL l') (SCons l' n ys)
      MinR l' -> f (MinR l') (SCons l' m (SCons l n xs))
    SCons l' m ys -> \f -> compNat n m \case
      MinL l'' -> mergeSorted xs (SCons l' m ys) \case
        MinL _ -> \ys -> f (MinL l'') (SCons l n ys)
        MinR _ -> \ys -> f (MinL l'') (SCons l'' n ys)
      MinR l'' -> mergeSorted (SCons l n xs) ys \case
        MinL _ -> \ys -> f (MinR l'') (SCons l'' m ys)
        MinR _ -> \ys -> f (MinR l'') (SCons l' m ys)

main :: IO ()
main = pure ()
