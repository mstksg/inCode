#! /usr/bin/env -S nix develop --command runghc -Wall

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

compNat :: SNat n -> SNat m -> Either (LTE n m) (LTE m n)
compNat = \case
  SZ -> \_ -> Left LTEZ
  SS n -> \case
    SZ -> Right LTEZ
    SS m -> bimap LTES LTES $ compNat n m

insertSorted :: SNat n -> Sorted m -> Either (Sorted n) (Sorted m)
insertSorted n = \case
  SNil m -> case compNat n m of
    Left l -> Left $ SCons l n (SNil m)
    Right l -> Right $ SCons l m (SNil n)
  SCons l m xs -> case compNat n m of
    Left l' -> Left $ SCons l' n (SCons l m xs)
    Right l' -> SCons l m <$> insertSorted n xs

-- case compNat n m of
-- Left l  -> \f -> f l (SCons l n $ SNil m)
-- Right r ->
-- Left $ SCons l n $ SNil m
--  Nothing -> Right $ SCons _ m $ SNil n

-- isLTE :: SNat n -> SNat m -> Maybe (LTE n m)

main :: IO ()
main = pure ()
