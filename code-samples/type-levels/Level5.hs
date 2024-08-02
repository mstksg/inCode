#! /usr/bin/env -S nix develop --command runghc -Wall

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Level5 () where

import Data.Kind
import Data.Void

data Nat = Z | S Nat

data Vec :: Nat -> Type -> Type where
  VNil :: Vec Z a
  (:+) :: a -> Vec n a -> Vec (S n) a

infixr 5 :+

zeroItems :: Vec Z Int
zeroItems = VNil

oneItem :: Vec (S Z) Int
oneItem = 1 :+ VNil

twoItems :: Vec (S (S Z)) Int
twoItems = 1 :+ 2 :+ VNil

threeItems :: Vec (S (S (S Z))) Int
threeItems = 1 :+ 2 :+ 3 :+ VNil

vmap :: (a -> b) -> Vec n a -> Vec n b
vmap f = \case
  VNil -> VNil
  x :+ xs -> f x :+ vmap f xs

vzipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vzipWith f = \case
  VNil -> \case
    VNil -> VNil
  x :+ xs -> \case
    y :+ ys -> f x y :+ vzipWith f xs ys

type family Plus (x :: Nat) (y :: Nat) where
  Plus Z y = y
  Plus (S z) y = S (Plus z y)

type family Times (x :: Nat) (y :: Nat) where
  Times Z y = Z
  Times (S z) y = Plus y (Times z y)

vconcat :: Vec n a -> Vec m a -> Vec (Plus n m) a
vconcat = \case
  VNil -> id
  x :+ xs -> \ys -> x :+ vconcat xs ys

vconcatMap :: (a -> Vec m b) -> Vec n a -> Vec (Times n m) b
vconcatMap f = \case
  VNil -> VNil
  x :+ xs -> f x `vconcat` vconcatMap f xs

data Fin :: Nat -> Type where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

vindex :: Fin n -> Vec n a -> a
vindex = \case
  FZ -> \case
    x :+ _ -> x
  FS i -> \case
    _ :+ xs -> vindex i xs

data LTE :: Nat -> Nat -> Type where
  LTEZ :: LTE Z m
  LTES :: LTE n m -> LTE ('S n) ('S m)

vtake :: LTE n m -> Vec m a -> Vec n a
vtake = \case
  LTEZ -> \_ -> VNil
  LTES l -> \case x :+ xs -> x :+ vtake l xs

data SNat :: Nat -> Type where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

vreplicate :: SNat n -> a -> Vec n a
vreplicate = \case
  SZ -> \_ -> VNil
  SS n -> \x -> x :+ vreplicate n x

class KnownNat n where
  nat :: SNat n

instance KnownNat Z where
  nat = SZ

instance KnownNat n => KnownNat (S n) where
  nat = SS nat

vreplicate' :: KnownNat n => a -> Vec n a
vreplicate' = vreplicate nat

data SomeVec a = forall n. MkSomeVec (SNat n) (Vec n a)

toSomeVec :: [a] -> SomeVec a
toSomeVec = \case
  [] -> MkSomeVec SZ VNil
  x : xs -> case toSomeVec xs of
    MkSomeVec n ys -> MkSomeVec (SS n) (x :+ ys)

withVec :: [a] -> (forall n. SNat n -> Vec n a -> r) -> r
withVec = \case
  [] -> \f -> f SZ VNil
  x : xs -> \f -> withVec xs \n ys -> f (SS n) (x :+ ys)

isLTE :: SNat n -> SNat m -> Maybe (LTE n m)
isLTE = \case
  SZ -> \_ -> Just LTEZ
  SS n -> \case
    SZ -> Nothing
    SS m -> LTES <$> isLTE n m

main :: IO ()
main = pure ()
