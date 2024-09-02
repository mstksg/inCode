#! /usr/bin/env -S nix develop --command runghc -Wall

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Level6 (main, Entry (..)) where

import Data.Bifunctor
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Type.Equality
import Data.Type.Ord
import GHC.TypeNats
import Unsafe.Coerce

insertSortedList :: (Int, a) -> [(Int, a)] -> [(Int, a)]
insertSortedList (p, x) = \case
  [] -> [(p, x)]
  (q, y) : ys
    | p <= q -> (p, x) : (q, y) : ys
    | otherwise -> (q, y) : insertSortedList (p, x) ys

newtype Entry (n :: Nat) a = Entry a

instance (KnownNat n, Show a) => Show (Entry n a) where
  show (Entry x) = "Entry @" <> show (natVal (Proxy @n)) <> " " <> show x

data Sorted :: Nat -> Type -> Type where
  SSingle :: Entry n a -> Sorted n a
  SCons :: (KnownNat m, n <= m) => Entry n a -> Sorted m a -> Sorted n a

infixr 5 `SCons`

deriving instance (KnownNat n, Show a) => Show (Sorted n a)

data DecideInsert :: Nat -> Nat -> Type where
  DIZ :: (n <= m, Min n m ~ n) => DecideInsert n m
  DIS :: (m <= n, Min n m ~ m) => DecideInsert n m

decideInsert :: forall a b. (KnownNat a, KnownNat b) => DecideInsert a b
decideInsert = case cmpNat (Proxy @a) (Proxy @b) of
  LTI -> DIZ -- if a < b, DIZ
  EQI -> DIZ -- if a == b, DIZ
  GTI -> case cmpNat (Proxy @b) (Proxy @a) of
    LTI -> DIS -- if a > b, DIZ, except GHC isn't smart enough to know this
    GTI -> error "absurd, we can't have both a > b and b > a"

sConsMin ::
  forall q r n a.
  (KnownNat q, KnownNat r, n <= q, n <= r) =>
  Entry n a ->
  Sorted (Min q r) a ->
  Sorted n a
sConsMin = case cmpNat (Proxy @q) (Proxy @r) of
  LTI -> SCons :: Entry n a -> Sorted q a -> Sorted n a
  EQI -> SCons :: Entry n a -> Sorted q a -> Sorted n a
  GTI -> SCons :: Entry n a -> Sorted r a -> Sorted n a

flipMin :: forall a b. (KnownNat a, KnownNat b) => Min a b :~: Min b a
flipMin = case cmpNat (Proxy @a) (Proxy @b) of
  LTI -> case cmpNat (Proxy @b) (Proxy @a) of
    LTI -> error "absurd"
    GTI -> Refl
  EQI -> Refl
  GTI -> case cmpNat (Proxy @b) (Proxy @a) of
    LTI -> Refl
    GTI -> error "absurd"

insertSorted ::
  forall n m a. (KnownNat n, KnownNat m) => Entry n a -> Sorted m a -> Sorted (Min n m) a
insertSorted x = \case
  SSingle y -> case decideInsert @n @m of
    DIZ -> SCons x (SSingle y)
    DIS -> SCons y (SSingle x)
  SCons @q y ys -> case decideInsert @n @m of
    DIZ -> SCons x (SCons y ys)
    DIS -> sConsMin @n @q y $ insertSorted x ys

mergeSorted ::
  forall n m a.
  (KnownNat n, KnownNat m) =>
  Sorted n a ->
  Sorted m a ->
  Sorted (Min n m) a
mergeSorted = \case
  SSingle x -> insertSorted x
  SCons @q x xs -> \case
    SSingle y ->
      gcastWith (flipMin @n @m) $
        insertSorted y (SCons x xs)
    SCons @r y ys -> case decideInsert @n @m of
      DIZ -> sConsMin @q @m x $ mergeSorted xs (SCons y ys)
      DIS -> sConsMin @n @r y $ mergeSorted (SCons x xs) ys

popSorted ::
  Sorted n a ->
  (Entry n a -> r) ->
  (forall m. (KnownNat m, n <= m) => Entry n a -> Sorted m a -> r) ->
  r
popSorted = \case
  SSingle x -> \f _ -> f x
  SCons x xs -> \_ g -> g x xs

data SomeSorted a = forall n. KnownNat n => SomeSorted (Sorted n a)

deriving instance Show a => Show (SomeSorted a)

someSortedMin :: forall n m a. (KnownNat n, KnownNat m) => Sorted (Min n m) a -> SomeSorted a
someSortedMin = case cmpNat (Proxy @n) (Proxy @m) of
  LTI -> SomeSorted
  EQI -> SomeSorted
  GTI -> SomeSorted

popSomeSorted :: Sorted n a -> (Entry n a, Maybe (SomeSorted a))
popSomeSorted = \case
  SSingle x -> (x, Nothing)
  SCons x xs -> (x, Just (SomeSorted xs))

insertionSort :: forall a. NonEmpty (Natural, a) -> SomeSorted a
insertionSort ((k0, x0) :| xs0) = withSomeSNat k0 \(SNat @k) -> go xs0 (SomeSorted (SSingle (Entry @k x0)))
  where
    go :: [(Natural, a)] -> SomeSorted a -> SomeSorted a
    go [] = id
    go ((k, x) : xs) = \case
      SomeSorted @_ @n ys -> withSomeSNat k \(SNat @k) ->
        go xs $
          someSortedMin @k @n $
            insertSorted (Entry @k x) ys

main :: IO ()
main = print $ insertionSort ((4, 'a') :| [(3, 'b'), (5, 'c'), (4, 'd')])
