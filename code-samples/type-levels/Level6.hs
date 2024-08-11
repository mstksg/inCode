#! /usr/bin/env -S nix develop --command runghc -Wall

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Level6 () where

import Data.Bifunctor
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Type.Equality
import Data.Type.Ord
import GHC.TypeNats
import Unsafe.Coerce

-- import Level5 (LTE (..), Nat (..), SNat (..), isLTE)

insertSortedList :: Ord a => a -> [a] -> [a]
insertSortedList x = \case
  [] -> [x]
  y : ys
    | x <= y -> x : y : ys
    | otherwise -> y : insertSortedList x ys

data Entry (n :: Nat) a = Entry a

constProxy :: p n a -> Proxy n
constProxy _ = Proxy

data Sorted :: Nat -> Type -> Type where
  SSingle :: Entry n a -> Sorted n a
  SCons :: (KnownNat m, n <= m) => Entry n a -> Sorted m a -> Sorted n a

type family FlipOrdering o where
  FlipOrdering LT = GT
  FlipOrdering EQ = EQ
  FlipOrdering GT = LT

-- withFlippedOrdering :: forall a b o r. Compare a b ~ o => (Compare b a ~ FlipOrdering o => r) -> r
-- withFlippedOrdering x = case unsafeCoerce Refl :: Compare b a :~: FlipOrdering o of
--   Refl -> x

cmpNatConst :: forall p p' n m a b. (KnownNat n, KnownNat m) => p n a -> p' m b -> OrderingI n m
cmpNatConst _ _ = cmpNat (Proxy @n) (Proxy @m)

data DecideLTE :: Nat -> Nat -> Type where
  DLTE :: (n <= m, Min n m ~ n) => DecideLTE n m
  DGT :: (m <= n, Min n m ~ m) => DecideLTE n m

decideLTE :: (KnownNat a, KnownNat b) => p a -> p b -> DecideLTE a b
decideLTE x y = case cmpNat x y of
  LTI -> DLTE
  EQI -> DLTE
  GTI -> case cmpNat y x of
    LTI -> DGT
    GTI -> error "absurd"

decideLTE2 :: forall a b p p' x y. (KnownNat a, KnownNat b) => p a x -> p' b y -> DecideLTE a b
decideLTE2 _ _ = decideLTE (Proxy @a) (Proxy @b)

-- we can probably do something better without doing a double-compare, maybe
-- pass in the first compare
flipCmpNat ::
  forall a b o. (KnownNat a, KnownNat b, Compare a b ~ o) => Compare b a :~: FlipOrdering o
flipCmpNat = case cmpNat (Proxy @a) (Proxy @b) of
  LTI -> case cmpNat (Proxy @b) (Proxy @a) of
    LTI -> error "absurd"
    GTI -> Refl
  EQI -> Refl
  GTI -> case cmpNat (Proxy @b) (Proxy @a) of
    LTI -> Refl
    GTI -> error "absurd"

flipMin :: forall a b. (KnownNat a, KnownNat b) => Min a b :~: Min b a
flipMin = case cmpNat (Proxy @a) (Proxy @b) of
  LTI -> gcastWith (flipCmpNat @a @b) Refl
  EQI -> Refl
  GTI -> gcastWith (flipCmpNat @a @b) Refl

insertSorted ::
  forall n m a. (KnownNat n, KnownNat m) => Entry n a -> Sorted m a -> Sorted (Min n m) a
insertSorted x = \case
  SSingle y -> case decideLTE2 x y of
    DLTE -> SCons x (SSingle y)
    DGT -> SCons y (SSingle x)
  SCons @q y ys -> case decideLTE2 x y of
    DLTE -> SCons x (SCons y ys)
    DGT -> case decideLTE2 x ys of
      DLTE -> SCons y $ insertSorted x ys
      DGT -> SCons y $ insertSorted x ys

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
    SCons @r y ys -> case decideLTE2 x y of
      DLTE -> case decideLTE2 xs y of
        DLTE -> SCons x $ mergeSorted xs (SCons y ys)
        DGT -> SCons x $ mergeSorted xs (SCons y ys)
      DGT -> case decideLTE2 x ys of
        DLTE -> SCons y $ mergeSorted (SCons x xs) ys
        DGT -> SCons y $ mergeSorted (SCons x xs) ys

-- mergeSorted ::
--   forall n m a.
--   (KnownNat n, KnownNat m) =>
--   Sorted n a ->
--   Sorted m a ->
--   Sorted (Min n m) a
-- mergeSorted = \case
--   SSingle x -> insertSorted x
--   SCons @q x xs -> \case
--     SSingle y ->
--       gcastWith (flipMin @n @m) $
--         insertSorted y (SCons x xs)
--     SCons @r y ys -> case cmpNatConst x y of
--       LTI -> ($ mergeSorted xs (SCons y ys)) case cmpNatConst xs y of
--         LTI -> SCons x :: Sorted q a -> Sorted n a
--         EQI -> SCons x :: Sorted q a -> Sorted n a
--         GTI -> SCons x :: Sorted m a -> Sorted n a
--       EQI -> ($ mergeSorted xs (SCons y ys)) case cmpNatConst xs y of
--         LTI -> SCons x :: Sorted q a -> Sorted n a
--         EQI -> SCons x :: Sorted q a -> Sorted n a
--         GTI -> SCons x :: Sorted m a -> Sorted n a
--       GTI -> gcastWith (flipCmpNat @n @m) $
--         ($ mergeSorted (SCons x xs) ys) case cmpNatConst x ys of
--           LTI -> SCons y :: Sorted n a -> Sorted m a
--           EQI -> SCons y :: Sorted n a -> Sorted m a
--           GTI -> SCons y :: Sorted r a -> Sorted m a

-- GTI -> SCons y $ mergeSorted (SCons x xs) ys
-- case cmpNatConst x y of
-- LTI -> insertSorted y (SCons x xs)

-- case cmpNat (Proxy @n) (Proxy @m) of
-- LTI -> case cmpNat (Proxy @n) (Proxy @q) of
--   LTI -> SCons @n x $ mergeSorted @m @q (SSingle @m y) xs

-- EQI -> SCons @n x $

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
