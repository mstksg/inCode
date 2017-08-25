#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver nightly-2017-08-20 --package singletons -- -Wall

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeInType         #-}
{-# LANGUAGE TypeOperators      #-}

import           Data.Kind
import           Data.Singletons
import           Data.Singletons.Decide
import           Data.Singletons.TH
import           Prelude hiding         ((++))

$(singletons [d|
  data Nat = Z | S Nat
    deriving Eq
  |])

data Vec :: Nat -> Type -> Type where
    VNil :: Vec 'Z a
    (:+) :: a -> Vec n a -> Vec ('S n) a

infixr 5 :+

mapVec :: (a -> b) -> Vec n a -> Vec n b
mapVec f = \case
    VNil    -> VNil
    x :+ xs -> f x :+ mapVec f xs

zipVec :: Vec n a -> Vec n b -> Vec n (a, b)
zipVec = \case
    VNil -> \case
      VNil -> VNil
    x :+ xs -> \case
      y :+ ys -> (x,y) :+ zipVec xs ys

splitVec_ :: Sing n -> Vec (n + m) a -> (Vec n a, Vec m a)
splitVec_ = \case
    SZ   -> \xs -> (VNil, xs)
    SS l -> \case
      x :+ xs -> case splitVec_ l xs of
        (ys, zs) -> (x :+ ys, zs)

splitVec :: SingI n => Vec (n + m) a -> (Vec n a, Vec m a)
splitVec = splitVec_ sing

type family (n :: Nat) + (m :: Nat) :: Nat where
    'Z   + m = m
    'S n + m = 'S (n + m)

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
(++) = \case
    VNil    -> \ys -> ys
    x :+ xs -> \ys -> x :+ (xs ++ ys)

data Fin :: Nat -> Type where
    FZ :: Fin ('S n)
    FS :: Fin n -> Fin ('S n)

deriving instance Show (Fin n)

index :: Fin n -> Vec n a -> a
index = \case
    FZ -> \case
      x :+ _ -> x
    FS i -> \case
      _ :+ xs -> index i xs

singSize :: Sing (n :: Nat) -> String
singSize = \case
    -- here, n is 'Z
    SZ        -> "Size of zero!"
    -- here, n is ('S 'Z)
    SS SZ     -> "Size of one!"
    -- here, n is ('S ('S n))
    SS (SS _) -> "Wow, so big!"

replicate_ :: Sing n -> a -> Vec n a
replicate_ = \case
    SZ   -> \_ -> VNil
    SS l -> \x -> x :+ replicate_ l x

replicate :: SingI n => a -> Vec n a
replicate = replicate_ sing

generate_ :: Sing n -> (Fin n -> a) -> Vec n a
generate_ = \case
    SZ   -> \_ -> VNil
    SS l -> \f -> f FZ :+ generate_ l (f . FS)

generate :: SingI n => (Fin n -> a) -> Vec n a
generate = generate_ sing

withVec :: [a] -> (forall n. Sing n -> Vec n a -> r) -> r
withVec = \case
    []   -> \f -> f SZ VNil
    x:xs -> \f -> withVec xs $ \l ys ->
        f (SS l) (x :+ ys)

vecLength :: Vec n a -> Sing n
vecLength = \case
    VNil    -> SZ
    _ :+ xs -> SS (vecLength xs)

exactLength_ :: Sing m -> Vec n a -> Maybe (Vec m a)
exactLength_ sM v = case sM %~ vecLength v of
    Proved Refl -> Just v
    Disproved _ -> Nothing

exactLength :: SingI m => Vec n a -> Maybe (Vec m a)
exactLength = exactLength_ sing

exactLengthInductive_ :: Sing m -> Vec n a -> Maybe (Vec m a)
exactLengthInductive_ = \case
    SZ -> \case
      VNil   -> Just VNil
      _ :+ _ -> Nothing
    SS l -> \case
      VNil    -> Nothing
      x :+ xs -> (x :+) <$> exactLengthInductive_ l xs

exactLengthInductive :: SingI m => Vec n a -> Maybe (Vec m a)
exactLengthInductive = exactLengthInductive_ sing

data LTE :: Nat -> Nat -> Type where
    LEZ :: LTE 'Z n
    LES :: LTE n m -> LTE ('S n) ('S m)

isLTE :: Sing n -> Sing m -> Decision (LTE n m)
isLTE = \case
    SZ   -> \_ -> Proved LEZ
    SS n -> \case
      SZ -> Disproved $ \case       -- EmptyCase
      SS m -> case isLTE n m of
        Proved l    -> Proved $ LES l
        Disproved p -> Disproved $ \case
          LES l -> p l

atLeast_ :: Sing n -> Vec m a -> Maybe (LTE n m, Vec m a)
atLeast_ sN v = case isLTE sN (vecLength v) of
    Proved l    -> Just (l, v)
    Disproved _ -> Nothing

atLeast :: SingI n => Vec m a -> Maybe (LTE n m, Vec m a)
atLeast = atLeast_ sing

takeVec :: LTE n m -> Vec m a -> Vec n a
takeVec = \case
    LEZ   -> \_ -> VNil
    LES l -> \case
      x :+ xs -> x :+ takeVec l xs

takeVecMaybe_ :: Sing n -> Vec m a -> Maybe (Vec n a)
takeVecMaybe_ sN v = uncurry takeVec <$> atLeast_ sN v

takeVecMaybe :: SingI n => Vec m a -> Maybe (Vec n a)
takeVecMaybe v = uncurry takeVec <$> atLeast v

main :: IO ()
main = return ()
