#! /usr/bin/env -S nix develop --command runghc

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

import Control.Monad.ST
import Data.Finite
import Data.Foldable
import Data.Foldable.WithIndex
import qualified Data.Vector.Mutable.Sized as MV
import Data.Vector.Sized (MVector, Vector)
import qualified Data.Vector.Sized as V
import GHC.TypeNats
import Linear.Metric
import Linear.Vector

initialClusters :: (Additive p, Fractional a, KnownNat k) => [p a] -> Vector k (p a)
initialClusters pts = runST do
  sums <- MV.replicate zero
  counts <- MV.replicate 0
  ifor_ pts \i p -> do
    let i' = modulo (fromIntegral i)
    MV.modify sums (^+^ p) i'
    MV.modify counts (+ 1) i'
  liftA2 (^/) <$> V.freeze sums <*> (fmap fromInteger <$> V.freeze counts)

moveClusters ::
  (Metric p, Floating a, Ord a, KnownNat (k + 1)) =>
  [p a] ->
  Vector (k + 1) (p a) ->
  Vector (k + 1) (p a)
moveClusters pts cs0 = runST do
  sums <- MV.replicate zero
  counts <- MV.replicate 0
  for_ pts \p -> do
    let closestIx = V.minIndex (distance p <$> cs0)
    MV.modify sums (^+^ p) closestIx
    MV.modify counts (+ 1) closestIx
  liftA2 (^/) <$> V.freeze sums <*> (fmap fromInteger <$> V.freeze counts)

kMeans ::
  (Metric p, Floating a, Ord a, KnownNat (k + 1), Eq (p a)) =>
  [p a] ->
  Vector (k + 1) (p a)
kMeans pts = go (initialClusters pts)
  where
    go !cs
      | cs == cs' = cs
      | otherwise = go cs'
      where
        cs' = moveClusters pts cs

main :: IO ()
main = putStrLn "hello"
