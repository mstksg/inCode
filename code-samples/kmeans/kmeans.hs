#! /usr/bin/env -S nix develop --command runghc

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

import Control.Monad
import Control.Monad.ST
import Data.Bitraversable
import Data.Finite
import Data.Foldable
import Data.Foldable.WithIndex
import qualified Data.Map as M
import Data.Maybe
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple.Strict (T2 (..))
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

groupAndSum ::
  (Metric p, Floating a, Ord a, KnownNat (k + 1)) =>
  [p a] ->
  Vector (k + 1) (p a) ->
  Vector (k + 1) (p a, Integer)
groupAndSum pts cs0 = runST do
  sums <- MV.replicate zero
  counts <- MV.replicate 0
  for_ pts \p -> do
    let closestIx = V.minIndex (distance p <$> cs0)
    MV.modify sums (^+^ p) closestIx
    MV.modify counts (+ 1) closestIx
  liftA2 (,) <$> V.freeze sums <*> V.freeze counts

silhouetteForCluster ::
  forall k p a.
  (Metric p, Floating a, Ord a, Ord (p a)) =>
  Vector k (Set (p a)) ->
  Finite k ->
  Maybe a
silhouetteForCluster xs i = do
  T2 (First selfMean) (Min otherMean) <- bisequenceA $ foldMap go (V.indexed xs)
  pure $ (otherMean - selfMean) / max selfMean otherMean
  where
    ptsHere = xs `V.index` i
    go (j, ptsThere)
      | i == j = T2 (First <$> meanDist) Nothing
      | otherwise = T2 Nothing (Min <$> meanDist)
      where
        meanDist :: Maybe a
        meanDist = mfilter (> 0) $ flip meanOf ptsHere \p ->
          fromMaybe 0 $
            meanOf (distance p) (p `S.delete` ptsThere)

meanOf :: (Foldable f, Fractional b) => (a -> b) -> f a -> Maybe b
meanOf f xs
  | totNum > 0 = Just (totSum / fromInteger totNum)
  | otherwise = Nothing
  where
    T2 (Sum totSum) (Sum totNum) = foldMap (\x -> T2 (Sum (f x)) (Sum 1)) xs

applyClusters ::
  (Metric p, Floating a, Ord a, Ord (p a), KnownNat (k + 1)) =>
  [p a] ->
  Vector (k + 1) (p a) ->
  Vector (k + 1) (Set (p a))
applyClusters pts cs = V.generate \i -> M.findWithDefault S.empty i mp
  where
    mp =
      M.fromListWith
        (<>)
        [ (closestIx, S.singleton p)
        | p <- pts
        , let closestIx = V.minIndex (distance p <$> cs)
        ]

silhouetteCoefficient ::
  (Metric p, Floating a, Ord a, Ord (p a), KnownNat (k + 1)) =>
  [p a] ->
  Vector (k + 1) (p a) ->
  Maybe a
silhouetteCoefficient pts cs = getMax <$> silhouettes
  where
    clustered = applyClusters pts cs
    silhouettes = foldMap (fmap Max . silhouetteForCluster clustered) finites

main :: IO ()
main = putStrLn "hello"
