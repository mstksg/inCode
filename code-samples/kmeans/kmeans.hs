#! /usr/bin/env -S nix develop --command runghc -Wall

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

import Control.Monad
import Control.Monad.ST
import Data.Finite
import Data.Foldable
import Data.Foldable.WithIndex
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable
import Data.Type.Equality
import qualified Data.Vector.Mutable.Sized as MV
import Data.Vector.Sized (Vector)
import qualified Data.Vector.Sized as V
import GHC.TypeLits.Compare
import GHC.TypeNats
import Linear.Metric
import Linear.V2
import Linear.Vector
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC
import System.Random.Stateful (StatefulGen (..))

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
  forall k p a.
  (Metric p, Floating a, Ord a, KnownNat k, 1 <= k) =>
  [p a] ->
  Vector k (p a) ->
  Vector k (p a)
moveClusters pts cs0 = runST do
  sums <- MV.replicate zero
  counts <- MV.replicate 0
  for_ pts \p -> do
    let closestIx = V.minIndex @_ @(k - 1) (distance p <$> cs0)
    MV.modify sums (^+^ p) closestIx
    MV.modify counts (+ 1) closestIx
  sums_ <- V.freeze sums
  counts_ <- V.freeze counts
  pure $ moveIfNotZero <$> cs0 <*> sums_ <*> counts_
  where
    moveIfNotZero orig tot n
      | n == 0 = orig
      | otherwise = tot ^/ fromInteger n

kMeans ::
  forall k p a.
  (Metric p, Floating a, Ord a, Eq (p a), KnownNat k, 1 <= k) =>
  [p a] ->
  Vector k (p a)
kMeans pts = go 0 (initialClusters pts)
  where
    go :: Int -> Vector k (p a) -> Vector k (p a)
    go !i !cs
      | cs == cs' || i > 100 = cs
      | otherwise = go (i + 1) cs'
      where
        cs' = moveClusters pts cs

kMeans' ::
  forall p a.
  (Metric p, Floating a, Ord a, Eq (p a)) =>
  Natural ->
  [p a] ->
  [p a]
kMeans' k pts = case someNatVal k of
  SomeNat @k pk -> case SNat @1 `isLE` pk of
    Nothing -> []
    Just Refl -> toList $ kMeans @k pts

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

generateSamples ::
  forall p g m.
  (Applicative p, Traversable p, StatefulGen g m) =>
  -- | number of points per cluster
  Int ->
  -- | number of clusters
  Int ->
  g ->
  m ([p Double], [p Double])
generateSamples npts k g = do
  (centers, ptsWithSortKey) <-
    unzip <$> replicateM k do
      center <- sequenceA $ pure @p $ MWC.uniformRM (0, boxSize) g
      pts <- replicateM npts do
        ptSortKey <- uniformWord16 g
        pt <- for center \c -> MWC.normal c 0.1 g
        pure (ptSortKey, pt)
      pure (center, pts)
  let shuffledPoints = snd <$> sortOn fst (concat ptsWithSortKey)
  pure (centers, shuffledPoints)
  where
    dim = length $ pure @p ()
    boxSize = (fromIntegral k ** recip (fromIntegral dim)) * 20

main :: IO ()
main = do
  g <- MWC.createSystemRandom
  (centers, samps) <- generateSamples @V2 10 3 g
  putStrLn "points"
  mapM_ print samps
  putStrLn "actual centers"
  print centers
  putStrLn "kmeans centers"
  print $ kMeans' 3 samps
