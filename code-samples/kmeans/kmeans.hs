#! /usr/bin/env -S nix develop --command runghc

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Bitraversable
import Data.Finite
import Data.Foldable
import Data.Foldable.WithIndex
import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable
import Data.Tuple.Strict (T2 (..))
import qualified Data.Vector.Mutable.Sized as MV
import Data.Vector.Sized (MVector, Vector)
import qualified Data.Vector.Sized as V
import GHC.TypeNats
import Linear.Metric
import Linear.V2
import Linear.Vector
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC
import System.Random.Stateful (StatefulGen (..))

-- import qualified Statistics.Distribution as Stats
-- import qualified Statistics.Distribution.Normal as Stats
-- import qualified Statistics.Distribution.Uniform as Stats

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
  sums_ <- V.freeze sums
  counts_ <- V.freeze counts
  pure $ moveIfNotZero <$> cs0 <*> sums_ <*> counts_
  where
    moveIfNotZero orig tot n
      | n == 0 = orig
      | otherwise = tot ^/ fromInteger n

kMeans ::
  (Metric p, Floating a, Ord a, KnownNat (k + 1), Eq (p a)) =>
  [p a] ->
  Vector (k + 1) (p a)
kMeans pts = go 0 (initialClusters pts)
  where
    go !i !cs
      | cs == cs' || i > 100 = cs
      | otherwise = go (i + 1) cs'
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

silhouettesForCluster ::
  forall k p a.
  (Metric p, Floating a, Ord a, Ord (p a)) =>
  Vector k (Set (p a)) ->
  Finite k ->
  Map (p a) a
silhouettesForCluster xs i = M.fromSet silhouetteForPoint ptsHere
  where
    ptsHere = xs `V.index` i
    silhouetteForPoint p = fromMaybe 0 do
      T2 (First selfMean) (Min otherMean) <- bisequenceA $ foldMap (go p) (V.indexed xs)
      pure $ (otherMean - selfMean) / max selfMean otherMean
    go p (j, ptsThere)
      | i == j = T2 (First <$> meanDist) Nothing
      | otherwise = T2 Nothing (Min <$> meanDist)
      where
        meanDist :: Maybe a
        meanDist
          | totNum > 0 = Just $ totSum / fromInteger totNum
          | otherwise = Nothing
          where
            T2 (Sum totSum) (Sum totNum) =
              foldMap
                (\q -> T2 (Sum (distance p q)) (Sum 1))
                (xs `V.index` j)

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

averageSilhouette ::
  (Metric p, Floating a, Ord a, Ord (p a), KnownNat (k + 1)) =>
  [p a] ->
  Vector (k + 1) (p a) ->
  a
averageSilhouette pts cs = totSum / totNum
  where
    clustered = applyClusters pts cs
    allSilhouettes = foldMap (silhouettesForCluster clustered) finites
    T2 (Sum totSum) (Sum totNum) = foldMap (\x -> T2 (Sum x) (Sum 1)) allSilhouettes

generateSamples ::
  forall p g m.
  (Applicative p, Traversable p, StatefulGen g m) =>
  -- | number of points per cluster
  Int ->
  -- | number of clusters
  Int ->
  g ->
  m [p Double]
generateSamples pts k g = do
  ptsWithSortKey <-
    concat <$> replicateM k do
      center <- sequenceA $ pure @p $ MWC.uniformRM (0, boxSize) g
      replicateM pts do
        ptSortKey <- uniformWord16 g
        pt <- for center \c -> MWC.normal c 0.25 g
        pure (ptSortKey, pt)
  pure $ snd <$> sortOn fst ptsWithSortKey
  where
    dim = length $ pure @p 0
    boxSize = (fromIntegral k ** recip (fromIntegral dim)) * 20

main :: IO ()
main = do
  g <- MWC.createSystemRandom
  samps <- generateSamples @V2 10 4 g
  putStrLn "points"
  mapM_ print samps
  let centers2 :: Vector 2 (V2 Double)
      centers2 = kMeans samps
      centers4 :: Vector 4 (V2 Double)
      centers4 = kMeans samps
      centers8 :: Vector 8 (V2 Double)
      centers8 = kMeans samps
  putStrLn "k = 2"
  print centers2
  print $ averageSilhouette samps centers2
  putStrLn "k = 4"
  print centers4
  print $ averageSilhouette samps centers4
  putStrLn "k = 8"
  print centers8
  print $ averageSilhouette samps centers8
