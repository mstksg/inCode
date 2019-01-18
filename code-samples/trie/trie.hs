#!/usr/bin/env stack
-- stack --install-ghc ghci --package recursion-schemes --package containers --package fgl --package mtl --package graphviz --package text --resolver nightly-2019-01-03

{-# LANGUAGE DeriveFunctor                  #-}
{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE InstanceSigs                   #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE PatternSynonyms                #-}
{-# LANGUAGE TupleSections                  #-}
{-# LANGUAGE TypeFamilies                   #-}
{-# LANGUAGE ViewPatterns                   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

import           Control.Monad.State
import           Control.Monad.Writer hiding       (First(..))
import           Data.Bifunctor
import           Data.Either
import           Data.Functor.Foldable
import           Data.Graph.Inductive.Graph        (LNode, LEdge)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.GraphViz                     (GraphvizParams(..))
import           Data.Map                          (Map)
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Graph.Inductive.Graph        as G
import qualified Data.GraphViz                     as GV
import qualified Data.GraphViz.Printing            as GV
import qualified Data.Map                          as M
import qualified Data.Text.Lazy                    as T

data Trie k v = MkT (Maybe v) (Map k (Trie k v))
  deriving Show

data TrieF k v x = MkTF (Maybe v) (Map k x)
  deriving (Functor, Show)

type instance Base (Trie k v) = TrieF k v

instance Recursive (Trie k v) where
    project :: Trie k v -> TrieF k v (Trie k v)
    project (MkT v xs) = MkTF v xs

instance Corecursive (Trie k v) where
    embed :: TrieF k v (Trie k v) -> Trie k v
    embed (MkTF v xs) = MkT v xs

testTrie :: Trie Char Int
testTrie = MkT Nothing $ M.fromList [
      ('t', MkT Nothing $ M.fromList [
          ('o', MkT (Just 9) $ M.fromList [
              ( 'n', MkT (Just 3) M.empty )
            ]
          )
        , ('a', MkT Nothing $ M.fromList [
              ( 'x', MkT (Just 2) M.empty )
            ]
          )
        ]
      )
    ]

count :: Trie k v -> Int
count = cata countAlg

countAlg :: TrieF k v Int -> Int
countAlg (MkTF v subtrieCounts)
    | isJust v  = 1 + subtrieTotal
    | otherwise = subtrieTotal
  where
    subtrieTotal = sum subtrieCounts

trieSum :: Num a => Trie k a -> a
trieSum = cata trieSumAlg

trieSumAlg :: Num a => TrieF k a a -> a
trieSumAlg (MkTF v subtrieSums) = fromMaybe 0 v + sum subtrieSums

trieSumExplicit :: Num a => Trie k a -> a
trieSumExplicit (MkT v subtries) =
    fromMaybe 0 v + sum (fmap trieSumExplicit subtries)

trieSumCata :: Num a => Trie k a -> a
trieSumCata = cata $ \(MkTF v subtrieSums) ->
    fromMaybe 0 v + sum subtrieSums

lookup
    :: Ord k
    => [k]
    -> Trie k v
    -> Maybe v
lookup ks t = cata lookupperAlg t ks

lookupperAlg
    :: Ord k
    => TrieF k v ([k] -> Maybe v)
    -> ([k] -> Maybe v)
lookupperAlg (MkTF v lookuppers) = \case
    []   -> v
    k:ks -> case M.lookup k lookuppers of
      Nothing        -> Nothing
      Just lookupper -> lookupper ks

cata' :: (TrieF k v a -> a) -> Trie k v -> a
cata' alg = alg . fmap (cata' alg) . project

singleton :: [k] -> v -> Trie k v
singleton k v = ana (mkSingletonCoalg v) k

mkSingletonCoalg :: v -> ([k] -> TrieF k v [k])
mkSingletonCoalg v = singletonCoalg
  where
    singletonCoalg []     = MkTF (Just v) M.empty
    singletonCoalg (k:ks) = MkTF Nothing  (M.singleton k ks)

fromMap
    :: Ord k
    => Map [k] v
    -> Trie k v
fromMap = ana fromMapCoalg

fromMapCoalg
    :: Ord k
    => Map [k] v
    -> TrieF k v (Map [k] v)
fromMapCoalg = uncurry rebuild . M.foldMapWithKey splitOut
  where
    rebuild  v      xs = MkTF (getFirst <$> v) (M.fromListWith M.union xs)
    splitOut []     v  = (Just (First v), mempty                 )
    splitOut (k:ks) v  = (mempty        , [(k, M.singleton ks v)])

toMap
    :: Ord k
    => Trie k v
    -> Map [k] v
toMap = cata toMapAlg

toMapAlg
    :: Ord k
    => TrieF k v (Map [k] v)
    -> Map [k] v
toMapAlg (MkTF v mp) = M.foldMapWithKey rejoin mp
                    <> foldMap (M.singleton []) v
  where
    rejoin :: k -> Map [k] v -> Map [k] v
    rejoin x = M.mapKeysMonotonic (x:)

fresh :: MonadState Int m => m Int
fresh = state $ \i -> (i, i+1)

singletonMap :: Map k a -> Maybe (k, a)
singletonMap m = do
    (kv, m') <- M.minViewWithKey m
    kv <$ guard (M.null m')

graphAlg
    :: (MonadState Int m, MonadWriter [Either (LNode (Maybe v)) (LEdge [k])] m)
    => TrieF k v (m (Int, [k]))
    -> m (Int, [k])
graphAlg (MkTF v xs) = do
    gs <- sequenceA xs
    case guard (isNothing v) *> singletonMap gs of
      Nothing -> do
        n <- fresh
        tell [Left (n, v)]
        _ <- flip M.traverseWithKey gs $ \k (top, ks) ->
          tell [Right (n, top, k:ks)]
        pure (n, [])
      Just (k, (top, ks)) -> pure (top, k:ks)

runGraph
    :: StateT Int (Writer [Either (LNode (Maybe v)) (LEdge [k])]) (Int, [k])
    -> Gr (Maybe v) [k]
runGraph = uncurry G.mkGraph
         . uncurry topOut
         . second partitionEithers
         . runWriter
         . flip evalStateT 0
  where
    topOut (top, ks) (ns, es)
      | null ks   = (ns, es)
      | otherwise = ((top + 1, Nothing):ns, (top + 1, top, ks):es)


graphDot
    :: GV.Labellable v
    => Gr (Maybe v) String
    -> T.Text
graphDot = GV.printIt . GV.graphToDot params
  where
    params = GV.nonClusteredParams
      { fmtNode = \(_,  l) -> case l of
          Nothing -> [GV.shape GV.PointShape]
          Just l' -> [GV.toLabel l', GV.shape GV.PlainText]
      , fmtEdge = \(_,_,l) -> [GV.toLabel (concat ["[", l, "]"])]
      }

toGraph
    :: Ord k
    => Map [k] v
    -> Gr (Maybe v) [k]
toGraph = runGraph . hylo graphAlg fromMapCoalg

toDot
    :: GV.Labellable v
    => Map String v
    -> T.Text
toDot = graphDot . toGraph
