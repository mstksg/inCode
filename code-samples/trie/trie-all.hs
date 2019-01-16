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
import           Data.Char
import           Data.Either
import           Data.Functor.Foldable
import           Data.Graph.Inductive.Graph        (LNode, LEdge)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.GraphViz                     (GraphvizParams(..))
import           Data.List
import           Data.Map                          (Map)
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Graph.Inductive.Graph        as G
import qualified Data.GraphViz                     as GV
import qualified Data.GraphViz.Attributes.HTML     as HTML
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

singletonCoalg :: v -> [k] -> TrieF k v [k]
singletonCoalg v []     = MkTF (Just v) M.empty
singletonCoalg _ (k:ks) = MkTF Nothing  (M.singleton k ks)

singleton :: [k] -> v -> Trie k v
singleton k v = ana (singletonCoalg v) k

emptyTrie :: Trie k v
emptyTrie = MkT Nothing M.empty

toMapAlg
    :: Ord k
    => TrieF k v (Map [k] v)
    -> Map [k] v
toMapAlg (MkTF mv xs) = case mv of
    Nothing -> M.foldMapWithKey propagate xs
    Just v  -> M.insert [] v $ M.foldMapWithKey propagate xs
  where
    propagate k = M.mapKeys (k:)

fromMapCoalg
    :: Ord k
    => Map [k] v
    -> TrieF k v (Map [k] v)
fromMapCoalg = uncurry rebuild . M.foldMapWithKey splitOut
  where
    rebuild  v      xs = MkTF (getFirst <$> v) (M.fromListWith M.union xs)
    splitOut []     v  = (Just (First v), mempty                 )
    splitOut (k:ks) v  = (mempty        , [(k, M.singleton ks v)])

elemsAlg
    :: TrieF k v [v]
    -> [v]
elemsAlg (MkTF v xs) = maybeToList v ++ foldMap id xs

elems :: Trie k v -> [v]
elems = cata elemsAlg

insertAlg
    :: Ord k
    => v
    -> TrieF k v (Trie k v, [k] -> Trie k v)
    -> [k]
    -> Trie k v
insertAlg v (MkTF v0 xs) = \case
    []   -> MkT (Just v) finished
    k:ks -> MkT v0       $ case M.lookup k inserters of
      Nothing -> M.insert k (singleton ks v) finished
      Just f  -> M.insert k (f ks)           finished
  where
    finished  = fmap fst xs
    inserters = fmap snd xs

insertTrie
    :: Ord k
    => [k]
    -> v
    -> Trie k v
    -> Trie k v
insertTrie k v t0 = para (insertAlg v) t0 k

insertAll
    :: Ord k
    => [([k], v)]
    -> Trie k v
insertAll = foldl' (flip (uncurry insertTrie)) emptyTrie

fresh :: MonadState Int m => m Int
fresh = state $ \i -> (i, i+1)

graphAlg
    :: (MonadState Int m, MonadWriter [Either (LNode (Maybe v)) (LEdge k)] m)
    => TrieF k v (m Int)
    -> m Int
graphAlg (MkTF v xs) = do
    gs <- sequenceA xs
    n  <- fresh
    tell [Left (n, v)]
    _ <- flip M.traverseWithKey gs $ \k top ->
      tell [Right (n, top, k)]
    pure n

singletonMap :: Map k a -> Maybe (k, a)
singletonMap m = do
    (kv, m') <- M.minViewWithKey m
    kv <$ guard (M.null m')

graphAlg'
    :: (MonadState Int m, MonadWriter [Either (LNode (Maybe v)) (LEdge [k])] m)
    => TrieF k v (m (Int, [k]))
    -> m (Int, [k])
graphAlg' (MkTF v xs) = do
    gs <- sequenceA xs
    case guard (isNothing v) *> singletonMap gs of
      Nothing -> do
        n <- fresh
        tell [Left (n, v)]
        _ <- flip M.traverseWithKey gs $ \k (top, ks) ->
          tell [Right (n, top, k:ks)]
        pure (n, [])
      Just (k, (top, ks)) -> pure (top, k:ks)

toGraph
    :: Trie k v
    -> Gr (Maybe v) [k]
toGraph t = G.mkGraph ns es
  where
    (ns, es) = uncurry topOut
             . second partitionEithers
             . runWriter
             . evalStateT (cata graphAlg' t)
             $ 0

topOut
    :: (Int, [k])
    -> ([LNode (Maybe v)], [LEdge [k]])
    -> ([LNode (Maybe v)], [LEdge [k]])
topOut (top, ks) (ns, es)
    | null ks   = (ns, es)
    | otherwise = ((top + 1, Nothing):ns, (top + 1, top, ks):es)

toDot :: (GV.Labellable v) => Trie Char v -> T.Text
toDot = GV.printIt
      . GV.graphToDot params
      . toGraph
  where
    params = GV.nonClusteredParams
      { fmtNode = \(_,  l) -> case l of
          Nothing -> [GV.shape GV.PointShape]
          Just l' -> [GV.toLabel l', GV.shape GV.PlainText]
      , fmtEdge = \(_,_,l) -> [GV.toLabel (concat ["[", l, "]"])]
      }

prequelMemes :: String -> Trie Char HTML.Label
prequelMemes = insertAll . map (uncurry processLine . span (/= ',')) . lines
  where
    processLine qt (drop 1->img) = (filter (not . isSpace) qt, HTML.Table (HTML.HTable Nothing [] [r1,r2]))
      where
        r1 = HTML.Cells [HTML.LabelCell [] (HTML.Text [HTML.Str (T.pack qt)])]
        r2 = HTML.Cells [HTML.ImgCell   [] (HTML.Img [HTML.Src img])]
