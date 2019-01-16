#!/usr/bin/env stack
-- stack --install-ghc ghci --package recursion-schemes --package nonempty-containers --package containers --package these --resolver nightly-2019-01-03

{-# LANGUAGE DeriveFunctor                  #-}
{-# LANGUAGE InstanceSigs                   #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE PatternSynonyms                #-}
{-# LANGUAGE TypeFamilies                   #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

import           Data.Foldable
import           Data.Functor.Foldable
import           Data.List.NonEmpty    (NonEmpty(..))
import           Data.Map              (Map)
import           Data.Map.NonEmpty     (NEMap, pattern IsNonEmpty, pattern IsEmpty)
import           Data.Semigroup
import           Data.These
import qualified Data.List.NonEmpty    as NE
import qualified Data.Map              as M
import qualified Data.Map.NonEmpty     as NEM

data Trie k v = TNow v (Map   k (Trie k v))
              | TLater (NEMap k (Trie k v))
  deriving Show

data TrieF k v x = TNowF v (Map   k x)
                 | TLaterF (NEMap k x)
  deriving (Functor, Show)

type instance Base (Trie k v) = TrieF k v

instance Recursive (Trie k v) where
    project :: Trie k v -> TrieF k v (Trie k v)
    project (TNow v xs) = TNowF v xs
    project (TLater xs) = TLaterF xs

instance Corecursive (Trie k v) where
    embed :: TrieF k v (Trie k v) -> Trie k v
    embed (TNowF v xs) = TNow v xs
    embed (TLaterF xs) = TLater xs

singletonCoalg :: v -> [k] -> TrieF k v [k]
singletonCoalg v []     = TNowF v M.empty
singletonCoalg _ (k:ks) = TLaterF (NEM.singleton k ks)

singleton :: [k] -> v -> Trie k v
singleton k v = ana (singletonCoalg v) k

toMapAlg
    :: Ord k
    => TrieF k v (NEMap [k] v)
    -> NEMap [k] v
toMapAlg = \case
    TNowF v (IsNonEmpty xs) -> NEM.insert [] v . NEM.foldMapWithKey propagate $ xs
    TNowF v IsEmpty         -> NEM.singleton [] v
    TLaterF xs              -> NEM.foldMapWithKey propagate xs
  where
    propagate k = NEM.mapKeys (k:)

toMap :: Ord k => Trie k v -> NEMap [k] v
toMap = cata toMapAlg

fromMapCoalg
    :: Ord k
    => NEMap [k] v
    -> TrieF k v (NEMap [k] v)
fromMapCoalg = rebuild . NEM.foldMapWithKey splitOut
  where
    rebuild (This (First v)    ) = TNowF v M.empty
    rebuild (That xs           ) = TLaterF $ NEM.fromListWith NEM.union xs
    rebuild (These (First v) xs) = TNowF v $ M.fromListWith   NEM.union (toList xs)
    splitOut []     v = This (First v)
    splitOut (k:ks) v = That ((k, NEM.singleton ks v) :| [])

fromMap
    :: Ord k
    => NEMap [k] v
    -> Trie k v
fromMap = ana fromMapCoalg

elemsAlg :: TrieF k v [v] -> [v]
elemsAlg (TNowF v xs) = v : foldMap id xs
elemsAlg (TLaterF xs) = foldMap id xs

-- insertAlg
--     :: v
--     -> TrieF k v ([k] -> Trie k v)
--     -> [k]
--     -> Trie k v
-- insertAlg v = \case
--     TNowF v0 xs -> \case
--       []   -> TNow  v  (($ []) <$> xs)
--       k:ks -> TNowF v0 $ M.insertWith _ k v xs
--     -- TLaterF xs -> \case
