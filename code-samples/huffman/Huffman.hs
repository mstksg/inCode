{-# LANGUAGE ScopedTypeVariables #-}
-- http://blog.jle.im/entry/streaming-huffman-compression-in-haskell-part-1-trees
-- http://blog.jle.im/entry/streaming-huffman-compression-in-haskell-part-2-binary

module Huffman where

import Control.Applicative              ((<$>))
import Control.Monad.Trans.State.Strict
import Data.Map.Strict                  (Map)
import qualified Data.Map.Strict        as M

import PQueue
import PreTree
import Weighted

-- | Frequency tables
--
-- FreqTable: A map of items to their frequency of occurrence.
type FreqTable a = Map a Int

-- listFreq: Build a frequency table of `a`'s from a list of `a`'s.
listFreq :: Ord a => [a] -> FreqTable a
listFreq = foldr f M.empty
  where
    f x = M.insertWith (+) x 1

-- runListFreq: The same thing as listFreq, but using the mapM_ in a State
--      monad instead of a fold.  Identical, mostly, and sorta pointless.
runListFreq :: forall a. Ord a => [a] -> FreqTable a
runListFreq xs = execState listFreqState M.empty
  where
    listFreqState :: State (FreqTable a) ()
    listFreqState = mapM_ addFreq xs

    addFreq :: a -> State (FreqTable a) ()
    addFreq x = modify (M.insertWith (+) x 1)


-- | List queues
--
-- listQueue: Build a priority queue of weighted `a`'s from a list of
--      `a`'s. Lower-weighted items are prioritized highest.
listQueue :: Ord a => [a] -> PQueue (Weighted a)
listQueue = M.foldrWithKey f emptyPQ . listFreq
  where
    f k v pq = insertPQ (WPair v k) pq

-- runListQueue: The same thing as listQueue, but using traverseWithKey in
--      a State monad.
runListQueue :: Ord a => [a] -> PQueue (WeightedPT a)
runListQueue xs = execState (listQueueState xs) emptyPQ

-- listQueueState: The stateful computation of building a priority list
--      from a given queue, using a PQueue state.
listQueueState :: Ord a => [a] -> State (PQueue (WeightedPT a)) ()
listQueueState xs = M.traverseWithKey addNode (listFreq xs) >> return ()
  where
    addNode :: a -> Int -> State (PQueue (WeightedPT a)) ()
    addNode x i = modify (insertPQ (WPair i (makePT x)))

-- | Building trees
--
-- buildTree: The stateful computation of building a Huffman encoding tree
--      with an underlying PQueue state.  It expects a populated PQueue as
--      an initial state.
buildTree :: State (PQueue (WeightedPT a)) (Maybe (PreTree a))
buildTree = do
    t1' <- state popPQ
    case t1' of
      Nothing ->
        -- queue was empty to begin with, so this fails.
        return Nothing
      Just t1 -> do
        t2' <- state popPQ
        case t2' of
          Nothing  ->
            -- We're done, there was only one item!  Return a `Just` to
            -- indicate success.
            return (Just (_wItem t1))     -- break out of the loop
          Just t2 -> do
            -- merge and push
            let combined = mergeWPT t1 t2
            modify (insertPQ combined)
            buildTree                     -- recursive call

-- runBuildTree: Returns a Huffman-encoded prefix tree of `a`s from the
--      given list of `a`'s.
runBuildTree :: Ord a => [a] -> (Maybe (PreTree a))
runBuildTree xs = evalState (listQueueState xs >> buildTree) emptyPQ

-- | Testing functions
--
-- testTree: Tests an encode-decode cycle.  Does not terminate for strings
--      that contain only one (repeated) character.
testTree :: Ord a => [a] -> [a]
testTree [] = []                    -- handle the empty list
testTree xs = decodeAll pt enc
  where
    Just pt  = runBuildTree xs
    Just enc = encodeAll pt xs

-- testTree': Tests an encode-decode cycle.  Is safe and total.
testTree' :: Ord a => [a] -> Maybe [a]
testTree' xs = do
    pt  <- runBuildTree xs
    enc <- encodeAll pt xs
    decodeAll' pt enc
