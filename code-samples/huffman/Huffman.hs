{-# LANGUAGE ScopedTypeVariables #-}

module Huffman where

import Control.Applicative              ((<$>))
import Control.Monad.Trans.State.Strict
import Data.Map.Strict                  (Map)
import Data.Maybe                       (fromJust)
import PQueue
import PreTree
import Weighted
import qualified Data.Map.Strict        as M

type FreqTable a = Map a Int

listFreq :: Ord a => [a] -> FreqTable a
listFreq = foldr f M.empty
  where
    f x = M.insertWith (+) x 1

listQueue :: Ord a => [a] -> PQueue (Weighted a)
listQueue = M.foldrWithKey f emptyPQ . listFreq
  where
    f k v pq = insertPQ (WPair v k) pq

runListFreq :: forall a. Ord a => [a] -> FreqTable a
runListFreq xs = execState listFreqState M.empty
  where
    listFreqState :: State (FreqTable a) ()
    listFreqState = mapM_ addFreq xs

    addFreq :: a -> State (FreqTable a) ()
    addFreq x = modify (M.insertWith (+) x 1)

runListQueue :: Ord a => [a] -> PQueue (WPreTree a)
runListQueue xs = execState (listQueueState xs) emptyPQ

listQueueState :: Ord a => [a] -> State (PQueue (WPreTree a)) ()
listQueueState xs = M.traverseWithKey addNode (listFreq xs) >> return ()
  where
    addNode :: a -> Int -> State (PQueue (WPreTree a)) ()
    addNode x i = modify (insertPQ (WPair i (makePT x)))

buildTree :: State (PQueue (WPreTree a)) (PreTree a)
buildTree = do
    t1  <- fromJust <$> state popPQ         -- queue should never be empty
    t2' <- state popPQ
    case t2' of
        Nothing  ->
            -- We're done!
            return (_wItem t1)              -- break out of the loop
        Just t2 -> do
            -- merge and push
            let combined = mergeWPT t1 t2
            modify (insertPQ combined)
            buildTree                       -- recursive call

runBuildTree :: Ord a => [a] -> PreTree a
runBuildTree xs = evalState (listQueueState xs >> buildTree) emptyPQ
