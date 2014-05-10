{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Applicative
import Control.Monad hiding                     (mapM_, forM_)
import Control.Monad.Trans.State.Strict
import Data.Binary hiding                       (encodeFile)
import Data.Bits
import Data.ByteString                          (ByteString)
import Data.Char
import Data.Foldable
import Data.List hiding                         (sum)
import Data.Map.Strict                          (Map, (!))
import Data.Monoid
import Data.Word
import Huffman
import Lens.Family2
import PQueue
import Pipes
import Pipes.ByteString                  hiding (ByteString)
import Pipes.Parse
import PreTree
import Prelude hiding                           (sum, mapM_)
import System.IO
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as BL
import qualified Data.Map.Strict                as M
import qualified Pipes.Prelude                  as PP

main :: IO ()
main = do
    f <- analyzeFile "test.txt"
    case f of
      Nothing    -> error "Empty file"
      Just (l,t) -> do
        encodeFile "test.txt" "result.txt" l t

-- returns the file length and the huffman encoding tree
analyzeFile :: FilePath -> IO (Maybe (Int, PreTree Word8))
analyzeFile fp = withFile fp ReadMode $ \hIn -> do
    fqs <- runEffect $ freqs (fromHandle hIn >-> bytes)
    let l    = sum fqs
        tree = evalState (listQueueStateTable fqs >> buildTree) emptyPQ
    return $ fmap (l,) tree
  where
    freqs :: (Monad m, Ord a) => Producer a m () -> m (M.Map a Int)
    freqs = PP.fold (\m x -> M.insertWith (+) x 1 m) M.empty id


-- run the encoding pipeline
encodeFile :: FilePath -> FilePath -> Int -> PreTree Word8 -> IO ()
encodeFile fpi fpo l t =
    withFile fpi ReadMode $ \hIn ->
    withFile fpo WriteMode $ \hOut -> do
      B.hPut hOut . BL.toStrict $ encode l
      B.hPut hOut . BL.toStrict $ encode t
      let dirStream = fromHandle hIn
                  >-> bytes
                  >-> encodeByte tb
      runEffect $ view pack (dirsBytes dirStream)
              >-> toHandle hOut
  where
    tb = ptTable t


-- Transforms a stream of bytes into a stream of directions that encode
-- every byte.
encodeByte :: (Ord a, Monad m) => Map a Encoding -> Pipe a Direction m r
encodeByte t = PP.mapFoldable (t !)

-- Parser that turns a stream of directions into a stream of bytes, by
-- condensing eight directions into one byte.  If the direction stream
-- stops mid-byte, pad it with zero's.  If direction stream is already
-- exhausted, return Nothing.
--
-- Ideally, I'd like to be able to do this without explicit recursion.
--
dirsBytesP :: (Monad m, Functor m) => Parser Direction m (Maybe Word8)
dirsBytesP = do
    isEnd <- isEndOfInput
    if isEnd
      then return Nothing
      else Just <$> go 0 0
  where
    go :: Monad m => Word8 -> Int -> Parser Direction m Word8
    go b 8 = return b
    go b i = do
      dir <- draw
      case dir of
        Just DLeft  -> go     b            (i + 1)
        Just DRight -> go     (setBit b i) (i + 1)
        Nothing     -> return b

-- Transform a Direction producer into a byte/Word8 producer.  Pads the
-- last byte with zeroes if the direction stream runs out mid-byte.
dirsBytes :: (MonadIO m, Functor m) => Producer Direction m r -> Producer Word8 m ()
dirsBytes p = do
    (res,lo) <- lift $ runStateT dirsBytesP p
    forM_ res $ \byte -> do
      yield byte
      dirsBytes lo

-- Receive ByteStrings from upstream and send its Word8 components
-- downstream
bytes :: Monad m => Pipe ByteString Word8 m r
bytes = PP.mapFoldable B.unpack
