{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.State.Strict
import Data.ByteString                          (ByteString)
import Data.Foldable                            (sum)
import Data.Word
import Huffman
import PQueue
import Pipes
import Pipes.ByteString                  hiding (ByteString)
import Pipes.Parse
import PreTree
import Prelude hiding                           (sum)
import System.Environment
import System.IO
import qualified Data.ByteString                as B
import qualified Data.Map.Strict                as M
import qualified Pipes.Prelude                  as PP

main :: IO ()
main = do
    args     <- getArgs
    let (inp,out)  = case args of
                       i:o:_      -> (i,o)
                       _          -> error "Give input and output files."

    metadata <- analyzeFile inp
    let (len,tree) = case metadata of
                       Just (l,t) -> (l,t)
                       _          -> error "Empty file."

    encodeFile inp out len tree

-- returns the file length and the huffman encoding tree
analyzeFile :: FilePath -> IO (Maybe (Int, PreTree Word8))
analyzeFile fp = withFile fp ReadMode $ \hIn -> do
    fqs <- freqs (fromHandle hIn >-> bsToBytes)
    let len  = sum fqs
        tree = evalState (listQueueStateTable fqs >> buildTree) emptyPQ
    return $ fmap (len,) tree
  where
    freqs :: (Monad m, Ord a) => Producer a m () -> m (M.Map a Int)
    freqs = PP.fold (\m x -> M.insertWith (+) x 1 m) M.empty id


encodeFile :: FilePath -> FilePath -> Int -> PreTree Word8 -> IO ()
encodeFile inp out len tree = return ()

-- Receive ByteStrings from upstream and send its Word8 components
-- downstream
bsToBytes :: Monad m => Pipe ByteString Word8 m r
bsToBytes = PP.mapFoldable B.unpack
