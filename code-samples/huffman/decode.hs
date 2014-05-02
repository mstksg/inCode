{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.ByteString.Lazy           (ByteString)
import Data.Word
import Huffman
import Pipes
import Pipes.Binary
import Pipes.Parse
import PreTree
import System.IO
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Pipes.ByteString     as P
import qualified Pipes.Prelude        as PP

main :: IO ()
main = decodeFile "result.txt" "decoded.txt"

-- parse the metadata and run the decoding pipeline
decodeFile :: FilePath -> FilePath -> IO ()
decodeFile fpi fpo =
    withFile fpi ReadMode $ \hIn ->
    withFile fpo WriteMode $ \hOut -> do
      let metapipe = P.fromHandle hIn

      -- consume metapipe to read in the tree/metadata
      (res, encpipe) <- runStateT decode metapipe

      case res of
        Left e      -> error "Corrupt metadata."
        Right (i,t) -> do
          -- do everything with the rest
          runEffect $     encpipe    >-> bytes   >-> dirs
                      >-> searchPT t >-> limit i
                      >-> PP.map B.singleton
                      >-> P.toHandle hOut

-- utility function to limit the amount of bytes drawn.  this is because
-- our direction stream actually is padded with zeroes, so it's important
-- to terminate before trying to decode those padded zeroes.
limit :: Monad m => Int -> Pipe a a m r
limit n = do
    PP.take n
    PP.drain

-- Takes a stream of Directions and yields a byte every time it succesfuly
-- decodes one.  This works because we have Prefix Tree; every direction
-- "traverses down" the tree, and as soon as a leaf is hit, its data is
-- emitted and we move back to the root to start again.
searchPT :: forall m r. Monad m => PreTree Word8 -> Pipe Direction Word8 m r
searchPT pt0 = go pt0
  where
    go :: PreTree Word8 -> Pipe Direction Word8 m r
    go (PTLeaf x) = do
      yield x
      go pt0
    go (PTNode pt1 pt2) = do
      dir <- await
      case dir of
        DLeft  -> go pt1
        DRight -> go pt2

-- Takes bytestrings from upstream and yields its component bytes
bytes :: Monad m => Pipe B.ByteString Word8 m r
bytes = forever $
          B.foldl (\p c -> p >> yield c) (return ()) =<< await

-- Turns a stream of bytes into a stream of directions, yielding eight
-- times per byte.
dirs :: Monad m => Pipe Word8 Direction m r
dirs = forever $ do
         mapM_ yield . byteToDirs =<< await

-- Turns a byte into a list of directions
byteToDirs :: Word8 -> [Direction]
byteToDirs b = map f [0..7]
  where
    f i | testBit b i = DRight
        | otherwise   = DLeft

