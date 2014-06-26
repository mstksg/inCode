{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

-- | decode.hs
--
-- Huffman decoding implementation in Haskell.  For use with encode.hs.
--
-- Usage:
--
-- > ghc -O2 decode.hs
-- > ./decode input.enc output.txt
--
-- http://blog.jle.im/entry/pipes-streaming-huffman-compression-in-haskell-part-3
--

module Main where

-- General imports
import Control.Monad      (forever)
import Lens.Family2       (view)
import System.Environment (getArgs)
import System.IO          (withFile, IOMode(..))

-- Pipes imports
import Pipes
import Pipes.Parse
import qualified Pipes.Binary     as PB
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude    as PP

-- Working with Binary
import Data.Bits                 (testBit)
import Data.ByteString           (ByteString)
import Data.Word                 (Word8)
import qualified Data.ByteString as B

-- Huffman imports
import PreTree

main :: IO ()
main = do
    args     <- getArgs
    let (inp, out)  = case args of
                        i:o:_      -> (i,o)
                        _          -> error "Give input and output files."
    decodeFile inp out

-- The "decoding pipeline"
decodeFile :: FilePath -> FilePath -> IO ()
decodeFile inp out =
    withFile inp ReadMode  $ \hIn  ->
    withFile out WriteMode $ \hOut -> do
      let metadataPipe = PB.fromHandle hIn

      -- consume metapipe to read in the tree/metadata
      (metadata, decodingPipe) <- runStateT PB.decode metadataPipe

      case metadata of
        Left   _          ->
          error "Corrupt metadata."
        Right (len, tree) -> do
          -- do everything with the rest
          let bytesOut  = decodingPipe >-> bsToBytes
                      >-> bytesToDirs  >-> searchPT tree
                      >-> PP.take len
              bsOut     = (view PB.pack) bytesOut
              pipeline  = bsOut
                      >-> PB.toHandle hOut

          runEffect pipeline

-- Uses each incoming direction to "travel down" a Huffman tree and pop
-- a decoded byte downstream every time it reaches a leaf.  Repeats
-- forever.
searchPT :: forall a m r. Monad m
         => PreTree a
         -> Pipe Direction a m r
searchPT t = searchPT' t >~ cat
  where
    searchPT' :: PreTree a -> Consumer' Direction m a
    searchPT' (PTLeaf x)       =
        return x
    searchPT' (PTNode pt1 pt2) = do
        dir <- await
        searchPT' $ case dir of
                      DLeft  -> pt1
                      DRight -> pt2

-- Receive ByteStrings from upstream and send its Word8 components
-- downstream
bsToBytes :: Monad m => Pipe ByteString Word8 m r
bsToBytes = PP.mapFoldable B.unpack


-- Turns a stream of bytes into a stream of directions, yielding eight
-- times per byte.
bytesToDirs :: Monad m => Pipe Word8 Direction m r
bytesToDirs = forever $ do
                mapM_ yield . byteToDirList =<< await
  where
    -- Turns a byte into a list of directions
    byteToDirList :: Word8 -> [Direction]
    byteToDirList b = map f [0..7]
      where
        f i | testBit b i = DRight
            | otherwise   = DLeft
