{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

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
import Control.Lens
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
          let searchPipe = searchPT t >~ cat
          runEffect $ view P.pack ( encpipe >-> bytes
                                >-> dirs    >-> searchPipe
                                >-> PP.take i )
                  >-> P.toHandle hOut

-- Takes a stream of Directions and yields a byte every time it succesfuly
-- decodes one.  This works because we have Prefix Tree; every direction
-- "traverses down" the tree, and as soon as a leaf is hit, its data is
-- emitted and we move back to the root to start again.
searchPT :: Monad m => PreTree a -> Consumer' Direction m a
searchPT (PTLeaf x)       =
    return x
searchPT (PTNode pt1 pt2) = do
    dir <- await
    case dir of
      DLeft  -> searchPT pt1
      DRight -> searchPT pt2

-- Takes bytestrings from upstream and yields its component bytes
bytes :: Monad m => Pipe B.ByteString Word8 m r
bytes = PP.mapFoldable B.unpack

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

