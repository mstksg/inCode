{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | encode.hs
--
-- Huffman encoding implementation in Haskell.
--
-- Usage:
--
-- > ghc -O2 encode.hs
-- > ./encode input.txt output.enc
--
-- http://blog.jle.im/entry/pipes-streaming-huffman-compression-in-haskell-part-3
--

module Main where

-- General imports
import Control.Applicative              ((<$>))
import Control.Monad.Trans.State.Strict (evalState)
import Data.Foldable                    (sum)
import Data.Map.Strict                  (Map, (!))
import Lens.Family2                     (view)
import Prelude hiding                   (sum)
import System.Environment               (getArgs)
import System.IO                        (withFile, IOMode(..))
import qualified Data.Map.Strict        as M

-- Pipes imports
import Pipes
import Pipes.Parse
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude    as PP

-- Working with Binary
import Data.Binary hiding             (encodeFile)
import Data.Bits                      (setBit)
import Data.ByteString                (ByteString)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL

-- Huffman imports
import Huffman
import PQueue
import PreTree

main :: IO ()
main = do
    args     <- getArgs
    let (inp, out)  = case args of
                        i:o:_      -> (i,o)
                        _          -> error "Give input and output files."

    metadata <- analyzeFile inp
    let (len, tree) = case metadata of
                        Just (l, t) -> (l, t)
                        Nothing     -> error "Empty File"

    encodeFile inp out len tree

-- returns the file length and the huffman encoding tree
analyzeFile :: FilePath -> IO (Maybe (Int, PreTree Word8))
analyzeFile fp = withFile fp ReadMode $ \hIn -> do
    let byteProducer = PB.fromHandle hIn >-> bsToBytes
    fqs <- freqs byteProducer
    let len  = sum fqs
        tree = evalState (listQueueStateTable fqs >> buildTree) emptyPQ
    return $ fmap (len,) tree
  where
    freqs :: (Monad m, Ord a) => Producer a m () -> m (M.Map a Int)
    freqs = PP.fold f M.empty id
      where
        f m x = M.insertWith (+) x 1 m


-- The "encoding pipeline"
encodeFile :: FilePath -> FilePath -> Int -> PreTree Word8 -> IO ()
encodeFile inp out len tree =
    withFile inp ReadMode  $ \hIn  ->
    withFile out WriteMode $ \hOut -> do
      BL.hPut hOut $ encode (len, tree)
      let dirsOut   = PB.fromHandle hIn
                  >-> bsToBytes
                  >-> encodeByte encTable
          bsOut     = view PB.pack . dirsBytes $ dirsOut
          pipeline  = bsOut
                  >-> PB.toHandle hOut

      runEffect pipeline
  where
    encTable  = ptTable tree

-- Receive ByteStrings from upstream and send its Word8 components
-- downstream
bsToBytes :: Monad m => Pipe ByteString Word8 m r
bsToBytes = PP.mapFoldable B.unpack

-- Transforms a stream of bytes into a stream of directions that encode
-- every byte.
encodeByte :: (Ord a, Monad m)
           => Map a Encoding
           -> Pipe a Direction m r
encodeByte encTable = PP.mapFoldable (encTable !)

-- Transform a Direction producer into a byte/Word8 producer.  Pads the
-- last byte with zeroes if the direction stream runs out mid-byte.
dirsBytes :: (MonadIO m, Functor m)
          => Producer Direction m r
          -> Producer Word8     m ()
dirsBytes p = do
    (result, leftovers) <- lift $ runStateT dirsBytesP p
    case result of
      Just byte -> do
        yield byte
        dirsBytes leftovers
      Nothing   -> return ()

-- Parser that turns a stream of directions into a stream of bytes, by
-- condensing eight directions into one byte.  If the direction stream
-- stops mid-byte, pad it with zero's.  If direction stream is already
-- exhausted, return Nothing.
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

