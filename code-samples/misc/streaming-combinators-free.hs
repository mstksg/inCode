#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver lts-16 --package free --package mtl --package list-transformer --package resourcet

{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Control.Exception
import           Control.Monad
import           Control.Monad.Free.TH
import           Control.Monad.Trans
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Resource
import           Data.Char
import           Data.Void
import           System.IO
import           System.IO.Error
import qualified List.Transformer             as L

data PipeF i o a =
    YieldF o a
  | AwaitF (Maybe i -> a)
    deriving Functor

type Pipe i o = FreeT (PipeF i o)

yield :: Monad m => o -> Pipe i o m ()
yield x = liftF $ YieldF x ()

await :: Monad m => Pipe i o m (Maybe i)
await = liftF $ AwaitF id

(.|)
    :: Monad m
    => Pipe a b m x
    -> Pipe b c m y
    -> Pipe a c m y
pf .| pg = do
    gRes <- lift $ runFreeT pg
    case gRes of
      Pure x            -> pure x
      Free (YieldF o x) -> do
        yield o
        pf .| x
      Free (AwaitF g  ) -> do
        fRes <- lift $ runFreeT pf
        case fRes of
          Pure _            -> pure () .| g Nothing
          Free (YieldF o y) -> y       .| g (Just o)
          Free (AwaitF f  ) -> do
            i <- await
            f i .| FreeT (pure gRes)
infixl 1 .|

handlePipeF :: PipeF () Void (m a) -> m a
handlePipeF = \case
    YieldF o _ -> absurd o
    AwaitF f   -> f (Just ())

runPipe :: Monad m => Pipe () Void m a -> m a
runPipe = iterT handlePipeF

sourceHandleIO :: Handle -> Pipe i String IO ()
sourceHandleIO handle = do
    res <- lift $ tryJust (guard . isEOFError) (hGetLine handle)
    case res of
      Left  _   -> return ()
      Right out -> do
        yield out
        sourceHandle handle

sourceHandle :: MonadIO m => Handle -> Pipe i String m ()
sourceHandle handle = do
    res <- liftIO $ tryJust (guard . isEOFError) (hGetLine handle)
    case res of
      Left  _   -> return ()
      Right out -> do
        yield out
        sourceHandle handle

sinkStdoutIO :: Pipe String o IO ()
sinkStdoutIO = do
    inp <- await
    case inp of
      Nothing -> pure ()
      Just x  -> do
        lift $ putStrLn x
        sinkStdout

sinkStdout :: MonadIO m => Pipe String o m ()
sinkStdout = do
    inp <- await
    case inp of
      Nothing -> pure ()
      Just x  -> do
        liftIO $ putStrLn x
        sinkStdout

toUpperPipe :: Monad m => Pipe String String m ()
toUpperPipe = do
    inp <- await
    case inp of
      Nothing -> pure ()
      Just x  -> do
        yield (map toUpper x)
        toUpperPipe

untilSTOP :: Monad m => Pipe String String m ()
untilSTOP = do
    inp <- await
    case inp of
      Nothing -> pure ()
      Just x
        | x == "STOP" -> pure ()
        | otherwise   -> do
            yield x
            untilSTOP

samplePipeIO :: Handle -> Pipe i o IO ()
samplePipeIO handle =
       sourceHandleIO handle
    .| untilSTOP
    .| toUpperPipe
    .| sinkStdoutIO

samplePipe :: Handle -> Pipe i o IO ()
samplePipe handle =
       sourceHandle handle
    .| untilSTOP
    .| toUpperPipe
    .| sinkStdout

main :: IO ()
main = withFile "testpipefile.txt" ReadMode $ \handle ->
    runPipe $ samplePipe handle

-- | Exercise 1: premap and postmap
postMapF :: (o -> o') -> PipeF i o a -> PipeF i o' a
postMapF f (YieldF x n) = YieldF (f x) n
postMapF _ (AwaitF g  ) = AwaitF g

preMapF :: (i' -> i) -> PipeF i o a -> PipeF i' o a
preMapF _ (YieldF x n) = YieldF x n
preMapF f (AwaitF g  ) = AwaitF (g . fmap f)

postMap :: Monad m => (o -> o') -> Pipe i o m a -> Pipe i o' m a
postMap f = transFreeT (postMapF f)

preMap :: Monad m => (i' -> i) -> Pipe i o m a -> Pipe i' o m a
preMap f = transFreeT (preMapF f)

-- | Exercise 2: ResourceT
sourceFile :: MonadIO m => FilePath -> Pipe i String (ResourceT m) ()
sourceFile fp = do
    (_, handle) <- lift $ allocate (openFile fp ReadMode) hClose
    sourceHandle handle

samplePipe2 :: FilePath -> Pipe i o (ResourceT IO) ()
samplePipe2 fp =
       sourceFile fp
    .| untilSTOP
    .| toUpperPipe
    .| hoistFreeT lift sinkStdout

main2 :: IO ()
main2 = runResourceT . runPipe $ samplePipe2 "testpipefile.txt"

-- | Exercise 4: ListT
toListT :: Monad m => Pipe () o m a -> L.ListT m o
toListT p = do
    pRes <- lift $ runFreeT p
    case pRes of
      Pure x             -> L.empty
      Free (YieldF o xs) -> pure o L.<|> toListT xs
      Free (AwaitF g   ) -> toListT $ g (Just ())

fromListT :: Monad m => L.ListT m o -> Pipe i o m ()
fromListT lt = do
    ltRes <- lift $ L.next lt
    case ltRes of
      L.Cons x xs -> do
        yield x
        fromListT xs
      L.Nil       -> pure ()
