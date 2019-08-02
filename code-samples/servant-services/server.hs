#!/usr/bin/env stack
-- stack --install-ghc runghc --package servant --package servant-server --package containers --package text --package warp --package aeson --resolver nightly-2019-07-31

import           Api
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.IntMap (IntMap)
import           Data.IntSet (IntSet)
import           Data.Text (Text)
import           Network.Wai.Handler.Warp
import           Servant
import qualified Data.IntMap as IM

serveTodoApi :: IORef (IntMap Task) -> Server TodoApi
serveTodoApi taskRef = serveList
                  :<|> serveAdd
                  :<|> serveSet
                  :<|> serveDelete
                  :<|> servePrune
  where
    serveList :: Bool -> Handler (IntMap Task)
    serveList filt = filtFunction <$> liftIO (readIORef taskRef)
      where
        filtFunction
          | filt      = IM.filter (not . taskStatus)
          | otherwise = id
    serveAdd :: Text -> Handler Int
    serveAdd t = liftIO $ atomicModifyIORef' taskRef $ \ts ->
      let newKey = maybe 0 ((+ 1) . fst) (IM.lookupMax ts)
      in  ( IM.insert newKey (Task False t) ts, newKey )
    serveSet :: Int -> Maybe Bool -> Handler ()
    serveSet tid s = liftIO $ atomicModifyIORef' taskRef $ \ts ->
        ( IM.adjust adjuster tid ts, () )
      where
        adjuster (Task c d) = case s of
          Nothing -> Task (not c) d
          Just c' -> Task c'      d
    serveDelete :: Int -> Handler ()
    serveDelete tid = liftIO $ atomicModifyIORef' taskRef $ \ts ->
      ( IM.delete tid ts, () )
    servePrune :: Handler IntSet
    servePrune = liftIO $ atomicModifyIORef' taskRef $ \ts ->
      let (compl,incompl) = IM.partition taskStatus ts
      in  (incompl, IM.keysSet compl)

main :: IO ()
main = do
    taskRef <- newIORef IM.empty
    putStrLn "Launching server..."
    run 3434 $
      serve todoApi (serveTodoApi taskRef)
