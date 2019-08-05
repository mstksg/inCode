#!/usr/bin/env stack
-- stack --install-ghc runghc --package servant --package servant-cli --package containers --package text --package warp --package aeson --package optparse-applicative --resolver nightly-2019-07-31

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeInType        #-}
{-# OPTIONS_GHC -Wall          #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

import           Api
import           Control.Exception
import           Data.IntMap (IntMap)
import           Data.List
import           Data.Proxy
import           Data.Text (Text)
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Options.Applicative
import           Servant.API
import           Servant.CLI
import           Servant.Client
import           Text.Printf
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Text as T

instance ToParam (QueryFlag "filtered") where
    toParam _ = DocQueryParam "filtered" [] "Whether or not to filter completed items" Flag
instance ToParam (QueryParam' '[Required] "desc" Text) where
    toParam _ = DocQueryParam "desc" [] "Task description" Normal
instance ToParam (QueryParam "completed" Bool) where
    toParam _ = DocQueryParam "completed" ["True","False"] "Set status to (leave out for toggle)" Normal
instance ToCapture (Capture "id" Int) where
    toCapture _ = DocCapture "id" "ID number of task"

main :: IO ()
main = do
    c <- parseHandleClient todoApi (Proxy :: Proxy ClientM)
        ( header "todo" <> progDesc "Todo TCP/IP service client" )
        ( displayList
     :<|> (\i -> "Added with ID " ++ show i)
     :<|> const "Set!"
     :<|> const "Deleted!"
     :<|> (\ts -> "Cleared items: " ++ intercalate ", " (map show (IS.toList ts)))
        )

    res <- newManager defaultManagerSettings >>= \mgr ->
      runClientM c $
        mkClientEnv mgr (BaseUrl Http "localhost" 3434 "")

    case res of
      Left  e -> throwIO e
      Right r -> putStrLn r

displayList :: IntMap Task -> String
displayList = unlines
            . map (\(k, t) -> printf "%d) %s" k (displayTask t))
            . IM.toList
  where
    displayTask (Task c t)
      | c         = "[x] " ++ T.unpack t
      | otherwise = "[ ] " ++ T.unpack t
