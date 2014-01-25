module Main where

import "base" Prelude
import Config.SiteData
import Control.Applicative                  ((<$>))
import Data.ByteString                      (ByteString, isPrefixOf)
import Development.Blog.Util
import Network.Wai                          (rawPathInfo)
import Network.Wai.Middleware.Cache
import Network.Wai.Middleware.Headers
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import System.Environment                   (getEnv)
import Web.Blog.Database                    (runDB)
import Web.Blog.Models.Util                 (loadDatabase)
import Web.Blog.Routes
import Web.Blog.Types
import Web.Scotty

main :: IO ()
main = do
  startupHelpers

  port <- case hostConfigPort $ siteDataHostConfig siteData of
    Just p' -> return p'
    Nothing -> read <$> getEnv "PORT"

  db <- runDB loadDatabase

  scotty port $ do

    middleware logStdoutDev
    -- middleware $ addHeaders [("Cache-Control","max-age=86400")]
    middleware headerETag
    middleware $ cache cacheBackend
    middleware $ staticPolicy (noDots >-> addBase "tmp/static")
    middleware $ staticPolicy (noDots >-> addBase "static")
    middleware $ addHeaders [("Cache-Control","max-age=900")]

    route db

cacheBackend :: CacheBackend
cacheBackend app req =
  case lookupETag req of
    Just _  ->
      if anyPrefixes toCache && not (anyPrefixes toNotCache)
        then
          return Nothing
        else
          Just <$> app req
    Nothing -> Just <$> app req
  where
    anyPrefixes = any (`isPrefixOf` rawPathInfo req)

toCache :: [ByteString]
toCache = [
    "/css"
  , "/favicon.ico"
  , "/font"
  , "/img"
  -- , "/js"
  , "/robots.txt"
  ]

toNotCache :: [ByteString]
toNotCache = [ "/img/entries" ]

