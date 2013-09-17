{-# LANGUAGE OverloadedStrings #-}

-- import Network.Wai
-- import Web.Blog.Database
import Control.Applicative                  ((<$>))
import Control.Monad.IO.Class
import Development.Blog.Util
import Network.Wai.Middleware.Headers
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import System.Environment                   (getEnv)
import Web.Blog.Routes
import Config.SiteData
import Web.Blog.Types
import Web.Scotty

main :: IO ()
main = do
  port <- case hostConfigPort $ siteDataHostConfig siteData of
    Just p' -> return p'
    Nothing -> read <$> getEnv "PORT"

  scotty port $ do

  liftIO startupHelpers

  middleware logStdoutDev
  middleware $ addHeaders [("Cache-Control","max-age=86400")]
  middleware $ staticPolicy (noDots >-> addBase "static")
  middleware $ staticPolicy (noDots >-> addBase "tmp/static")
  middleware $ addHeaders [("Cache-Control","max-age=900")]

  route

