{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Web.Blog.Routes
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Web.Blog.SiteData
import Web.Blog.Database
import Control.Monad.IO.Class

main :: IO ()
main = scotty 4268 $ do
  
  liftIO $ runDB blogMigrate

  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "static")

  routes siteData

