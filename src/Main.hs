{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Web.Blog.Routes
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Web.Blog.SiteData

main :: IO ()
main = scotty 4268 $ do

  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "static")

  routes siteData

