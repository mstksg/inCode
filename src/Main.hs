{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Web.Blog.Database
import Web.Blog.Routes
import Web.Scotty

main :: IO ()
main = scotty 4268 $ do
  
  liftIO $ runDB blogMigrate

  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "static")

  route

