{-# LANGUAGE OverloadedStrings #-}

-- import Network.Wai
import Control.Monad.IO.Class
import Network.Wai.Middleware.Headers
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Web.Blog.Database
import Web.Blog.Routes
import Web.Scotty

main :: IO ()
main = scotty 4288 $ do

  liftIO $ runDB blogMigrate

  middleware logStdoutDev
  middleware $ addHeaders [("Cache-Control","max-age=3600")]
  middleware $ staticPolicy (noDots >-> addBase "static")
  middleware $ staticPolicy (noDots >-> addBase "tmp/static")
  middleware $ addHeaders [("Cache-Control","max-age=0")]

  route

