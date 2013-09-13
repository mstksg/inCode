
module Development.Blog.Util.Compass (compileCompass) where

import System.Cmd
import Control.Monad
import Config.SiteData
import Web.Blog.Types
import System.Directory

compileCompass :: IO ()
compileCompass = do
  createDirectoryIfMissing True "tmp/static/css"
  void $ rawSystem "compass" ["compile", "-e" , environment]
  where
    environment =
      case siteDataSiteEnvironment siteData of
        SiteEnvironmentDevelopment -> "development"
        SiteEnvironmentProduction  -> "production"
