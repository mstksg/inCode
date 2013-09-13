
module Development.Blog.Util.Compass (compileCompass) where

import System.Cmd
import Control.Monad
import Config.SiteData
import Web.Blog.Types

compileCompass :: IO ()
compileCompass = void $ rawSystem "compass" ["compile", "-e" , environment]
  where
    environment =
      case siteDataSiteEnvironment siteData of
        SiteEnvironmentDevelopment -> "development"
        SiteEnvironmentProduction  -> "production"
