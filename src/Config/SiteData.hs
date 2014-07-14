module Config.SiteData (
    siteData
  ) where

import "base" Prelude
import Web.Blog.Types
import qualified Data.Text as T (concat)

siteData :: SiteData
siteData =
  SiteData
    { siteDataTitle           = "in Code"
    , siteDataAuthorInfo      = authorInfo
    , siteDataDescription     = description
    , siteDataCopyright       = "2013 Justin Le"
    , siteDataPublicBlobs     = Just "https://github.com/mstksg/inCode/blob/master/"
    , siteDataInteractiveUrl  = Just "https://www.fpcomplete.com/user/jle/"
    , siteDataHostConfig      = hostConfig
    , siteDataDeveloperAPIs   = developerAPIs
    , siteDataAppPrefs        = appPrefs
    , siteDataDatabaseConfig  = Nothing
    , siteDataSiteEnvironment = SiteEnvironmentProduction
    , siteDataShareLibs       = Just ".cabal-sandbox/share/x86_64-linux-ghc-7.6.3/"
    , siteDataPackageConf     = Just ".cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d"
    , siteDataPrecompileFay   = True
    }
  where
    description = T.concat
      [ "Weblog of Justin Le, covering his various adventures in "
      , "programming and explorations in the vast worlds of computation, "
      , "physics, and knowledge."]
    authorInfo = AuthorInfo
                   { authorInfoName      = "Justin Le"
                   , authorInfoEmail     = "mstksg@gmail.com"
                   , authorInfoRel       = "https://plus.google.com/107705320197444500140"
                   , authorInfoFacebook  = "mstksg"
                   , authorInfoTwitterID = "907281"
                   , authorInfoGPlus     = "107705320197444500140"
                   , authorInfoGithub    = "mstksg"
                   , authorInfoLinkedIn  = "lejustin"
                   }

    hostConfig = HostConfig
                   { hostConfigHost = "blog.jle.im"
                   , hostConfigPort = Nothing
                   }

    developerAPIs = DeveloperAPIs
                      { developerAPIsAnalytics       = ("UA-443711-8", "jle.im")
                      , developerAPIsDisqusShortname = "incode"
                      , developerAPIsFacebook        = "641852699171929"
                      , developerAPIsAddThis         = "ra-5234d67a6b68dcd4"
                      , developerAPIsFeedburner      = "incodeblog"
                      }

    appPrefs = AppPrefs
                 { appPrefsSlugLength  = 8
                 , appPrefsHomeEntries = 8
                 , appPrefsLedeMax     = 6
                 , appPrefsFeedEntries = 10
                 }
