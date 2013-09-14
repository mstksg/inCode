module Config.SiteData (
    siteData
  ) where

import Web.Blog.Types
import qualified Data.Text as T (concat)

siteData :: SiteData
siteData =
  SiteData
    { siteDataTitle           = "in Code"
    , siteDataAuthorInfo      = authorInfo
    , siteDataDescription     = description
    , siteDataCopyright       = "2013 Justin Le"
    , siteDataHostConfig      = hostConfig
    , siteDataDeveloperAPIs   = developerAPIs
    , siteDataAppPrefs        = appPrefs
    , siteDataDatabaseConfig  = Nothing
    , siteDataSiteEnvironment = SiteEnvironmentProduction
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
                      { developerAPIsAnalytics       = ("UA-443711-7", "jle0.com")
                      , developerAPIsDisqusShortname = "incode"
                      , developerAPIsFacebook        = "641852699171929"
                      , developerAPIsAddThis         = "ra-5234d67a6b68dcd4"
                      , developerAPIsFeedburner      = "incodeblog"
                      }
    appPrefs = AppPrefs
                 { appPrefsSlugLength  = 8
                 , appPrefsHomeEntries = 5
                 , appPrefsLedeMax     = 2
                 , appPrefsFeedEntries = 15
                 }
    -- databaseConfig = Just DatabaseConfig
    --                    { databaseConfigHost     = "localhost"
    --                    , databaseConfigName     = "test_blog"
    --                    , databaseConfigUser     = "blog-test"
    --                    , databaseConfigPassword = "blog-testblog-test"
    --                    , databaseConfigPort     = 4432
    --                    }
