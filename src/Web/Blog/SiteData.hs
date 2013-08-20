module Web.Blog.SiteData (
    siteData
  ) where

import Web.Blog.Types (SiteData(..), DeveloperAPIs(..), AuthorInfo(..), AppPrefs(..))
import qualified Data.Text as T

siteData :: SiteData
siteData =
  SiteData
    "in Code"
    authorInfo
    (T.concat
      [ "Weblog of Justin Le, covering his various adventures in "
      , "programming and explorations in the vast worlds of computation, "
      , "physics, and knowledge."] )
    "2013 Justin Le"
    "blog-dev.jle0.com"
    developerAPIs
    appPrefs
  where
    authorInfo = AuthorInfo
                   "Justin Le"
                   "mstksg@gmail.com"
                   "https://plus.google.com/107705320197444500140"
                   "mstksg"
                   "907281"
                   "107705320197444500140"
                   "lejustin"
    developerAPIs = DeveloperAPIs
                      ("UA-443711-7", "jle0.com")
                      "justinleblogdevelopment"
                      "645245675494525"
                      "ra-520df7c304b817b9"
                      "justinleblogdevelopment"
    appPrefs = AppPrefs
                 6
                 5
                 2
                 15

