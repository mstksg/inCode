module Web.Blog.SiteData (
    siteData
  ) where

import Web.Blog.Types (SiteData(..), DeveloperAPIs(..))

siteData :: SiteData
siteData =
  SiteData
    "in Code"
    "Justin Le"
    "blog-dev.jle0.com"
    "https://plus.google.com/107705320197444500140/posts"
    "justinleblogdevelopment"
    (DeveloperAPIs ("UA-443711-7", "jle0.com") "645245675494525")
    6
    5
    2

