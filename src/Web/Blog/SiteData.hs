module Web.Blog.SiteData (
    siteData
  ) where

import Web.Blog.Types (SiteData(..))

siteData :: SiteData
siteData =
  SiteData
    "Blog"
    "Justin Le"
    "home.jle0.com:4268"
    "https://plus.google.com/107705320197444500140/posts"
    10
    5

