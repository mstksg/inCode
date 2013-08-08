module Web.Blog.SiteData (
    siteData
  ) where

import Web.Blog.Types (SiteData(..))

siteData :: SiteData
siteData =
  SiteData
    "Blog"
    "Justin Le"
    "blog-dev.jle0.com"
    "https://plus.google.com/107705320197444500140/posts"
    10
    5
    3

