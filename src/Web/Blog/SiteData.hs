module Web.Blog.SiteData (
    SiteData
  , siteDataTitle
  , siteData

  ) where

import Data.Text

data SiteData = SiteData
                { siteDataTitle :: Text
                }

siteData :: SiteData
siteData = SiteData "Blog"

