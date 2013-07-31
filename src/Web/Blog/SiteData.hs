module Web.Blog.SiteData (
    SiteData
  , siteTitle

  ) where

import Data.Text

data SiteData = SiteData
                { siteTitle :: Text
                }

