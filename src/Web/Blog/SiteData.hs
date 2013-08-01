module Web.Blog.SiteData (
    SiteData
  , siteDataTitle
  , siteDataAuthor
  , siteDataAuthorRel
  , siteData

  ) where

import Data.Text

data SiteData = SiteData
                { siteDataTitle :: Text
                , siteDataAuthor :: Text
                , siteDataAuthorRel :: Text
                }

siteData :: SiteData
siteData =
  SiteData
    "Blog"
    "Justin Le"
    "https://plus.google.com/107705320197444500140/posts"

