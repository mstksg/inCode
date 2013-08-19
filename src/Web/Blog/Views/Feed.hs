{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Feed (viewFeed) where

import Text.RSS
import Web.Blog.SiteData
import Web.Blog.Types
import qualified Data.Text      as T
import qualified Data.Text.Lazy as L
import Network.URI

viewFeed :: SiteRender L.Text
viewFeed = return $ L.pack $ showXML $ rssToXML feedRss

feedRss :: RSS
feedRss = RSS
            feedTitle
            feedURI
            feedDescription
            []
            []
  where
    siteDataString r = T.unpack $ r siteData
    feedTitle = siteDataString siteDataTitle
    feedURI = URI "http:" (Just auth) "" "" ""
      where
        auth = URIAuth "" host ""
        host = siteDataString siteDataSiteHost
    feedDescription = siteDataString siteDataDescription
    -- feedAuthor = siteDataString (authorInfoEmail . siteDataAuthorInfo)
