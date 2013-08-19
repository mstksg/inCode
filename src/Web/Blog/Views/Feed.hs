{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Feed (viewFeed) where

import Text.RSS
import Web.Blog.SiteData
import Web.Blog.Types
import qualified Data.Text      as T
import qualified Data.Text.Lazy as L
import Network.URI
import Data.Time

viewFeed :: SiteRender L.Text
viewFeed = return $ L.pack $ showXML $ rssToXML $ feedRss []

feedRss :: [String] -> RSS
feedRss entryInfos = RSS
            feedTitle
            feedLink
            feedDescription
            feedChannel
            []
  where
    siteDataString r = T.unpack $ r siteData
    feedTitle = siteDataString siteDataTitle
    feedLink = URI "http:" (Just auth) "" "" ""
      where
        auth = URIAuth "" host ""
        host = siteDataString siteDataSiteHost
    feedDescription = siteDataString siteDataDescription
    feedAuthorEmail = siteDataString (authorInfoEmail . siteDataAuthorInfo)
    feedAuthorName = siteDataString (authorInfoName . siteDataAuthorInfo)
    feedAuthor = concat [feedAuthorEmail, " (", feedAuthorName, ")"]
    feedChannel =
      [ Language "en"
      , Copyright "Copyright 2013 Justin Le"
      , ManagingEditor feedAuthor
      , WebMaster feedAuthor
      -- , ChannelPubDate ""
      , LastBuildDate testTime
      -- , ChannelCategory (Just "dmoz") "Computers/Programming/Internet/Personal_Pages"
      -- , ChannelCategory (Just "syndic8") ""
      , Generator "rss-3000.2.0.2 (Bas van Dijk)"
      -- , TTL 60
      -- , Image "" "" "" "" "" ""
      , SkipHours []
      , SkipDays []
      ]


testTime :: UTCTime
testTime = UTCTime (ModifiedJulianDay 10352) 19542
