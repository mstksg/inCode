{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Feed (viewFeed) where

import Network.URI
import Text.RSS
import Web.Blog.Models
import Web.Blog.Models.Util
import Web.Blog.SiteData
import Web.Blog.Types
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as L
import qualified Database.Persist.Postgresql     as D
import qualified Text.Blaze.Html.Renderer.String as B

viewFeed :: [(D.Entity Entry, (T.Text,[Tag]))] -> SiteRender L.Text
viewFeed entryInfos = return $ L.pack $ showXML $ rssToXML $ feedRss entryInfos

feedRss :: [(D.Entity Entry, (T.Text, [Tag]))] -> RSS
feedRss entryInfos = RSS
            feedTitle
            feedLink
            feedDescription
            feedChannel
            feedItems
  where
    siteDataString r = T.unpack $ r siteData
    feedTitle = siteDataString siteDataTitle ++ " (RSS Feed)"
    feedBaseUri = URI "http:" (Just auth)
      where
        auth = URIAuth "" host ""
        host = siteDataString siteDataSiteHost
    feedLink = feedBaseUri "" "" ""
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
      , LastBuildDate feedBuildDate
      -- , ChannelCategory (Just "dmoz") "Computers/Programming/Internet/Personal_Pages"
      -- , ChannelCategory (Just "syndic8") ""
      , Generator "rss-3000.2.0.2 (Bas van Dijk)"
      -- , TTL 60
      -- , Image "" "" "" "" "" ""
      , SkipHours []
      , SkipDays []
      ]
    feedBuildDate = entryPostedAt $ D.entityVal $ fst $ head entryInfos
    feedItems = map feedItem entryInfos
    feedItem (eEntity@(D.Entity _ entry), (entryUrl, tags)) =
      [ Title $ T.unpack $ entryTitle entry
      , Link $ feedBaseUri (T.unpack entryUrl) "" ""
      , Description $ B.renderHtml $ entryHtml entry
      -- , Author feedAuthorName
      , Comments $ feedBaseUri (T.unpack entryUrl) "" "#disqus_thread"
      , Guid True $ show $ feedBaseUri (T.unpack $ entryPermalink eEntity) "" ""
      , PubDate $ entryPostedAt entry
      ] ++ map categoryElem tags
    categoryElem tag = Category Nothing $ T.unpack $ tagLabel tag
