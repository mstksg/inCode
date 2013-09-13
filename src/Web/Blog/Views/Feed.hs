{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Feed (viewFeed) where

import Data.Maybe                                (fromJust)
import Data.Time                                 (UTCTime)
import Data.Time.Format                          (formatTime)
import System.Locale
import Text.DublinCore.Types
import Text.RSS.Export
import Text.RSS.Syntax
import Text.XML.Light.Output
import Web.Blog.Models
import Web.Blog.Models.Util
import Web.Blog.Render
import Config.SiteData
import Web.Blog.Types
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as L
import qualified Database.Persist.Postgresql     as D
import qualified Text.Blaze.Html.Renderer.String as B
import qualified Text.XML.Light.Types            as X


viewFeed :: [(D.Entity Entry, (T.Text,[Tag]))] -> UTCTime -> SiteRender L.Text
viewFeed entryInfos now = return $ L.pack $ showElement $ xmlRSS $ feedRss entryInfos now
-- viewFeed entryInfos = return $ L.pack $ showXML $ rssToXML $ feedRss entryInfos

feedRss :: [(D.Entity Entry, (T.Text, [Tag]))] -> UTCTime -> RSS
feedRss entryInfos now = (nullRSS feedTitle feedLink)
  { rssChannel = channel
  , rssAttrs   = [dcSpec]
  }
  where
    channel = (nullChannel feedTitle feedLink)
      { rssDescription   = feedDescription
      , rssLanguage      = Just "en"
      , rssCopyright     = Just copyright
      , rssEditor        = Just feedAuthor
      , rssWebMaster     = Just feedAuthor
      , rssLastUpdate    = Just $ formatDateRfc feedBuildDate
      -- , rssCategories =
      , rssGenerator     = Just "feed-0.3.9.1 (Sigbjorn Finne)"
      , rssItems         = map rssItem entryInfos
      , rssChannelOther  = map dcItemToXml dcData
      }
    dcData =
      [ DCItem DC_Creator feedAuthorName
      , DCItem DC_Language "en"
      , DCItem DC_Rights copyright
      , DCItem DC_Date $ formatDateIso feedBuildDate
      , DCItem DC_Description feedDescription
      ]
    siteDataString r = T.unpack $ r siteData
    makeUrl          = T.unpack . renderUrl'
    formatDateRfc    =
      formatTime defaultTimeLocale rfc822DateFormat . fromJust
    formatDateIso d  =
      formatTime defaultTimeLocale (iso8601DateFormat Nothing) $ fromJust d
    feedTitle        = siteDataString siteDataTitle ++ " — Entries"
    feedLink         = makeUrl ""
    feedDescription  = siteDataString siteDataDescription
    feedBuildDate    = Just now
    feedAuthorEmail  = siteDataString (authorInfoEmail . siteDataAuthorInfo)
    feedAuthorName   = siteDataString (authorInfoName . siteDataAuthorInfo)
    feedAuthor       = concat [feedAuthorEmail, " (", feedAuthorName, ")"]
    copyright        = T.unpack $ T.append "Copyright " $ siteDataCopyright siteData
    rssItem (eEntity@(D.Entity _ entry), (entryUrl, tags)) =
      (nullItem $ T.unpack $ entryTitle entry)
        { rssItemLink        = Just $ makeUrl entryUrl
        , rssItemDescription = Just $ B.renderHtml $ entryHtml entry
        -- , rssItemAuthor   = Just feedAuthorName
        , rssItemCategories  = map rssCategory tags
        , rssItemGuid        = Just $ RSSGuid (Just True) [] $ makeUrl $ entryPermalink eEntity
        , rssItemPubDate     = Just $ formatDateRfc $ entryPostedAt entry
        , rssItemOther       = map dcItemToXml dcItemData
        }
      where
        dcItemData =
          [ DCItem DC_Creator feedAuthorName
          , DCItem DC_Date $ formatDateIso $ entryPostedAt entry
          , DCItem DC_Subject $ T.unpack $ T.intercalate ", " $ map tagLabel tags
          ]
    rssCategory tag = RSSCategory
      Nothing
      [] $
      T.unpack $ tagLabel tag
    dcSpec = X.Attr
      (X.QName "dc" Nothing (Just "xmlns"))
      "http://purl.org/dc/elements/1.1/"

dcItemToXml :: DCItem -> X.Element
dcItemToXml dcItem = X.Element eName [] [item] Nothing
  where
    eName = X.QName (infoToTag $ dcElt dcItem) Nothing (Just "dc")
    item = X.Text $ X.CData X.CDataText (dcText dcItem) Nothing

-- feedRss :: [(D.Entity Entry, (T.Text, [Tag]))] -> RSS
-- feedRss entryInfos = RSS
--             feedTitle
--             feedLink
--             feedDescription
--             feedChannel
--             feedItems
--   where
--     siteDataString r = T.unpack $ r siteData
--     feedTitle = siteDataString siteDataTitle ++ " (RSS Feed)"
--     feedBaseUri = URI "http:" (Just auth)
--       where
--         auth = URIAuth "" host ""
--         host = siteDataString siteDataSiteHost
--     feedLink = feedBaseUri "" "" ""
--     feedDescription = siteDataString siteDataDescription
--     feedAuthorEmail = siteDataString (authorInfoEmail . siteDataAuthorInfo)
--     feedAuthorName = siteDataString (authorInfoName . siteDataAuthorInfo)
--     feedAuthor = concat [feedAuthorEmail, " (", feedAuthorName, ")"]
--     feedChannel =
--       [ Language "en"
--       , Copyright "Copyright 2013 Justin Le"
--       , ManagingEditor feedAuthor
--       , WebMaster feedAuthor
--       -- , ChannelPubDate ""
--       , LastBuildDate feedBuildDate
--       -- , ChannelCategory (Just "dmoz") "Computers/Programming/Internet/Personal_Pages"
--       -- , ChannelCategory (Just "syndic8") ""
--       , Generator "rss-3000.2.0.2 (Bas van Dijk)"
--       -- , TTL 60
--       -- , Image "" "" "" "" "" ""
--       , SkipHours []
--       , SkipDays []
--       ]
--     feedBuildDate = entryPostedAt $ D.entityVal $ fst $ head entryInfos
--     feedItems = map feedItem entryInfos
--     feedItem (eEntity@(D.Entity _ entry), (entryUrl, tags)) =
--       [ Title $ T.unpack $ entryTitle entry
--       , Link $ feedBaseUri (T.unpack entryUrl) "" ""
--       , Description $ B.renderHtml $ entryHtml entry
--       -- , Author feedAuthorName
--       , Comments $ feedBaseUri (T.unpack entryUrl) "" "#disqus_thread"
--       , Guid True $ show $ feedBaseUri (T.unpack $ entryPermalink eEntity) "" ""
--       , PubDate $ entryPostedAt entry
--       ] ++ map categoryElem tags
--     categoryElem tag = Category Nothing $ T.unpack $ tagLabel tag
