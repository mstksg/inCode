{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blog.View.Feed where

import           Blog.Types
import           Blog.View
import           Data.Maybe
import           Data.Monoid
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Text.DublinCore.Types
import           Text.RSS.Export
import           Text.RSS.Syntax
import           Text.XML.Light.Output
import qualified Data.Text             as T
import qualified Text.XML.Light.Types  as X

viewFeed
    :: (?config :: Config)
    => [Entry]
    -> TimeZone
    -> UTCTime
    -> String
viewFeed entries tz now = showElement . xmlRSS $ feedRss entries tz now

feedRss
    :: (?config :: Config)
    => [Entry]
    -> TimeZone
    -> UTCTime
    -> RSS
feedRss entries tz now = (nullRSS feedTitle feedLink)
  { rssChannel = channel
  , rssAttrs   = [dcSpec]
  }
  where
    Config{..} = ?config
    channel = (nullChannel feedTitle feedLink)
      { rssDescription   = feedDescription
      , rssLanguage      = Just "en"
      , rssCopyright     = Just copyright
      , rssEditor        = Just feedAuthor
      , rssWebMaster     = Just feedAuthor
      , rssLastUpdate    = Just (formatDateRfc now)
      -- , rssCategories =
      , rssGenerator     = Just "feed-0.3.11.1 (Sigbjorn Finne)"
      , rssItems         = map rssItem entries
      , rssChannelOther  = map dcItemToXml dcData
      , rssImage         = Just siteLogo
      }
    dcData =
      [ DCItem DC_Creator     feedAuthorName
      , DCItem DC_Language    "en"
      , DCItem DC_Rights      copyright
      , DCItem DC_Date        (formatDateIso now)
      , DCItem DC_Description feedDescription
      ]
    makeUrl          = T.unpack . renderUrl
    formatDateRfc    :: UTCTime -> String
    formatDateRfc    = formatTime defaultTimeLocale rfc822DateFormat
    formatDateIso    :: UTCTime -> String
    formatDateIso    = formatTime defaultTimeLocale (iso8601DateFormat Nothing)
    feedTitle        = T.unpack (confTitle <> " — Entries")
    feedLink         = makeUrl "/"
    feedDescription  = T.unpack confDesc
    feedAuthorEmail  = T.unpack (authorEmail confAuthorInfo)
    feedAuthorName   = T.unpack (authorName  confAuthorInfo)
    feedAuthor       = concat [feedAuthorEmail, " (", feedAuthorName, ")"]
    copyright        = T.unpack ("Copyright " <> confCopyright)
    rssItem Entry{..} =
      (nullItem (T.unpack entryTitle))
        { rssItemLink        = Just (makeUrl (T.pack entryCanonical))
        , rssItemDescription = Just (copyToHtmlString (T.unpack entryContents))
        , rssItemAuthor      = Just feedAuthorName
        , rssItemCategories  = map rssCategory categs
        , rssItemGuid        = Just . RSSGuid (Just True) []
                                 $ makeUrl (T.pack entryCanonical)
        , rssItemPubDate     = formatDateRfc . localTimeToUTC tz <$> entryPostTime
        , rssItemOther       = map dcItemToXml dcItemData
        }
      where
        categs = flip mapMaybe entryTags $ \(tt,t) ->
                   case tt of
                     CategoryTag -> Just t
                     _           -> Nothing
        dcItemData =
          [ DCItem DC_Creator feedAuthorName
          , DCItem DC_Subject (T.unpack (T.intercalate ", " categs))
          ] ++ maybeToList (DCItem DC_Date . formatDateIso . localTimeToUTC tz
                          <$> entryPostTime)
    rssCategory tag = RSSCategory Nothing [] (T.unpack tag)
    dcSpec = X.Attr
      (X.QName "dc" Nothing (Just "xmlns"))
      "http://purl.org/dc/elements/1.1/"
    siteLogo =
      nullImage
        ( T.unpack $ renderUrl "/img/site_logo.jpg" )
        ( T.unpack confTitle )
        ( T.unpack $ renderUrl "/" )

dcItemToXml :: DCItem -> X.Element
dcItemToXml dcItem = X.Element eName [] [item] Nothing
  where
    eName = X.QName (infoToTag $ dcElt dcItem) Nothing (Just "dc")
    item = X.Text $ X.CData X.CDataText (dcText dcItem) Nothing
