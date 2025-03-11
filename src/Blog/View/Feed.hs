{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Blog.View.Feed where

import Blog.Types
import Blog.View
import Data.Default
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Time.Format.ISO8601 as ISO8601
import Data.Time.LocalTime
import Data.Time.Zones
import qualified Data.XML.Types as XT
import Text.DublinCore.Types
import qualified Text.Pandoc as P
import Text.RSS.Export
import Text.RSS.Syntax
import qualified Text.XML as X

viewFeed ::
  (?config :: Config) =>
  P.WriterOptions ->
  NonEmpty Entry ->
  TZ ->
  TL.Text
viewFeed wopts entries tz = renderElement . xmlRSS $ feedRss wopts entries tz

renderElement :: XT.Element -> TL.Text
renderElement e =
  X.renderText def $
    X.Document (X.Prologue [] Nothing []) e' []
  where
    e' = either (error . show) id $ X.fromXMLElement e

feedRss ::
  (?config :: Config) =>
  P.WriterOptions ->
  NonEmpty Entry ->
  TZ ->
  RSS
feedRss wopts entries tz =
  (nullRSS feedTitle feedLink)
    { rssChannel = channel,
      rssAttrs = [dcSpec]
    }
  where
    Config {..} = ?config
    now = case maximum $ entryPostTime <$> entries of
      Nothing -> error "No posted entries"
      Just t -> t
    channel =
      (nullChannel feedTitle feedLink)
        { rssDescription = feedDescription,
          rssLanguage = Just "en",
          rssCopyright = Just copyright,
          rssEditor = Just feedAuthor,
          rssWebMaster = Just feedAuthor,
          rssLastUpdate = Just (formatDateRfc now),
          -- , rssCategories =
          rssGenerator = Just "feed-1.0.0.0 (Sigbjorn Finne)",
          rssItems = map rssItem (toList entries),
          rssChannelOther = map dcItemToXml dcData,
          rssImage = Just siteLogo
        }
    dcData =
      [ DCItem DC_Creator feedAuthorName,
        DCItem DC_Language "en",
        DCItem DC_Rights copyright,
        DCItem DC_Date (formatDateIso now),
        DCItem DC_Description feedDescription
      ]
    makeUrl = renderUrl
    formatDateRfc :: LocalTime -> T.Text
    formatDateRfc =
      T.pack
        . formatTime defaultTimeLocale rfc822DateFormat
        . localTimeToUTCTZ tz
    formatDateIso :: LocalTime -> T.Text
    formatDateIso =
      T.pack
        . ISO8601.iso8601Show
        . utctDay
        . localTimeToUTCTZ tz
    feedTitle = confTitle <> " — Entries"
    feedLink = makeUrl "/"
    feedDescription = confDesc
    feedAuthorEmail = authorEmail confAuthorInfo
    feedAuthorName = authorName confAuthorInfo
    feedAuthor = T.concat [feedAuthorEmail, " (", feedAuthorName, ")"]
    copyright = "Copyright " <> confCopyright
    rssItem Entry {..} =
      (nullItem entryTitle)
        { rssItemLink = Just (makeUrl (T.pack entryCanonical)),
          rssItemDescription = Just . T.pack $ copyToHtmlString wopts entryContents,
          rssItemAuthor = Just feedAuthorName,
          rssItemCategories = map rssCategory categs,
          rssItemGuid =
            Just . RSSGuid (Just True) [] $
              makeUrl (T.pack entryCanonical),
          rssItemPubDate = formatDateRfc <$> entryPostTime,
          rssItemOther = map dcItemToXml dcItemData
        }
      where
        categs = flip mapMaybe entryTags $ \(tt, t) ->
          case tt of
            CategoryTag -> Just t
            SeriesTag -> Nothing
            GeneralTag -> Nothing
        dcItemData =
          [ DCItem DC_Creator feedAuthorName,
            DCItem DC_Subject (T.intercalate ", " categs)
          ]
            ++ maybeToList (DCItem DC_Date . formatDateIso <$> entryPostTime)
    rssCategory = RSSCategory Nothing []
    dcSpec =
      ( X.Name "dc" Nothing (Just "xmlns"),
        [XT.ContentText "http://purl.org/dc/elements/1.1/"]
      )
    -- X.Attr
    --   (X.QName "dc" Nothing (Just "xmlns"))
    --   "http://purl.org/dc/elements/1.1/"
    siteLogo =
      nullImage
        (renderUrl "/img/site_logo.jpg")
        confTitle
        (renderUrl "/")

dcItemToXml :: DCItem -> XT.Element
dcItemToXml dcItem = XT.Element eName [] [XT.NodeContent item]
  where
    -- dcItemToXml dcItem = X.Element eName [] [item] Nothing

    eName = X.Name (infoToTag (dcElt dcItem)) Nothing (Just "dc")
    item = XT.ContentText (dcText dcItem)
