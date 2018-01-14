{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blog.View.Feed where

-- import           Text.XML.Light.Output
-- import qualified Text.XML.Light.Types   as X
-- import qualified Text.XML.Stream.Render as X
import           Blog.Types
import           Blog.View
import           Data.Default
import           Data.Maybe
import           Data.Monoid
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Text.DublinCore.Types
import           Text.RSS.Export
import           Text.RSS.Syntax
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import qualified Data.XML.Types            as XT
import qualified Text.XML                  as X

viewFeed
    :: (?config :: Config)
    => [Entry]
    -> TimeZone
    -> UTCTime
    -> TL.Text
viewFeed entries tz now = renderElement . xmlRSS $ feedRss entries tz now

renderElement :: XT.Element -> TL.Text
renderElement e = X.renderText def $
    X.Document (X.Prologue [] Nothing []) e' []
  where
    Right e' = X.fromXMLElement e

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
      , rssGenerator     = Just "feed-1.0.0.0 (Sigbjorn Finne)"
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
    makeUrl          = renderUrl
    formatDateRfc    :: UTCTime -> T.Text
    formatDateRfc    = T.pack . formatTime defaultTimeLocale rfc822DateFormat
    formatDateIso    :: UTCTime -> T.Text
    formatDateIso    = T.pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing)
    feedTitle        = confTitle <> " — Entries"
    feedLink         = makeUrl "/"
    feedDescription  = confDesc
    feedAuthorEmail  = authorEmail confAuthorInfo
    feedAuthorName   = authorName  confAuthorInfo
    feedAuthor       = T.concat [feedAuthorEmail, " (", feedAuthorName, ")"]
    copyright        = "Copyright " <> confCopyright
    rssItem Entry{..} =
      (nullItem entryTitle)
        { rssItemLink        = Just (makeUrl (T.pack entryCanonical))
        , rssItemDescription = Just . T.pack $ copyToHtmlString entryContents
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
          , DCItem DC_Subject (T.intercalate ", " categs)
          ] ++ maybeToList ( DCItem DC_Date . formatDateIso . localTimeToUTC tz
                         <$> entryPostTime
                           )
    rssCategory = RSSCategory Nothing []
    dcSpec = ( X.Name "dc" Nothing (Just "xmlns")
             , [XT.ContentText "http://purl.org/dc/elements/1.1/"]
             )
    -- X.Attr
    --   (X.QName "dc" Nothing (Just "xmlns"))
    --   "http://purl.org/dc/elements/1.1/"
    siteLogo = nullImage (renderUrl "/img/site_logo.jpg")
                         confTitle
                         (renderUrl "/")

dcItemToXml :: DCItem -> XT.Element
dcItemToXml dcItem = XT.Element eName [] [XT.NodeContent item]
-- dcItemToXml dcItem = X.Element eName [] [item] Nothing
  where
    eName = X.Name (infoToTag (dcElt dcItem)) Nothing (Just "dc")
    item = XT.ContentText (dcText dcItem)
