{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Models.Entry  where

import Control.Applicative                   ((<$>))
import Control.Monad
import Control.Monad.IO.Class                (liftIO)
import Control.Monad.Loops                   (firstM)
import Data.List                             (groupBy)
import Data.Maybe                            (isNothing, fromJust, isJust)
import Data.Time
import Web.Blog.Models
import Web.Blog.Models.Types
import Web.Blog.SiteData
import Web.Blog.Types
import Web.Blog.Util
import qualified Data.Text                   as T
-- import qualified Database.Esqueleto          as E
import qualified Database.Persist.Postgresql as D
import qualified Text.Blaze.Html5            as H
import qualified Text.Pandoc                 as P

slugLength :: Int
slugLength = siteDataSlugLength siteData
ledeMax :: Int
ledeMax = siteDataLedeMax siteData

insertEntry :: Entry -> D.SqlPersistM (Maybe (D.Key Entry))
insertEntry entry = do
  entryKey <- D.insertUnique entry
  when (isJust entryKey) $
    insertSlug $ D.Entity (fromJust entryKey) entry
  return entryKey

insertEntry_ :: Entry -> D.SqlPersistM ()
insertEntry_ entry = do
  entryKey <- D.insert entry
  insertSlug $ D.Entity entryKey entry

insertSlug :: D.Entity Entry -> D.SqlPersistM ()
insertSlug (D.Entity entryKey entry) = do
  slugText <- genEntrySlug slugLength (entryTitle entry)
  D.insert_ $ Slug entryKey slugText True

entryPandoc :: Entry -> P.Pandoc
entryPandoc = P.readMarkdown (P.def P.ReaderOptions) . T.unpack . entryContent

entryHtml :: Entry -> H.Html
entryHtml = P.writeHtml (P.def P.WriterOptions) . entryPandoc

entryLede :: Entry -> T.Text
entryLede = T.pack . P.writeMarkdown (P.def P.WriterOptions) . entryLedePandoc

entryLedeHtml :: Entry -> H.Html
entryLedeHtml = P.writeHtml (P.def P.WriterOptions) . entryLedePandoc

entryLedePandoc :: Entry -> P.Pandoc
entryLedePandoc entry = P.Pandoc m ledeBs
  where
    P.Pandoc m bs = entryPandoc entry
    ledeBs = take ledeMax $ takeWhile isNotHeader bs
    isNotHeader b = case b of
                      P.Header {} -> False
                      _ -> True



-- TODO: Find way to generalize and merge this with genTagSlug
genEntrySlug :: Int -> T.Text -> D.SqlPersistM T.Text
genEntrySlug w t = do
  let
    baseSlug = genSlug w t
  base <- D.getBy $ UniqueSlug baseSlug
  case base of
    Just _ -> do
      freshSlug <- firstM isFresh $
        map (T.append baseSlug . T.pack . show) ([-2,-3..] :: [Integer])
      return $ fromJust freshSlug
    Nothing ->
      return baseSlug
  where
    isFresh :: T.Text -> D.SqlPersistM Bool
    isFresh s = do
      found <- D.getBy $ UniqueSlug s
      return $ isNothing found

-- TODO: separate changeSlug function to be able to re-double back on old
-- names

postedFilter :: UTCTime -> [D.Filter Entry]
postedFilter now = [ EntryPostedAt D.<=. now ]

postedEntries :: [D.SelectOpt Entry] -> D.SqlPersistM [D.Entity Entry]
postedEntries = postedEntriesFilter []

postedEntriesFilter :: [D.Filter Entry] -> [D.SelectOpt Entry] -> D.SqlPersistM [D.Entity Entry]
postedEntriesFilter filters opts = do
  now <- liftIO getCurrentTime
  D.selectList (filters ++ postedFilter now) opts

postedEntryCount :: D.SqlPersistM Int
postedEntryCount = do
  now <- liftIO getCurrentTime
  D.count $ postedFilter now

getEntryData :: D.Entity Entry -> D.SqlPersistM (T.Text,[Tag])
getEntryData e = do
  p <- getUrlPath e
  ts  <- getTags e []
  return (p,ts)

wrapEntryData :: D.Entity Entry -> D.SqlPersistM (D.Entity Entry, (T.Text, [Tag]))
wrapEntryData e = do
  d <- getEntryData e
  return (e,d)

getCurrentSlug :: D.Entity Entry -> D.SqlPersistM (Maybe (D.Entity Slug))
getCurrentSlug entry = D.selectFirst [ SlugEntryId   D.==. eKey
                                     , SlugIsCurrent D.==. True ] []
  where
    D.Entity eKey _ = entry

getUrlPath :: D.Entity Entry -> D.SqlPersistM T.Text
getUrlPath entry = do
  slug <- getCurrentSlug entry
  case slug of
    Just (D.Entity _ slug') ->
      return $ T.append "/entry/" (slugSlug slug')
    Nothing               -> do
      let
        D.Entity eKey _ = entry
      return $ T.append "/entry/id/" (T.pack $ show eKey)

entryPermalink :: D.Entity Entry -> T.Text
entryPermalink (D.Entity eKey _) = T.append "/entry/id/" entryId
  where
    Right entryId = D.fromPersistValueText $ D.unKey eKey


getTags :: D.Entity Entry -> [D.Filter Tag] -> D.SqlPersistM [Tag]
getTags entry = getTagsByEntityKey (D.entityKey entry)

getTagsByEntityKey :: D.Key Entry -> [D.Filter Tag] -> D.SqlPersistM [Tag]
getTagsByEntityKey k filters = do
  ets <- D.selectList [ EntryTagEntryId D.==. k ] []
  let
    tagKeys = map (entryTagTagId . D.entityVal) ets
    selectTags tt = D.selectList
                      ([ TagId D.<-. tagKeys, TagType_ D.==. tt ] ++ filters)
                      [ D.Asc TagLabel ]
  tags <- concat <$> mapM selectTags [GeneralTag .. SeriesTag]
  return $ map D.entityVal tags


getPrevEntry :: Entry -> D.SqlPersistM (Maybe (D.Entity Entry))
getPrevEntry e = do
  now <- liftIO getCurrentTime
  D.selectFirst (postedFilter now ++ [ EntryPostedAt D.<. entryPostedAt e ])
    [ D.Desc EntryPostedAt ]

getNextEntry :: Entry -> D.SqlPersistM (Maybe (D.Entity Entry))
getNextEntry e = do
  now <- liftIO getCurrentTime
  D.selectFirst (postedFilter now ++ [ EntryPostedAt D.>. entryPostedAt e ])
    [ D.Asc EntryPostedAt ]


groupEntries :: [D.Entity Entry] -> [[[D.Entity Entry]]]
groupEntries entries = groupedMonthsYears
  where
    groupedMonthsYears = map (groupBy sameMonth) groupedYears
    groupedYears = groupBy sameYear entries
    sameYear e1 e2 = yearOf e1 == yearOf e2
    sameMonth e1 e2 = yearMonthOf e1 == yearMonthOf e2
    yearOf = yearOfDay . dayOf
    yearOfDay (y,_,_) = y
    yearMonthOf = yearMonthOfDay . dayOf
    yearMonthOfDay (y,m,_) = (y,m)
    dayOf = toGregorian . utctDay . entryPostedAt . D.entityVal

