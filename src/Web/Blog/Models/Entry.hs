{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Models.Entry  where

-- import Control.Monad
-- import qualified Database.Esqueleto       as E
import Control.Applicative                   ((<$>))
import Control.Monad.IO.Class                (liftIO)
import Control.Monad.Loops                   (firstM)
import Data.List                             (groupBy)
import Data.Maybe                            (fromJust, isJust)
import Data.Time
import Web.Blog.Models
import Web.Blog.Models.Slug
import Web.Blog.Models.Types
import Config.SiteData
import Web.Blog.Types
import Web.Blog.Util
import qualified Data.Aeson                  as A
import qualified Data.Aeson.Types            as AT
import qualified Data.Foldable as Fo         (forM_)
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import qualified Database.Persist.Postgresql as D
import qualified Text.Blaze.Html5            as H
import qualified Text.Pandoc                 as P

slugLength :: Int
slugLength = appPrefsSlugLength $ siteDataAppPrefs siteData
ledeMax :: Int
ledeMax = appPrefsLedeMax $ siteDataAppPrefs siteData

insertEntry :: Entry -> D.SqlPersistM (Maybe (D.Key Entry))
insertEntry entry = do
  entryKey <- D.insertUnique entry
  Fo.forM_ entryKey $ \k ->
    insertSlug k $ entryTitle entry
  return entryKey

insertEntry_ :: Entry -> D.SqlPersistM ()
insertEntry_ entry = do
  entryKey <- D.insert entry
  insertSlug entryKey $ entryTitle entry

updateEntryTitle :: D.Key Entry -> T.Text -> D.SqlPersistM ()
updateEntryTitle eKey newTitle = do
    D.update eKey [EntryTitle D.=. newTitle]
    D.updateWhere [SlugEntryId D.==. eKey] [SlugIsCurrent D.=. False]
    insertSlug eKey newTitle

insertSlug :: D.Key Entry -> T.Text -> D.SqlPersistM ()
insertSlug eKey title = do
  slugText <- genEntrySlug slugLength eKey title
  oldSlug <- D.getBy $ UniqueSlug slugText
  case oldSlug of
    Just (D.Entity sKey _) -> D.update sKey [SlugIsCurrent D.=. True]
    Nothing -> D.insert_ $ Slug eKey slugText True


entryPandoc :: Entry -> P.Pandoc
entryPandoc = P.bottomUp stripRules . entryPandocRaw
  where
    stripRules P.HorizontalRule = P.Null
    stripRules other = other

entryPandocRaw :: Entry -> P.Pandoc
entryPandocRaw =
    P.readMarkdown (P.def P.ReaderOptions) . T.unpack . entryContent

entryHtml :: Entry -> H.Html
entryHtml = P.writeHtml (P.def P.WriterOptions) . entryPandoc

entryLede :: Entry -> T.Text
entryLede = T.pack . P.writeMarkdown (P.def P.WriterOptions) . entryLedePandoc

entryLedeHtml :: Entry -> H.Html
entryLedeHtml = P.writeHtml (P.def P.WriterOptions) . entryLedePandoc

entryLedePandoc :: Entry -> P.Pandoc
entryLedePandoc entry = P.Pandoc m ledeBs
  where
    P.Pandoc m bs = entryPandocRaw entry
    ledeBs = take ledeMax $ takeWhile validLede bs
    validLede b = case b of
                    P.Header {} -> False
                    P.HorizontalRule -> False
                    _ -> True



genEntrySlug :: Int -> D.Key Entry -> T.Text -> D.SqlPersistM T.Text
genEntrySlug w k t = do
  freshSlug <- firstM isFresh $
    map (flip (genSlugSuffix w) t) [0..]
  return $ fromJust freshSlug
  where
    isFresh :: T.Text -> D.SqlPersistM Bool
    isFresh s = do
      found <- D.selectList [ SlugSlug D.==. s, SlugEntryId D.!=. k ] []
      return $ null found



postedFilter :: UTCTime -> [D.Filter Entry]
postedFilter now = [ EntryPostedAt D.<=. Just now ]

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
getCurrentSlug entry = do
    current <- D.selectFirst [ SlugEntryId   D.==. eKey
                             , SlugIsCurrent D.==. True ]
                             [ D.Desc SlugId ]
    case current of
      Just _ -> return current
      Nothing -> D.selectFirst [ SlugEntryId D.==. eKey ]
                               [ D.Desc SlugId ]
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
    hasPostDate = filter (isJust . entryPostedAt . D.entityVal) entries
    groupedMonthsYears = map (groupBy sameMonth) groupedYears
    groupedYears = groupBy sameYear hasPostDate
    sameYear e1 e2 = yearOf e1 == yearOf e2
    sameMonth e1 e2 = yearMonthOf e1 == yearMonthOf e2
    yearOf = yearOfDay . dayOf
    yearOfDay (y,_,_) = y
    yearMonthOf = yearMonthOfDay . dayOf
    yearMonthOfDay (y,m,_) = (y,m)
    dayOf = toGregorian . utctDay . fromJust . entryPostedAt . D.entityVal

-- Maybe have issue where a slug will one day point to a completely
-- different entry.  Oh well.
removeEntry :: D.Entity Entry -> D.SqlPersistM (D.Key RemovedEntry)
removeEntry eEnt@(D.Entity eKey e) = do
  now <- liftIO getCurrentTime
  tagList <- T.pack . unlines . map show <$> getTags eEnt []
  slugList <- T.unlines . map (slugPrettyLabel . D.entityVal) <$>
    D.selectList [ SlugEntryId D.==. eKey ] [ D.Asc SlugId ]
  let
    removed = RemovedEntry
                (entryTitle e)
                (entryContent e)
                (entryCreatedAt e)
                (entryPostedAt e)
                (entryModifiedAt e)
                now
                (entryIdentifier e)
                tagList
                slugList
  D.deleteWhere [ SlugEntryId D.==. eKey ]
  D.deleteWhere [ EntryTagEntryId D.==. eKey ]
  D.delete eKey
  D.insert removed

genJSON :: D.Entity Entry -> D.SqlPersistM AT.Value
genJSON eEnt@(D.Entity eKey e) = do

  tagsJson <- A.toJSON <$> getTags eEnt []
  slusJson <- A.toJSON <$>
    D.selectList [ SlugEntryId D.==. eKey ] [ D.Asc SlugEntryId ]

  let
    eJson = A.toJSON e

  return $ AT.object
    [ "entryVal" AT..= eJson
    , "entryTags" AT..= tagsJson
    , "entrySlugs" AT..= slusJson
    ]

entryJSONs :: D.SqlPersistM AT.Value
entryJSONs = do
  es <- D.selectList [] [ D.Asc EntryId ]
  vs <- mapM genJSON es
  return $ AT.Array $ V.fromList vs

