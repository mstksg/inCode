
module Web.Blog.Models.Util  where

import Control.Applicative                   ((<$>))
import Control.Monad
import Control.Monad.IO.Class                (liftIO)
import Control.Monad.Loops                   (firstM)
import Data.Char                             (isAlphaNum, toLower, toUpper)
import Data.List                             (groupBy)
import Data.Maybe                            (isNothing, fromJust, isJust)
import Data.Time
import Web.Blog.Models
import Web.Blog.Models.Types
import Web.Blog.Render
import Web.Blog.SiteData
import Web.Blog.Types
import qualified Data.Text                   as T
import qualified Database.Persist.Postgresql as D
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as I
import qualified Text.Pandoc                 as P

slugLength :: Int
slugLength = siteDataSlugLength siteData
ledeMax :: Int
ledeMax = siteDataLedeMax siteData


-- | Entries

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

genSlug :: Int -> T.Text -> T.Text
genSlug w = squash . T.dropAround isDash . T.map replaceSymbols . T.toCaseFold
  where
    isDash = (==) '-'
    replaceSymbols s =
      if isAlphaNum s
        then
          s
        else
          '-'
    squash = T.intercalate "-" . take w . filter (not . T.null) . T.split isDash


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
  ts  <- getTags e
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


getTags :: D.Entity Entry -> D.SqlPersistM [Tag]
getTags entry = getTagsByEntityKey $ D.entityKey entry

getTagsByEntityKey :: D.Key Entry -> D.SqlPersistM [Tag]
getTagsByEntityKey k = do 
  ets <- D.selectList [ EntryTagEntryId D.==. k ] []
  let
    tagKeys = map (entryTagTagId . D.entityVal) ets
    selectTags tt = D.selectList
                      [ TagId D.<-. tagKeys, TagType_ D.==. tt ]
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

-- | Tags

data PreTag = PreTag T.Text TagType (Maybe T.Text)

insertTag :: PreTag -> D.SqlPersistM (Maybe (D.Key Tag))
insertTag ptag = D.insertUnique $ fillTag ptag

insertTag_ :: PreTag -> D.SqlPersistM ()
insertTag_ ptag = D.insert_ $ fillTag ptag

fillTag :: PreTag -> Tag
fillTag ptag = tag
  where
    PreTag l t d = ptag
    slug = genSlug (maxBound :: Int) l
    tag = case t of
      GeneralTag  -> Tag (T.map toLower l) t d slug
      CategoryTag -> Tag (T.map toUpper l) t d slug
      SeriesTag   -> Tag l t d slug

tagLabel' :: Tag -> T.Text
tagLabel' t = T.append (tagTypePrefix $ tagType_ t) $ tagLabel t

tagPath :: Tag -> T.Text
tagPath t = T.append prefix $ tagSlug t
  where
    prefix = T.append "/entries/"
      (case tagType_ t of
        GeneralTag  -> "tagged/"
        CategoryTag -> "category/@"
        SeriesTag   -> "series/+")

tagLi :: Tag -> H.Html
tagLi t = H.li $
  H.a H.! A.href (I.textValue $ renderUrl' $ tagPath t) $
    H.toHtml $ tagLabel' t

isGeneralTag :: Tag -> Bool
isGeneralTag t = case tagType_ t of
  GeneralTag -> True
  _          -> False

isCategoryTag :: Tag -> Bool
isCategoryTag t = case tagType_ t of
  CategoryTag -> True
  _           -> False

isSeriesTag :: Tag -> Bool
isSeriesTag t = case tagType_ t of
  SeriesTag -> True
  _         -> False

getTagInfoList :: TagType -> D.SqlPersistM [(Tag,(T.Text,Int))]
getTagInfoList tt = do
  allTags <- D.selectList [ TagType_ D.==. tt ] [ D.Asc TagLabel ]
  let
    tagInfo (D.Entity tKey t) = do
      c <- D.count [ EntryTagTagId D.==. tKey ]
      return (t,(tagPath t,c))

  tagInfos <- mapM tagInfo allTags

  return $ filter ((> 0) . snd . snd) tagInfos

