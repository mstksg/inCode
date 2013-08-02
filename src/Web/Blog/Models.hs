{-# LANGUAGE FlexibleContexts             #-} 
{-# LANGUAGE GADTs                        #-} 
{-# LANGUAGE QuasiQuotes                  #-} 
{-# LANGUAGE TemplateHaskell              #-} 
{-# LANGUAGE TypeFamilies                 #-} 
{-# LANGUAGE TypeSynonymInstances         #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving   #-} 
{-# LANGUAGE EmptyDataDecls               #-} 

-- module Web.Blog.Models (Entry, Tag, migrateAll) where
module Web.Blog.Models  where

import Control.Monad.Loops
import Data.Char
import Data.Maybe (isNothing, fromJust)
import Data.Time
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Applicative ((<$>))
import System.Locale

slugLength :: Int
slugLength = 10

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Entry
    title       T.Text
    description T.Text
    content     T.Text
    createdAt   UTCTime
    postedAt    UTCTime
    deriving    Show

Tag
    label       T.Text
    UniqueLabel label
    deriving    Show

EntryTag
    entryId          EntryId
    tagId            TagId
    UniqueEntryTag   entryId   tagId
    deriving         Show

Slug
    entryId    EntryId Eq
    slug       T.Text
    isCurrent  Bool
    UniqueSlug slug
    deriving Show

|]

insertEntry :: Entry -> SqlPersistM (Key Entry)
insertEntry entry = do
  slugText <- genSlug slugLength (entryTitle entry)
  entryKey <- insert entry
  insert_ $ Slug entryKey slugText True
  return entryKey

genSlug' :: Int -> T.Text -> T.Text
genSlug' w = squash . T.dropAround isDash . T.map replaceSymbols . T.toCaseFold
  where
    isDash = (==) '-'
    replaceSymbols s =
      if isAlphaNum s
        then
          s
        else
          '-'
    squash = T.intercalate "-" . take w . filter (not . T.null) . T.split isDash

-- TODO: Maybe include date in slug?
genSlug :: Int -> T.Text -> SqlPersistM T.Text
genSlug w t = do
  let
    baseSlug = genSlug' w t
  base <- getBy $ UniqueSlug baseSlug
  case base of
    Just _ -> do
      freshSlug <- firstM isFresh $
        map (T.append baseSlug . T.pack . show) ([-2,-3..] :: [Integer])
      return $ fromJust freshSlug
    Nothing ->
      return baseSlug
  where
    isFresh :: T.Text -> SqlPersistM Bool
    isFresh s = do
      found <- getBy $ UniqueSlug s
      return $ isNothing found

-- TODO: separate changeSlug function to be able to re-double back on old
-- names

postedEntries :: [SelectOpt Entry] -> SqlPersistM [Entity Entry]
postedEntries opts = do
  now <- liftIO getCurrentTime
  selectList [ EntryPostedAt <=. now ] opts

getCurrentSlug :: Entity Entry -> SqlPersistM (Maybe (Entity Slug))
getCurrentSlug entry = selectFirst [ SlugEntryId   ==. eKey
                                   , SlugIsCurrent ==. True ] []
  where
    Entity eKey _ = entry

getUrlPath :: Entity Entry -> SqlPersistM T.Text
getUrlPath entry = do
  slug <- getCurrentSlug entry
  case slug of
    Just (Entity _ slug') ->
      return $ T.append "/entry/" (slugSlug slug')
    Nothing               -> do
      let
        Entity eKey _ = entry
      return $ T.append "/entry/id/" (T.pack $ show eKey)

getTags :: Entity Entry -> SqlPersistM [Tag]
getTags entry = getTagsByEntityKey $ entityKey entry

getTagsByEntityKey :: Key Entry -> SqlPersistM [Tag]
getTagsByEntityKey k = do 
  ets <- selectList [ EntryTagEntryId   ==. k ] []
  let
    tagKeys = map (entryTagTagId . entityVal) ets
  mapM getJust tagKeys

-- getEntryBySlug :: Text

renderFriendlyTime :: UTCTime -> String
renderFriendlyTime = formatTime defaultTimeLocale "%A %B %-e, %Y"

renderDatetimeTime :: UTCTime -> String
renderDatetimeTime = formatTime defaultTimeLocale "%FT%XZ"

getPrevEntry :: Entry -> SqlPersistM (Maybe (Entity Entry))
getPrevEntry e = selectFirst [ EntryPostedAt <. entryPostedAt e ] [ Desc EntryPostedAt ]

getNextEntry :: Entry -> SqlPersistM (Maybe (Entity Entry))
getNextEntry e = selectFirst [ EntryPostedAt >. entryPostedAt e ] [ Asc EntryPostedAt ]
