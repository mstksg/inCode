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

getTags :: Entity Entry -> SqlPersistM [Tag]
getTags entry = do
  let
    Entity eKey _ = entry
  ets <- selectList [ EntryTagEntryId   ==. eKey ] []
  let
    tagKeys = map (entryTagTagId . entityVal) ets
  mapM getJust tagKeys

-- getEntryBySlug :: Text
