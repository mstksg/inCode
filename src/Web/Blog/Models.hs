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

-- import Database.Persist
-- import Database.Persist.Sqlite
-- import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Time
import qualified Data.Text as T
import Data.Char

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Entry
    title       T.Text
    description T.Text
    slug        T.Text
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

|]

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

buildEntry :: T.Text -> T.Text -> T.Text -> UTCTime -> UTCTime -> Entry
buildEntry t d = Entry t d s 
  where
    s = genSlug 5 t

