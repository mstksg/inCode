{-# LANGUAGE FlexibleContexts             #-} 
{-# LANGUAGE GADTs                        #-} 
{-# LANGUAGE QuasiQuotes                  #-} 
{-# LANGUAGE TemplateHaskell              #-} 
{-# LANGUAGE TypeFamilies                 #-} 
{-# LANGUAGE TypeSynonymInstances         #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving   #-} 
{-# LANGUAGE EmptyDataDecls               #-} 

module Web.Blog.Models () where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time
import Data.Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Entry
    title       String
    content     Text
    createdAt   UTCTime
    postTime    UTCTime
    deriving    Show

Tag
    label String

EntryTag
    entryId          EntryId
    tagId            TagId
    UniqueEntryTag   entryId   tagId
|]
