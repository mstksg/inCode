{-# LANGUAGE FlexibleContexts             #-} 
{-# LANGUAGE GADTs                        #-} 
{-# LANGUAGE QuasiQuotes                  #-} 
{-# LANGUAGE TemplateHaskell              #-} 
{-# LANGUAGE TypeFamilies                 #-} 
{-# LANGUAGE TypeSynonymInstances         #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving   #-} 
{-# LANGUAGE EmptyDataDecls               #-} 

module Web.Blog.Models  where

import Data.Time
import Database.Persist.TH
import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Entry
    title       T.Text
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


