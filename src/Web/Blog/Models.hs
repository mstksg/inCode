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
import Web.Blog.Models.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Entry
    title       T.Text
    content     T.Text
    createdAt   UTCTime
    postedAt    UTCTime

    UniqueEntryTitle title
    deriving    Show

Tag
    label           T.Text
    type_           TagType
    description     T.Text Maybe
    slug            T.Text

    UniqueLabelType label type_
    UniqueSlugType  slug  type_
    deriving        Show

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

