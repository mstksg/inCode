{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE TemplateHaskell              #-}
{-# LANGUAGE QuasiQuotes                  #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE TypeSynonymInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# LANGUAGE EmptyDataDecls               #-}

module Web.Blog.Models.Models  where

-- import Database.Persist.Quasi
import "base" Prelude
import Data.Time
import Database.Persist.TH
import Web.Blog.Models.Persist
import Web.Blog.Models.Types
import qualified Data.Text       as T

-- how can i make this strict?
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Entry json
    title       T.Text
    content     T.Text
    image       FilePath Maybe
    sourceFile  FilePath Maybe
    createdAt   UTCTime Maybe
    postedAt    UTCTime Maybe
    modifiedAt  UTCTime Maybe
    identifier  T.Text Maybe

    UniqueEntryTitle title
    deriving Show

Tag json
    label           T.Text
    type_           TagType
    description     T.Text Maybe
    slug            T.Text

    UniqueLabelType label type_
    UniqueSlugType  slug  type_
    deriving        Eq Show Read

EntryTag json
    entryId          EntryId
    tagId            TagId

    UniqueEntryTag   entryId   tagId
    deriving         Show

Slug json
    entryId    EntryId Eq
    slug       T.Text
    isCurrent  Bool

    UniqueSlug slug
    deriving Show

RemovedEntry json
    title       T.Text
    content     T.Text
    createdAt   UTCTime Maybe
    postedAt    UTCTime Maybe
    modifiedAt  UTCTime Maybe
    removedAt   UTCTime
    identifier  T.Text Maybe
    tagList     T.Text
    slugList    T.Text
|]
