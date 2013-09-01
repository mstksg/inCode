{-# LANGUAGE FlexibleContexts             #-} 
{-# LANGUAGE GADTs                        #-} 
{-# LANGUAGE QuasiQuotes                  #-} 
{-# LANGUAGE TemplateHaskell              #-} 
{-# LANGUAGE TypeFamilies                 #-} 
{-# LANGUAGE TypeSynonymInstances         #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving   #-} 
{-# LANGUAGE EmptyDataDecls               #-} 
{-# LANGUAGE FlexibleInstances            #-}

module Web.Blog.Models  where

import Data.Time
import Database.Persist.TH
import Web.Blog.Models.Types
-- import Data.Maybe (maybe)
import qualified Data.Text   as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Entry
    title       T.Text
    content     T.Text
    createdAt   UTCTime Maybe
    postedAt    UTCTime Maybe
    modifiedAt  UTCTime Maybe
    identifier  T.Text Maybe

    UniqueEntryTitle title

Tag
    label           T.Text
    type_           TagType
    description     T.Text Maybe
    slug            T.Text

    UniqueLabelType label type_
    UniqueSlugType  slug  type_
    deriving        Eq Show Read

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

instance Show Entry where
  show (Entry t _ cA pA _ i) = concat
    [ show t
    , " ("
    , maybe "" ((++ ", ") . show) i
    , maybe "" ((++ ", ") . show) cA
    , maybe "no post date" (("posted " ++) . show) pA
    , ")"
    ]
