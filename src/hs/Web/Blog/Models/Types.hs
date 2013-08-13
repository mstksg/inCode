{-# LANGUAGE TemplateHaskell #-}

module Web.Blog.Models.Types where

import Database.Persist.TH
import qualified Data.Text as T (Text)

data TagType = GeneralTag | CategoryTag | SeriesTag
  deriving (Show, Read, Eq, Ord, Enum)

derivePersistField "TagType"


tagTypePrefix :: TagType -> T.Text
tagTypePrefix GeneralTag = "#"
tagTypePrefix CategoryTag = "@"
tagTypePrefix SeriesTag = "+"
