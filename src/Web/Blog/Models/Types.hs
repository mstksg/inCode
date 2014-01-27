{-# LANGUAGE TemplateHaskell #-}

module Web.Blog.Models.Types where

import "base" Prelude
import Data.Aeson.TH
import Database.Persist.TH
import qualified Data.Text as T

data TagType = GeneralTag | CategoryTag | SeriesTag
  deriving (Show, Read, Eq, Ord, Enum)

derivePersistField "TagType"
deriveJSON defaultOptions ''TagType

tagTypePrefix :: TagType -> T.Text
tagTypePrefix GeneralTag = "#"
tagTypePrefix CategoryTag = "@"
tagTypePrefix SeriesTag = "+"
