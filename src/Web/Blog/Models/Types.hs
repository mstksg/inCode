{-# LANGUAGE TemplateHaskell #-}

module Web.Blog.Models.Types where

-- import Data.Aeson.Types
import Data.Aeson.TH
import Database.Persist.TH
import qualified Data.Text as T

data TagType = GeneralTag | CategoryTag | SeriesTag
  deriving (Show, Read, Eq, Ord, Enum)

derivePersistField "TagType"
deriveJSON id ''TagType

-- instance ToJSON TagType where
--     toJSON t = object [ "type" .= show t ]
-- instance FromJSON TagType where
--     parseJSON (Object v) = read $ v .: "type"



tagTypePrefix :: TagType -> T.Text
tagTypePrefix GeneralTag = "#"
tagTypePrefix CategoryTag = "@"
tagTypePrefix SeriesTag = "+"
