{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}

module Blog.Types where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Binary.Orphans ()
import           Data.Char
import           Data.Time.LocalTime
import           Data.Typeable
import           GHC.Generics
import           Hakyll
import qualified Data.Aeson.Types    as A
import qualified Data.Binary         as B
import qualified Data.Text           as T


data Config = Config
    { confTitle         :: !T.Text
    , confDesc          :: !T.Text
    , confAuthorInfo    :: !AuthorInfo
    , confCopyright     :: !T.Text
    , confBlobs         :: !(Maybe T.Text)
    , confInteractive   :: !(Maybe T.Text)
    , confHostInfo      :: !HostInfo
    , confDeveloperAPIs :: !DeveloperAPIs
    , confBlogPrefs     :: !BlogPrefs
    , confEnvType       :: !EnvType
    }
  deriving (Show)

instance FromJSON Config where
    parseJSON (Object v) = do
      confTitle        <- v .: "title"
      confDesc         <- v .: "description"
      confAuthorInfo   <- v .: "author"
      confCopyright    <- v .: "copyright"
      confBlobs        <- v .:? "public-blobs"
      confInteractive  <- v .:? "interactive-url"
      confHostInfo     <- v .: "host"
      confDeveloperAPIs <- v .: "developer-apis"
      confBlogPrefs     <- v .: "preferences"
      confEnvType       <- v .: "development"
      return Config{..}
    parseJSON _ = mzero

data EnvType = ETDevelopment | ETProduction
  deriving (Show, Eq, Ord, Enum)

data AuthorInfo = AuthorInfo
    { authorName     :: T.Text
    , authorEmail    :: T.Text
    , authorRel      :: T.Text
    , authorFacebook :: T.Text
    , authorTwitter  :: T.Text
    , authorGPlus    :: T.Text
    , authorGithub   :: T.Text
    , authorLinkedIn :: T.Text
    }
  deriving (Show, Generic)

data HostInfo = HostInfo
    { hostBase :: T.Text
    , hostPort :: Maybe Int
    }
  deriving (Show, Generic)

data DeveloperAPIs = DeveloperAPIs
    { devAnalytics  :: (T.Text, T.Text)
    , devDisqus     :: T.Text
    , devFacebook   :: T.Text
    , devAddThis    :: T.Text
    , devFeedburner :: T.Text
    }
  deriving (Show, Generic)

data BlogPrefs = BlogPrefs
    { prefSlugLength  :: Int
    , prefHomeEntries :: Int
    , prefLedeMax     :: Int
    , prefFeedEntries :: Int
    }
  deriving (Show, Generic)

instance FromJSON DeveloperAPIs where
  parseJSON (Object v) = do
    anObj         <- v .: "analytics"
    devAnalytics  <- liftA2 (,) (anObj .: "id")
                                (anObj .: "host")
    devDisqus     <- v .: "disqus"
    devFacebook   <- v .: "facebook"
    devAddThis    <- v .: "add-this"
    devFeedburner <- v .: "feedburner"
    return DeveloperAPIs{..}
  parseJSON _ = mzero
instance ToJSON DeveloperAPIs where
  toJSON DeveloperAPIs{..} =
      object [ "analytics"  .= object [ "id"   .= fst devAnalytics
                                      , "host" .= snd devAnalytics
                                      ]
             , "disqus"     .= devDisqus
             , "facebook"   .= devFacebook
             , "add-this"   .= devAddThis
             , "feedburner" .= devFeedburner
             ]

instance FromJSON EnvType where
  parseJSON j = case j of
                  Bool False -> return ETProduction
                  _          -> return ETDevelopment
instance ToJSON EnvType where
    toJSON ETDevelopment = Bool True
    toJSON ETProduction  = Bool False

instance FromJSON AuthorInfo where
  parseJSON = A.genericParseJSON $ A.defaultOptions
                { A.fieldLabelModifier = map toLower . drop 6 }
instance ToJSON AuthorInfo where
  toJSON = A.genericToJSON $ A.defaultOptions
             { A.fieldLabelModifier = map toLower . drop 6 }

instance FromJSON HostInfo where
  parseJSON = A.genericParseJSON $ A.defaultOptions
                { A.fieldLabelModifier = A.camelTo '-' . drop 4 }
instance ToJSON HostInfo where
  toJSON = A.genericToJSON $ A.defaultOptions
             { A.fieldLabelModifier = A.camelTo '-' . drop 4 }

instance FromJSON BlogPrefs where
  parseJSON = A.genericParseJSON $ A.defaultOptions
                { A.fieldLabelModifier = A.camelTo '-' . drop 4 }
instance ToJSON BlogPrefs where
  toJSON = A.genericToJSON $ A.defaultOptions
             { A.fieldLabelModifier = A.camelTo '-' . drop 4 }


data TagType = GeneralTag | CategoryTag | SeriesTag
  deriving (Show, Read, Eq, Ord, Enum, Generic, Typeable)

instance B.Binary TagType

data Entry = Entry
    { entryTitle      :: !T.Text
    , entryContents   :: !T.Text
    , entryHTML       :: !T.Text
    , entryLede       :: !T.Text
    , entrySourceFile :: !FilePath
    , entryCreateTime :: !(Maybe LocalTime)
    , entryPostTime   :: !(Maybe LocalTime)
    , entryModifyTime :: !(Maybe LocalTime)
    , entryIdentifier :: !(Maybe T.Text)
    , entrySlug       :: !(Maybe T.Text)
    , entryOldSlugs   :: ![T.Text]
    , entryId         :: !(Maybe Int)
    , entryCanonical  :: !Identifier
    , entryTags       :: ![(TagType, T.Text)]
    }
  deriving (Show, Generic, Typeable)

instance B.Binary Entry

data Tag = Tag
    { tagLabel       :: !T.Text
    , tagType        :: !TagType
    , tagDescription :: !(Maybe T.Text)
    , tagSlug        :: !T.Text
    , tagEntries     :: [Entry]
    }
  deriving (Show, Generic, Typeable)

instance B.Binary Tag

tagTypePrefix :: TagType -> T.Text
tagTypePrefix GeneralTag  = "#"
tagTypePrefix CategoryTag = "@"
tagTypePrefix SeriesTag   = "+"
