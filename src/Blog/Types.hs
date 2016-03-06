{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Blog.Types where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Binary.Orphans  ()
import           Data.Char
import           Data.Default
import           Data.Time.LocalTime
import           Data.Typeable
import           GHC.Generics
import qualified Data.Aeson.Types     as A
import qualified Data.Binary          as B
import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified Text.Blaze.Html5     as H


data Config = Config
    { confTitle         :: !T.Text
    , confDesc          :: !T.Text
    , confAuthorInfo    :: !AuthorInfo
    , confCopyright     :: !T.Text
    , confFeed          :: !T.Text
    , confBlobs         :: !(Maybe T.Text)
    , confCodeSamples   :: !(Maybe T.Text)
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
      confFeed         <- v .: "feed"
      confBlobs        <- v .:? "public-blobs"
      confCodeSamples  <- v .:? "code-samples"
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
    , hostRoot :: Maybe T.Text
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
    { prefSlugLength     :: Int
    , prefHomeEntries    :: Int
    , prefLedeMax        :: Int
    , prefFeedEntries    :: Int
    , prefSidebarEntries :: Int
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
    , entryLede       :: !T.Text
    , entrySourceFile :: !FilePath
    , entryCreateTime :: !(Maybe LocalTime)
    , entryPostTime   :: !(Maybe LocalTime)
    , entryModifyTime :: !(Maybe LocalTime)
    , entryIdentifier :: !(Maybe T.Text)
    , entrySlug       :: !(Maybe T.Text)
    , entryOldSlugs   :: ![T.Text]
    , entryId         :: !(Maybe Int)
    , entryCanonical  :: !FilePath
    , entryTags       :: ![(TagType, T.Text)]
    }
  deriving (Show, Generic, Typeable, Eq)
-- TODO: entry image

instance B.Binary Entry

data Tag = Tag
    { tagLabel       :: !T.Text
    , tagType        :: !TagType
    , tagDescription :: !(Maybe T.Text)
    , tagEntries     :: ![Entry]
    }
  deriving (Show, Generic, Typeable)

instance B.Binary Tag

data TaggedEntry = TE
    { teEntry :: Entry
    , teTags  :: [Tag]
    }
  deriving (Show, Generic, Typeable)

instance B.Binary TaggedEntry

data PageData = PD
    { pageDataTitle     :: !(Maybe T.Text)
    , pageDataDesc      :: !(Maybe T.Text)
    , pageDataImage     :: !(Maybe FilePath)
    , pageDataType      :: !(Maybe T.Text)
    , pageDataCanonical :: !(Maybe FilePath)
    , pageDataCss       :: ![T.Text]
    , pageDataJs        :: ![T.Text]
    , pageDataHeaders   :: ![H.Html]
    }

instance Default PageData where
    def = PD { pageDataTitle     = Nothing
             , pageDataDesc      = Nothing
             , pageDataImage     = Nothing
             , pageDataType      = Nothing
             , pageDataCanonical = Nothing
             , pageDataCss       = []
             , pageDataJs        = []
             , pageDataHeaders   = []
             }

type Year = Integer
data Month = JAN | FEB | MAR | APR | MAY | JUN
           | JUL | AUG | SEP | OCT | NOV | DEC
  deriving (Show, Eq, Ord, Enum)

mInt :: Month -> Int
mInt = succ . fromEnum

showMonth :: Month -> String
showMonth = \case
              JAN -> "January"
              FEB -> "February"
              MAR -> "March"
              APR -> "April"
              MAY -> "May"
              JUN -> "June"
              JUL -> "July"
              AUG -> "August"
              SEP -> "September"
              OCT -> "October"
              NOV -> "November"
              DEC -> "December"

data ArchiveData a = ADAll               (M.Map Year (M.Map Month [a]))
                   | ADYear   Year       (M.Map Month [a])
                   | ADMonth  Year Month [a]
                   | ADTagged Tag        [a]
  deriving (Show, Foldable, Traversable, Functor)

