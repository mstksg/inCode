{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Blog.Types where

import Control.Lens
import qualified Data.Binary as B
import Data.Binary.Instances ()
import Data.Binary.Orphans ()
import Data.Char
import Data.Default
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time.LocalTime
import Data.Typeable
import Dhall
import Hakyll.Core.Writable
import qualified Skylighting.Core as Sky
import qualified Text.Blaze.Html5 as H
import qualified Text.DocTemplates as DT
import qualified Text.DocTemplates.Internal as DT
import qualified Text.Pandoc.Definition as P

data Config = Config
  { confTitle :: !T.Text,
    confDesc :: !T.Text,
    confAuthorInfo :: !AuthorInfo,
    -- | ie: America/Los_Angeles
    confEntryTZ :: !T.Text,
    confCopyright :: !T.Text,
    confLicense :: !T.Text,
    confLicenseLink :: !T.Text,
    confFeed :: !T.Text,
    confBlobs :: !(Maybe Blobs),
    confCodeSamples :: !(Maybe T.Text),
    confInteractive :: !(Maybe T.Text),
    confHostInfo :: !HostInfo,
    confDeveloperAPIs :: !DeveloperAPIs,
    confBlogPrefs :: !BlogPrefs,
    confEnvType :: !EnvType
  }
  deriving (Show, Eq, Generic)

instance FromDhall Config where
  autoWith = genericAutoWithInputNormalizer basicInterpretOptions

interpretConfig :: Decoder Config
interpretConfig = genericAutoWith basicInterpretOptions

data PatronLevel
  = PLInactive
  | PLSupport
  | PLAmazing
  deriving (Show, Eq, Ord, Generic)

instance FromDhall PatronLevel where
  autoWith = genericAutoWithInputNormalizer basicInterpretOptions

data PatronInfo = PatronInfo
  { patronTwitter :: !(Maybe T.Text),
    patronLevel :: !PatronLevel
  }
  deriving (Show, Eq, Ord, Generic)

instance FromDhall PatronInfo where
  autoWith = genericAutoWithInputNormalizer basicInterpretOptions

type PatronList = M.Map T.Text PatronInfo

interpretPatronList :: Decoder PatronList
interpretPatronList =
  fmap M.fromList . list . record $
    (,)
      <$> field "name" strictText
      <*> field "info" (genericAutoWith basicInterpretOptions)

data EnvType = ETDevelopment | ETProduction
  deriving (Show, Eq, Ord, Enum, Generic)

instance FromDhall EnvType where
  autoWith = genericAutoWithInputNormalizer basicInterpretOptions

data AuthorInfo = AuthorInfo
  { authorName :: T.Text,
    authorEmail :: T.Text,
    authorRel :: T.Text,
    authorTwitter :: T.Text,
    authorGPlus :: T.Text,
    authorGithub :: T.Text,
    authorLinkedIn :: T.Text,
    authorKeybase :: T.Text,
    authorCoinbase :: T.Text,
    authorBitcoin :: T.Text,
    authorPatreon :: T.Text,
    authorTwitch :: T.Text
  }
  deriving (Show, Eq, Generic)

data HostInfo = HostInfo
  { hostSecure :: !Bool,
    hostBase :: T.Text,
    hostPort :: Maybe Natural,
    hostRoot :: Maybe T.Text
  }
  deriving (Show, Eq, Generic)

data DeveloperAPIs = DeveloperAPIs
  { devAnalytics :: (T.Text, T.Text),
    devDisqus :: T.Text,
    devFacebook :: T.Text,
    devAddThis :: T.Text,
    devFeedburner :: T.Text,
    devFlattr :: T.Text
  }
  deriving (Show, Eq, Generic)

data Blobs = Blobs
  { blobsTree :: !T.Text,
    blobsSourceBranch :: !(Maybe T.Text),
    blobsRenderBranch :: !(Maybe T.Text)
  }
  deriving (Show, Eq, Generic)

data BlogPrefs = BlogPrefs
  { prefSlugLength :: Natural,
    prefHomeEntries :: Natural,
    prefLedeMax :: Natural,
    prefFeedEntries :: Natural,
    prefSidebarEntries :: Natural
  }
  deriving (Show, Eq, Generic)

instance FromDhall DeveloperAPIs where
  autoWith = genericAutoWithInputNormalizer basicInterpretOptions

-- instance FromJSON PatronLevel where
--   parseJSON = A.genericParseJSON $ A.defaultOptions
--                 { A.allNullaryToStringTag  = True
--                 , A.constructorTagModifier = A.camelTo2 '-' . drop 2
--                 }
-- instance ToJSON PatronLevel where
--   toJSON = A.genericToJSON $ A.defaultOptions
--              { A.allNullaryToStringTag  = True
--              , A.constructorTagModifier = A.camelTo2 '-' . drop 2
--              }

-- instance FromJSON PatronInfo where
--   parseJSON = A.genericParseJSON $ A.defaultOptions
--                 { A.fieldLabelModifier = A.camelTo2 '-' . drop 6 }
-- instance ToJSON PatronInfo where
--   toJSON = A.genericToJSON $ A.defaultOptions
--              { A.fieldLabelModifier = A.camelTo2 '-' . drop 6 }

instance FromDhall AuthorInfo where
  autoWith = genericAutoWithInputNormalizer basicInterpretOptions

instance FromDhall HostInfo where
  autoWith = genericAutoWithInputNormalizer basicInterpretOptions

instance FromDhall Blobs where
  autoWith = genericAutoWithInputNormalizer basicInterpretOptions

instance FromDhall BlogPrefs where
  autoWith = genericAutoWithInputNormalizer basicInterpretOptions

data TagType = GeneralTag | CategoryTag | SeriesTag
  deriving (Show, Read, Eq, Ord, Enum, Generic, Typeable)

instance B.Binary TagType

data Entry = Entry
  { entryTitle :: !T.Text,
    entryContents :: !P.Pandoc,
    entryLede :: !P.Pandoc,
    entrySourceFile :: !FilePath,
    entryCreateTime :: !(Maybe LocalTime),
    entryPostTime :: !(Maybe LocalTime),
    entryModifyTime :: !(Maybe LocalTime),
    entryIdentifier :: !(Maybe T.Text),
    entrySlug :: !(Maybe T.Text),
    entryOldSlugs :: ![T.Text],
    entryId :: !(Maybe Int),
    entryCanonical :: !FilePath,
    entryTags :: ![(TagType, T.Text)],
    entryJS :: ![T.Text],
    entryCSS :: ![T.Text],
    entryNoSignoff :: !Bool
  }
  deriving (Show, Generic, Typeable, Eq)

-- TODO: entry image

instance B.Binary Entry

data Tag = Tag
  { tagLabel :: !T.Text,
    tagType :: !TagType,
    tagDescription :: !(Maybe T.Text),
    tagEntries :: ![Entry]
  }
  deriving (Show, Generic, Typeable)

instance B.Binary Tag

data TaggedEntry = TE
  { teEntry :: Entry,
    teTags :: [Tag]
  }
  deriving (Show, Generic, Typeable)

instance B.Binary TaggedEntry

data PageData = PD
  { pageDataTitle :: !(Maybe T.Text),
    pageDataDesc :: !(Maybe T.Text),
    pageDataImage :: !(Maybe FilePath),
    pageDataType :: !(Maybe T.Text),
    pageDataCanonical :: !(Maybe FilePath),
    pageDataCss :: ![T.Text],
    pageDataJs :: ![T.Text],
    pageDataHeaders :: ![H.Html]
  }

instance Default PageData where
  def =
    PD
      { pageDataTitle = Nothing,
        pageDataDesc = Nothing,
        pageDataImage = Nothing,
        pageDataType = Nothing,
        pageDataCanonical = Nothing,
        pageDataCss = [],
        pageDataJs = [],
        pageDataHeaders = []
      }

type Year = Integer

data Month
  = JAN
  | FEB
  | MAR
  | APR
  | MAY
  | JUN
  | JUL
  | AUG
  | SEP
  | OCT
  | NOV
  | DEC
  deriving (Show, Eq, Ord, Enum)

mInt :: Month -> Int
mInt = (+ 1) . fromEnum

-- if this is succ . fromEnum, there is ghc bug in 8.4 for some reason heh

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

data ArchiveData a
  = ADAll (M.Map Year (M.Map Month (M.Map LocalTime [a])))
  | ADYear Year (M.Map Month (M.Map LocalTime [a]))
  | ADMonth Year Month (M.Map LocalTime [a])
  | ADTagged Tag [a]
  deriving (Show, Foldable, Traversable, Functor)

basicInterpretOptions :: InterpretOptions
basicInterpretOptions =
  defaultInterpretOptions
    { fieldModifier =
        over _head toLower
          . T.dropWhile isLower,
      constructorModifier = \c ->
        let (pr, po) = T.span isUpper c
         in T.last pr `T.cons` po
    }

instance B.Binary P.Alignment

instance B.Binary P.Block

instance B.Binary P.Caption

instance B.Binary P.Cell

instance B.Binary P.Citation

instance B.Binary P.CitationMode

instance B.Binary P.ColSpan

instance B.Binary P.ColWidth

instance B.Binary P.Format

instance B.Binary P.Inline

instance B.Binary P.ListNumberDelim

instance B.Binary P.ListNumberStyle

instance B.Binary P.MathType

instance B.Binary P.Meta

instance B.Binary P.MetaValue

instance B.Binary P.Pandoc

instance B.Binary P.QuoteType

instance B.Binary P.Row

instance B.Binary P.RowHeadColumns

instance B.Binary P.RowSpan

instance B.Binary P.TableBody

instance B.Binary P.TableFoot

instance B.Binary P.TableHead

instance B.Binary DT.Alignment

instance B.Binary DT.Border

instance B.Binary DT.Pipe

instance B.Binary DT.Variable

instance (B.Binary a) => B.Binary (DT.Doc a)

instance (B.Binary a) => B.Binary (DT.Template a)

instance Writable Sky.Syntax where
  write = B.encodeFile
