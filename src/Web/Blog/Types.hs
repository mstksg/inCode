module Web.Blog.Types (
    SiteData(..)
  , SiteEnvironment(..)
  , HostConfig(..)
  , DeveloperAPIs(..)
  , AuthorInfo(..)
  , AppPrefs(..)
  , DatabaseConfig(..)
  , SiteRender
  , PageDataMap
  , PageData(..)
  , SiteDatabase(..)
  , RenderData
  , RouteReader
  , RouteReaderM
  , RouteDatabase
  , KeyMapKey
  , KeyMapPair
  , KeyMap
  ) where

import "base" Prelude
import Control.Monad.Reader
import Web.Blog.Models
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as L
import qualified Database.Persist.Postgresql as D
import qualified Text.Blaze.Html5            as H
import qualified Web.Scotty                  as S

data SiteData = SiteData
                { siteDataTitle           :: T.Text
                , siteDataAuthorInfo      :: AuthorInfo
                , siteDataDescription     :: T.Text
                , siteDataCopyright       :: T.Text
                , siteDataPublicBlobs     :: Maybe T.Text
                , siteDataInteractiveUrl  :: Maybe T.Text
                , siteDataHostConfig      :: HostConfig
                , siteDataDeveloperAPIs   :: DeveloperAPIs
                , siteDataAppPrefs        :: AppPrefs
                , siteDataDatabaseConfig  :: Maybe DatabaseConfig
                , siteDataSiteEnvironment :: SiteEnvironment
                , siteDataShareLibs       :: Maybe FilePath
                , siteDataPackageConf     :: Maybe FilePath
                , siteDataPrecompileFay   :: Bool
                }

data SiteEnvironment = SiteEnvironmentProduction | SiteEnvironmentDevelopment

data HostConfig = HostConfig
                  { hostConfigHost :: T.Text
                  , hostConfigPort :: Maybe Int
                  }

data DeveloperAPIs = DeveloperAPIs
                     { developerAPIsAnalytics       :: (T.Text,T.Text)
                     , developerAPIsDisqusShortname :: T.Text
                     , developerAPIsFacebook        :: T.Text
                     , developerAPIsAddThis         :: T.Text
                     , developerAPIsFeedburner      :: T.Text
                     }

data AuthorInfo = AuthorInfo
                  { authorInfoName      :: T.Text
                  , authorInfoEmail     :: T.Text
                  , authorInfoRel       :: T.Text
                  , authorInfoFacebook  :: T.Text
                  , authorInfoTwitterID :: T.Text
                  , authorInfoGPlus     :: T.Text
                  , authorInfoGithub    :: T.Text
                  , authorInfoLinkedIn  :: T.Text
                  }

data AppPrefs = AppPrefs
                { appPrefsSlugLength  :: Int
                , appPrefsHomeEntries :: Int
                , appPrefsLedeMax     :: Int
                , appPrefsFeedEntries :: Int
                }

data DatabaseConfig = DatabaseConfig
                      { databaseConfigHost     :: T.Text
                      , databaseConfigName     :: T.Text
                      , databaseConfigUser     :: T.Text
                      , databaseConfigPassword :: T.Text
                      , databaseConfigPort     :: Int
                      }

type SiteRender a = ReaderT PageData S.ActionM a

data PageData = PageData
                { pageDataTitle    :: Maybe T.Text
                , pageDataDesc     :: Maybe T.Text
                , pageDataImage    :: Maybe FilePath
                , pageDataType     :: Maybe T.Text
                , pageDataUrl      :: Maybe T.Text
                , pageDataCss      :: [T.Text]
                , pageDataJs       :: [T.Text]
                , pageDataHeaders  :: [H.Html]
                , pageDataMap      :: PageDataMap
                }

type PageDataMap = M.Map T.Text T.Text

type KeyMapKey a = D.KeyBackend (D.PersistEntityBackend a) a

type KeyMapPair a = (KeyMapKey a, a)

type KeyMap a = M.Map (KeyMapKey a) a

data SiteDatabase = SiteDatabase
                    { siteDatabaseEntries   :: KeyMap Entry
                    , siteDatabaseTags      :: KeyMap Tag
                    , siteDatabaseEntryTags :: KeyMap EntryTag
                    , siteDatabaseSlugs     :: KeyMap Slug
                    }
                    deriving (Show)

-- instance Default SiteDatabase where
--     def = SiteDatabase empty empty empty empty

type RenderData  = (SiteRender H.Html, PageData)

type RouteReaderM a = ReaderT (SiteDatabase, PageData) (Either L.Text) a

type RouteReader  = RouteReaderM RenderData

type RouteDatabase = S.ActionM RouteReader

