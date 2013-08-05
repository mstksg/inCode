{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.Archive (
    routeArchiveAll
  , routeArchiveYear
  ) where

-- import Control.Monad.Reader
-- import Control.Monad.Trans
import Control.Applicative                   ((<$>))
import Control.Monad.IO.Class
import Control.Monad.Trans                   (lift)
import Data.Char                             (isDigit)
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Views.Archive
import Web.Blog.SiteData
import Web.Blog.Types
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as L
import qualified Database.Persist.Postgresql as D
import qualified Text.Blaze.Html5            as H
import qualified Web.Scotty                  as S
import qualified Data.Map as M
import Control.Monad.State
import Data.Time
import System.Locale

routeArchive :: T.Text -> [D.Filter Entry] -> RouteEither
routeArchive title filters = do
  eList <- liftIO $ runDB $
    postedEntriesFilter filters [ D.Desc EntryPostedAt ] >>= mapM wrapEntryData

  let
    view = viewArchive eList 
    pageData' = pageData { pageDataTitle = Just title }

  return $ Right (view, pageData')

routeArchiveAll = routeArchive "Entries" []

routeArchiveYear = do
  year <- S.param "year"
  let
    startYear = year :: Int
    endYear = startYear + 1
    startTime = buildTime defaultTimeLocale [('Y',show startYear)] :: UTCTime
    endTime = buildTime defaultTimeLocale [('Y',show endYear)] :: UTCTime
  
  
  routeArchive (T.pack $ show year) [ EntryPostedAt D.>=. startTime
                                    , EntryPostedAt D.<=. endTime  ]
