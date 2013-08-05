{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.Archive (
    routeArchiveAll
  , routeArchiveYear
  , routeArchiveMonth
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

routeArchiveAll :: RouteEither
routeArchiveAll = routeArchive "Entries" []

routeArchiveYear :: Int -> RouteEither
routeArchiveYear year = routeArchive (T.pack $ show year) filters
  where
    startYear = year :: Int
    endYear = startYear + 1
    startTime = buildTime defaultTimeLocale [('Y',show startYear)] :: UTCTime
    endTime = buildTime defaultTimeLocale [('Y',show endYear)] :: UTCTime
    filters = [ EntryPostedAt D.>=. startTime
              , EntryPostedAt D.<=. endTime  ]

routeArchiveMonth :: Int -> Int -> RouteEither
routeArchiveMonth year month = routeArchive (T.pack $ show year) filters
  where
    startDay = buildTime defaultTimeLocale
      [('Y',show (year :: Int)),('m',show (month :: Int))] :: Day
    endDay = addGregorianMonthsRollOver 1 startDay
    startTime = UTCTime startDay 0
    endTime = UTCTime endDay 0
    filters = [ EntryPostedAt D.>=. startTime
              , EntryPostedAt D.<=. endTime  ]
