{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.Archive (
    routeArchiveAll
  , routeArchiveTag
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
import Web.Blog.Models.Types

routeArchive :: T.Text -> [D.Entity Entry] -> RouteEither
routeArchive title entries = do
  eList <- liftIO $ runDB $ mapM wrapEntryData entries
  let
    view = viewArchive eList 
    pageData' = pageData { pageDataTitle = Just title }

  return $ Right (view, pageData')

routeArchiveFilters :: T.Text -> [D.Filter Entry] -> RouteEither
routeArchiveFilters title filters = do
  entries <- liftIO $ runDB $
    postedEntriesFilter filters [ D.Desc EntryPostedAt ]
  routeArchive title entries


routeArchiveAll :: RouteEither
routeArchiveAll = routeArchiveFilters "Entries" []

routeArchiveTag :: TagType -> T.Text -> RouteEither
routeArchiveTag type_ slug = do
  tag <- liftIO $ runDB $ D.getBy $ UniqueSlugType slug type_

  case tag of
    Just (D.Entity tagKey tag') -> do
      entrytags <- liftIO $ runDB $ D.selectList [ EntryTagTagId D.==. tagKey ] []
      let
        entryKeys = map (entryTagEntryId . D.entityVal) entrytags

      routeArchiveFilters (tagLabel' tag') [ EntryId D.<-. entryKeys ]
      
    Nothing ->
      return $ error404 "TagNotFound"


routeArchiveYear :: Int -> RouteEither
routeArchiveYear year = routeArchiveFilters (T.pack $ show year) filters
  where
    startTime = buildTime defaultTimeLocale [('Y',show year)] :: UTCTime
    endTime = buildTime defaultTimeLocale [('Y',show $ year + 1)] :: UTCTime
    filters = [ EntryPostedAt D.>=. startTime
              , EntryPostedAt D.<=. endTime  ]

routeArchiveMonth :: Int -> Int -> RouteEither
routeArchiveMonth year month = routeArchiveFilters (T.pack timeString) filters
  where
    startDay = buildTime defaultTimeLocale
      [('Y',show year),('m',show month)] :: Day
    endDay = addGregorianMonthsRollOver 1 startDay
    startTime = UTCTime startDay 0
    endTime = UTCTime endDay 0
    filters = [ EntryPostedAt D.>=. startTime
              , EntryPostedAt D.<=. endTime  ]
    timeString = formatTime defaultTimeLocale "%B %Y" startDay
