{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.Archive (
    routeArchiveAll
  , routeArchiveTag
  , routeArchiveYear
  , routeArchiveMonth
  ) where

import Control.Monad.IO.Class
import Data.Time
import System.Locale
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Models.Types
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.Archive
import qualified Data.Text                   as T
import qualified Database.Persist.Postgresql as D

routeArchive :: T.Text -> [D.Entity Entry] -> ViewArchiveType -> RouteDatabase
routeArchive title entries viewType = do
  let
    grouped = groupEntries entries

  eList' <- liftIO $ runDB $ mapM (mapM (mapM wrapEntryData)) grouped
  blankPageData <- genPageData

  let
    view = viewArchive eList' viewType
    pageData = blankPageData { pageDataTitle = Just title
                             , pageDataCss   = ["/css/page/archive.css"]
                             , pageDataJs    = ["/js/disqus_count.js"] }

  return $ siteRight (view, pageData)

routeArchiveFilters :: T.Text -> [D.Filter Entry] -> ViewArchiveType -> RouteDatabase
routeArchiveFilters title filters pdMap = do
  entries <- liftIO $ runDB $
    postedEntriesFilter filters [ D.Desc EntryPostedAt ]
  routeArchive title entries pdMap


routeArchiveAll :: RouteDatabase
routeArchiveAll = routeArchiveFilters "History" [] ViewArchiveAll

routeArchiveTag :: TagType -> T.Text -> RouteDatabase
routeArchiveTag type_ slug = do
  tag <- liftIO $ runDB $ D.getBy $ UniqueSlugType slug type_

  case tag of
    Just (D.Entity tagKey tag') -> do
      entrytags <- liftIO $ runDB $ D.selectList [ EntryTagTagId D.==. tagKey ] []
      let
        entryKeys = map (entryTagEntryId . D.entityVal) entrytags
        viewType = case type_ of
                GeneralTag  -> ViewArchiveTag
                CategoryTag -> ViewArchiveCategory
                SeriesTag   -> ViewArchiveSeries

      routeArchiveFilters (tagLabel' tag') [ EntryId D.<-. entryKeys ] $ viewType tag'

    Nothing ->
      return $ error404 "TagNotFound"


routeArchiveYear :: Int -> RouteDatabase
routeArchiveYear year = routeArchiveFilters (T.pack $ show year) filters $ ViewArchiveYear year
  where
    startTime = buildTime defaultTimeLocale [('Y',show year)] :: UTCTime
    endTime = buildTime defaultTimeLocale [('Y',show $ year + 1)] :: UTCTime
    filters = [ EntryPostedAt D.>=. Just startTime
              , EntryPostedAt D.<=. Just endTime  ]

routeArchiveMonth :: Int -> Int -> RouteDatabase
routeArchiveMonth year month = routeArchiveFilters (T.pack timeString) filters $ ViewArchiveMonth year month
  where
    startDay = buildTime defaultTimeLocale
      [('Y',show year),('m',show month)] :: Day
    endDay = addGregorianMonthsRollOver 1 startDay
    startTime = UTCTime startDay 0
    endTime = UTCTime endDay 0
    filters = [ EntryPostedAt D.>=. Just startTime
              , EntryPostedAt D.<=. Just endTime  ]
    timeString = formatTime defaultTimeLocale "%B %Y" startDay

