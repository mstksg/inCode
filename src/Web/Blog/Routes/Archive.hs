{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.Archive (
    routeArchiveAll
  , routeArchiveTag
  , routeArchiveYear
  , routeArchiveMonth
  ) where

-- import Control.Applicative                ((<$>))
-- import Control.Monad.Reader
-- import Control.Monad.State
-- import Control.Monad.Trans
-- import Control.Monad.Trans                (lift)
-- import Data.Char                          (isDigit)
-- import Web.Blog.SiteData
-- import qualified Data.Map                 as M
-- import qualified Data.Text.Lazy           as L
-- import qualified Text.Blaze.Html5         as H
-- import qualified Web.Scotty               as S
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

routeArchive :: T.Text -> [D.Entity Entry] -> ViewArchiveType -> RouteEither
routeArchive title entries vat = do
  let
    grouped = groupEntries entries
  eList' <- liftIO $ runDB $ mapM (mapM (mapM wrapEntryData)) grouped
  let
    view = viewArchive eList' vat
    pageData' = pageData { pageDataTitle = Just title }

  return $ Right (view, pageData')

routeArchiveFilters :: T.Text -> [D.Filter Entry] -> ViewArchiveType -> RouteEither
routeArchiveFilters title filters pdMap = do
  entries <- liftIO $ runDB $
    postedEntriesFilter filters [ D.Desc EntryPostedAt ]
  routeArchive title entries pdMap


routeArchiveAll :: RouteEither
routeArchiveAll = routeArchiveFilters "Entries" [] ViewArchiveAll

routeArchiveTag :: TagType -> T.Text -> RouteEither
routeArchiveTag type_ slug = do
  tag <- liftIO $ runDB $ D.getBy $ UniqueSlugType slug type_

  case tag of
    Just (D.Entity tagKey tag') -> do
      entrytags <- liftIO $ runDB $ D.selectList [ EntryTagTagId D.==. tagKey ] []
      let
        entryKeys = map (entryTagEntryId . D.entityVal) entrytags
        vat = case type_ of
                GeneralTag  -> ViewArchiveTag
                CategoryTag -> ViewArchiveCategory
                SeriesTag   -> ViewArchiveSeries

      routeArchiveFilters (tagLabel' tag') [ EntryId D.<-. entryKeys ] vat
      
    Nothing ->
      return $ error404 "TagNotFound"


routeArchiveYear :: Int -> RouteEither
routeArchiveYear year = routeArchiveFilters (T.pack $ show year) filters ViewArchiveYear
  where
    startTime = buildTime defaultTimeLocale [('Y',show year)] :: UTCTime
    endTime = buildTime defaultTimeLocale [('Y',show $ year + 1)] :: UTCTime
    filters = [ EntryPostedAt D.>=. startTime
              , EntryPostedAt D.<=. endTime  ]

routeArchiveMonth :: Int -> Int -> RouteEither
routeArchiveMonth year month = routeArchiveFilters (T.pack timeString) filters ViewArchiveMonth
  where
    startDay = buildTime defaultTimeLocale
      [('Y',show year),('m',show month)] :: Day
    endDay = addGregorianMonthsRollOver 1 startDay
    startTime = UTCTime startDay 0
    endTime = UTCTime endDay 0
    filters = [ EntryPostedAt D.>=. startTime
              , EntryPostedAt D.<=. endTime  ]
    timeString = formatTime defaultTimeLocale "%B %Y" startDay

