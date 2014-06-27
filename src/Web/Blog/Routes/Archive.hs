module Web.Blog.Routes.Archive (
    routeArchiveAll
  , routeArchiveTag
  , routeArchiveYear
  , routeArchiveMonth
  ) where

import "base" Prelude hiding                 (find, elem)
import Control.Applicative
import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Foldable                         (find, elem)
import Data.Time
import System.Locale
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Models.EntryI
import Web.Blog.Models.Types
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.Archive
import qualified Data.Map.Strict             as M
import qualified Data.Text                   as T
import qualified Database.Persist.Postgresql as D

routeArchive :: T.Text -> ViewArchiveType -> [D.Entity Entry] -> RouteReader
routeArchive title viewType entries = do
    eList <- (mapM . mapM . mapM) ( (fmap . first) (uncurry D.Entity)
                                  . wrapEntryDataI . D.entityKey
                                  ) grouped
    blankData <- asks snd

    let view     = viewArchive eList viewType
        pageData = blankData { pageDataTitle = Just title
                             , pageDataCss   = ["/css/page/archive.css"]
                             , pageDataJs    = ["/js/disqus_count.js"]
                             }

    siteRight (view, pageData)
  where
    grouped = groupEntries entries

  -- eList' <- liftIO $ runDB $ mapM (mapM (mapM wrapEntryData)) grouped
  -- blankPageData <- genPageData

  -- let
  --   view = viewArchive eList' viewType
  --   pageData = blankPageData { pageDataTitle = Just title
  --                            , pageDataCss   = ["/css/page/archive.css"]
  --                            , pageDataJs    = ["/js/disqus_count.js"] }

  -- return $ siteRight (view, pageData)

routeArchiveFiltersI :: T.Text
                     -> (KeyMapPair Entry -> Bool)
                     -> ViewArchiveType
                     -> UTCTime
                     -> RouteReader
routeArchiveFiltersI title p viewType now = routeArchive title viewType
                                          . map (uncurry D.Entity)
                                          . filter p
                                          . sortEntries
                                        =<< postedEntriesI now

routeArchiveFilters :: T.Text -> [D.Filter Entry] -> ViewArchiveType -> RouteDatabase
routeArchiveFilters title filters pdMap = fmap (routeArchive title pdMap)
                                        . liftIO . runDB
                                        $ postedEntriesFilter filters [ D.Desc EntryPostedAt ]

routeArchiveAll :: RouteDatabase
routeArchiveAll = routeArchiveFiltersI "History" (\_ -> True) ViewArchiveAll
              <$> liftIO getCurrentTime
    -- routeArchiveFilters "History" [] ViewArchiveAll

routeArchiveTag :: TagType -> T.Text -> RouteDatabase
routeArchiveTag type_ slug = do
    now <- liftIO getCurrentTime

    return $ do
      db  <- askDb

      let tag = find (\(_,Tag _ t _ s) -> t == type_ && s == slug)
              . M.toList
              . siteDatabaseTags
              $ db

      case tag of
        Nothing    ->
          error404 "TagNotFound"

        Just (k,t) -> do
          let entryKeys = fmap entryTagEntryId
                        . M.filter ((== k) . entryTagTagId)
                        . siteDatabaseEntryTags
                        $ db

          routeArchiveFiltersI (tagLabel' t) ((`elem` entryKeys) . fst) (viewType t) now

  where
    viewType = case type_ of
                 GeneralTag  -> ViewArchiveTag
                 CategoryTag -> ViewArchiveCategory
                 SeriesTag   -> ViewArchiveSeries

  -- tag <- liftIO $ runDB $ D.getBy $ UniqueSlugType slug type_

  -- case tag of
  --   Just (D.Entity tagKey tag') -> do
  --     entrytags <- liftIO $ runDB $ D.selectList [ EntryTagTagId D.==. tagKey ] []
  --     let
  --       entryKeys = map (entryTagEntryId . D.entityVal) entrytags
  --       viewType = case type_ of
  --               GeneralTag  -> ViewArchiveTag
  --               CategoryTag -> ViewArchiveCategory
  --               SeriesTag   -> ViewArchiveSeries

  --     routeArchiveFilters (tagLabel' tag') [ EntryId D.<-. entryKeys ] $ viewType tag'

  --   Nothing ->
  --     return $ error404 "TagNotFound"


routeArchiveYear :: Int -> RouteDatabase
routeArchiveYear year = routeArchiveFiltersI showYear p viewType
                    <$> liftIO getCurrentTime
  where
    showYear  = T.pack (show year)
    viewType  = ViewArchiveYear year
    startTime = buildTime defaultTimeLocale [('Y',show year)] :: UTCTime
    endTime   = buildTime defaultTimeLocale [('Y',show $ year + 1)] :: UTCTime
    p (_,e)   = ((&&) <$> (>= Just startTime) <*> (<= Just endTime))
              . entryPostedAt
              $ e

    -- filters = [ EntryPostedAt D.>=. Just startTime
    --           , EntryPostedAt D.<=. Just endTime  ]

routeArchiveMonth :: Int -> Int -> RouteDatabase
routeArchiveMonth year month = routeArchiveFiltersI timeString p (ViewArchiveMonth year month)
                           <$> liftIO getCurrentTime
  where
    startDay = buildTime defaultTimeLocale
      [('Y',show year),('m',show month)] :: Day
    endDay = addGregorianMonthsRollOver 1 startDay
    startTime = UTCTime startDay 0
    endTime = UTCTime endDay 0
    p (_,e)   = ((&&) <$> (>= Just startTime) <*> (<= Just endTime))
              . entryPostedAt
              $ e
    timeString = T.pack $ formatTime defaultTimeLocale "%B %Y" startDay

    -- filters = [ EntryPostedAt D.>=. Just startTime
    --           , EntryPostedAt D.<=. Just endTime  ]
-- routeArchiveFilters (T.pack timeString) filters $ ViewArchiveMonth year month

