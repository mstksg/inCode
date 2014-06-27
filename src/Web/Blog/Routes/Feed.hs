module Web.Blog.Routes.Feed (routeFeed) where

import "base" Prelude
import Config.SiteData
import Data.Time
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Data.Time                             (getCurrentTime)
import Web.Blog.Database
import Web.Blog.Models.EntryI
import Web.Blog.Models
import Web.Blog.Models.Types
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.Feed
import qualified Data.Text.Lazy              as L
import qualified Database.Persist.Postgresql as D
import qualified Web.Scotty                  as S

routeFeed :: S.ActionM (RouteReaderM L.Text)
routeFeed = readerFeed <$> liftIO getCurrentTime

  -- where
  --   m = appPrefsFeedEntries (siteDataAppPrefs siteData)

  -- eId <- S.param "eId"
  -- now <- liftIO getCurrentTime
  -- let url = L.append "/entry/id/" (L.pack (show eId))
  -- return $
  --   readerEntry now url =<< entryById eId False

readerFeed :: UTCTime -> RouteReaderM L.Text
readerFeed now = do
    posteds <- take m . sortEntries <$> postedEntriesI now
    entryInfos <- forM posteds $ \(eKey, e)-> do
                      up  <- getUrlPathI eKey
                      tgs <- filter ((== CategoryTag) . tagType_)
                               <$> getTagsI eKey
                      return (D.Entity eKey e, (up, tgs))
    return (viewFeed entryInfos now)
  where
    m = appPrefsFeedEntries (siteDataAppPrefs siteData)

-- routeFeed :: S.ActionM (SiteRender L.Text, PageData)
-- routeFeed = do
--   let
--     m = appPrefsFeedEntries $ siteDataAppPrefs siteData

--   now <- liftIO getCurrentTime
--   eList <- liftIO $ runDB $
--     postedEntries [ D.Desc EntryPostedAt
--                   , D.LimitTo m ]

--   let
--     wrapEntryInfo eEntity = do
--       urlAndTags <- liftIO . runDB $ (,)
--                                  <$> getUrlPath eEntity
--                                  <*> getTags eEntity [ TagType_ D.==. CategoryTag ]
--       return (eEntity, urlAndTags)

--   entryInfos <- mapM wrapEntryInfo eList

--   return (viewFeed entryInfos now, emptyPageData)

