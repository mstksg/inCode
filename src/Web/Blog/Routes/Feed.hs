{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.Feed (routeFeed) where

import Control.Monad.IO.Class
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Models.Types
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.Feed
import qualified Data.Text.Lazy              as L
import qualified Database.Persist.Postgresql as D
import qualified Web.Scotty                  as S

routeFeed :: S.ActionM (SiteRender L.Text, PageData)
routeFeed = do
  eList <- liftIO $ runDB $
    postedEntries [ D.Desc EntryPostedAt
                  , D.LimitTo 25 ]

  let
    wrapEntryInfo eEntity = do
      entryUrl <- liftIO $ runDB $ getUrlPath eEntity
      entryTags <- liftIO $ runDB $ getTags eEntity [ TagType_ D.!=. SeriesTag ]
      return (eEntity,(entryUrl,entryTags))

  entryInfos <- mapM wrapEntryInfo eList

  return (viewFeed entryInfos, pageData)
