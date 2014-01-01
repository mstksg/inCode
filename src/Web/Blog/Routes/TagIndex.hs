{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.TagIndex (
    routeTagIndex
  ) where

import Control.Monad.IO.Class
import Web.Blog.Database
import Web.Blog.Models.Types
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.TagIndex

routeTagIndex :: TagType -> RouteDatabase
routeTagIndex tt = do
  let
    sorting = case tt of
                GeneralTag -> TagSortCount
                -- CategoryTag -> TagSortLabel
                CategoryTag -> TagSortCount
                SeriesTag -> TagSortRecent

  tagInfos <- liftIO $ runDB $ getTagInfoList tt sorting (tt /= GeneralTag)
  blankPageData <- genPageData

  let
    view = viewTagIndex tagInfos tt
    title = case tt of
              GeneralTag -> "Tags List"
              CategoryTag -> "Category List"
              SeriesTag -> "Series List"
    pageData = blankPageData { pageDataTitle = Just title
                             , pageDataCss   = ["/css/page/archive.css"] }

  return $ siteRight (view,pageData)
