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

routeTagIndex :: TagType -> RouteEither
routeTagIndex tt = do
  tagInfos <- liftIO $ runDB $ getTagInfoListRecent tt (tt /= GeneralTag)

  let
    view = viewTagIndex tagInfos tt
    title = case tt of
              GeneralTag -> "Tags List"
              CategoryTag -> "Category List"
              SeriesTag -> "Series List"
    pageData' = pageData { pageDataTitle = Just title
                         , pageDataCss   = Just "/css/page/archive.min.css" }

  return $ Right (view,pageData')
