{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.NotFound (routeNotFound) where

import Control.Applicative     ((<$>))
import Data.List               (find)
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.NotFound
import qualified Web.Scotty    as S

routeNotFound :: RouteDatabase
routeNotFound = do
  err <- find ((== "err") . fst) <$> S.params
  blankPageData <- genPageData

  let
    view = viewNotFound
    pageData = blankPageData { pageDataTitle = Just "Not Found" 
                             , pageDataCss   = ["/css/page/not-found.css"] }

  return $ case err of
    Just _  -> siteLeft "/not-found"
    Nothing -> siteRight (view, pageData)

