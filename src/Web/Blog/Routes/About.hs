{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.About (
    routeAbout
  ) where

import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.About

routeAbout :: RouteDatabase
routeAbout = do
  blankPageData <- genPageData

  let
    view = viewAbout
    pageData = blankPageData { pageDataTitle = Just "About Me" 
                             , pageDataCss   = ["/css/page/about.css"] }

  return $ siteRight (view,pageData)

