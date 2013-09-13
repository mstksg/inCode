{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.About (
    routeAbout
  ) where

import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.About

routeAbout :: RouteEither
routeAbout = do
  let
    view = viewAbout
    pageData' = pageData { pageDataTitle = Just "About Me" 
                         , pageDataCss   = ["/css/page/about.css"] }

  return $ Right (view,pageData')

