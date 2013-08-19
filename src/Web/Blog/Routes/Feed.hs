{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.Feed (routeFeed) where

import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.Feed
import qualified Data.Text.Lazy as L
import qualified Web.Scotty     as S

routeFeed :: S.ActionM (SiteRender L.Text, PageData)
routeFeed = return (viewFeed, pageData)
