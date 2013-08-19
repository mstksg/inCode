{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.Feed (routeFeed) where

import Web.Blog.Types
import qualified Data.Text.Lazy as L
import Web.Blog.Render
import qualified Web.Scotty as S

routeFeed :: S.ActionM (SiteRender L.Text, PageData)
routeFeed = return (return "", pageData)
