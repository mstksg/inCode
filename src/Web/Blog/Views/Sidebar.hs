{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Sidebar (viewSidebar) where

-- import Data.Monoid
-- import Data.Maybe
import Control.Monad.Reader
import Web.Blog.Render
import Web.Blog.Models
import qualified Data.Text as T

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

viewSidebar :: SiteRender H.Html
viewSidebar = 
  return $ do
    H.h2 "Sidebar"
    H.p "Sidebar content"

