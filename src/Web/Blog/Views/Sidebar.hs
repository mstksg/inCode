{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Sidebar (viewSidebar) where

-- import Data.Maybe
-- import Data.Monoid
import Control.Monad.Reader
import Text.Blaze.Html5 ((!))
import Web.Blog.Models
import Web.Blog.Render
import Web.Blog.Types
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

viewSidebar :: SiteRender H.Html
viewSidebar = 
  return $ do
    H.h2 "Sidebar"
    H.p "Sidebar content"

