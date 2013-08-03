{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.NotFound (viewNotFound) where

-- import Data.Maybe
-- import Data.Monoid
import Control.Monad.Reader
import Text.Blaze.Html5 ((!))
import Web.Blog.Models
import Web.Blog.Types
import qualified Database.Persist.Postgresql as D
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

viewNotFound :: SiteRender H.Html
viewNotFound = 
  return $ 
    H.section $ do
      H.h1 "Not Found"
      H.p "The page you were looking for was not found.  Sorry!"

