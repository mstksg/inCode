{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Home (viewHome) where

-- import Data.Maybe
-- import Data.Monoid
import Control.Monad.Reader
import Text.Blaze.Html5 ((!))
import Web.Blog.Models
import Web.Blog.Types
import qualified Database.Persist.Postgresql as D
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

viewHome :: [(D.Entity Entry,(Maybe (D.Entity Slug),[Tag]))] -> SiteRender H.Html
viewHome eList = 
  return $ 
    H.div $ do
      H.h1 "Entry"
      H.p "This is an entry."
