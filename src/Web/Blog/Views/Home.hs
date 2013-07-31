{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Home (viewHome) where

-- import Data.Monoid
-- import Data.Maybe

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad.Reader

viewHome :: H.Html
viewHome = 
  H.div $ do
    H.h1 "Entry"
    H.p "This is an entry."
