{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Entry (viewEntry) where

-- import Data.Monoid
-- import Data.Maybe
import Control.Monad.Reader
import Web.Blog.Render

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

viewEntry :: HtmlRender
viewEntry = return $ H.docTypeHtml $
  H.div $ do
    H.div $ do
    H.h1 "Entry"
    H.p "This is an entry."

