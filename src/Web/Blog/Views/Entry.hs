{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Entry (viewEntry) where

-- import Data.Monoid
-- import Data.Maybe
import Control.Monad.Reader
import Web.Blog.Render
import Web.Blog.Models
import Text.Pandoc
import qualified Data.Text as T

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

viewEntry :: Entry -> SiteRender H.Html
viewEntry entry = do
  return $ do

    H.h1 $ H.toHtml $ entryTitle entry
    H.div $ H.toHtml $ writeHtml (def WriterOptions) $
      readMarkdown (def ReaderOptions) $ T.unpack $ entryContent entry
