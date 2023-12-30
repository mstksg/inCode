{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Web.Blaze where

import Blog.Render
import Blog.Types
import Control.Applicative
import qualified Data.Text as T
import Hakyll
import qualified Text.Blaze.Html.Renderer.String as H
import qualified Text.Blaze.Html5 as H

blazeCompiler ::
  (?config :: Config) =>
  PageData ->
  H.Html ->
  Compiler (Item String)
blazeCompiler pd body = do
  r <- getRoute =<< getUnderlying
  let r' = (<|> r) $ do
        rC <- r
        st <- T.stripSuffix "/index.html" $ T.pack rC
        return $ T.unpack st
      pd' = case pageDataCanonical pd of
        Nothing -> pd {pageDataCanonical = r'}
        Just _ -> pd
      h = renderLayout pd' body
  makeItem $ H.renderHtml h
