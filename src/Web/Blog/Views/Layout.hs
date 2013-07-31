{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Layout (viewLayout) where

import Data.Monoid
import Data.Maybe
import Control.Monad.Reader
import qualified Web.Scotty as S

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Text as T

import Web.Blog.Render

viewLayout :: HtmlRender -> HtmlRender
viewLayout body = do
  pageData' <- ask
  bodyHtml <- body

  -- agent <- lift $ S.reqHeader "User-Agent"

  return $ H.docTypeHtml $ do

    H.head $ do
      H.title (H.toHtml (fromMaybe "Blog" (pageTitle pageData')))
      H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
      sequence_ (pageHeaders pageData')

    H.body $ do

      H.div ! A.id "header_container" $
        H.div ! A.id "header_content" $
          mempty

      H.div ! A.id "main_container" $
        H.div ! A.id "main_content" $
          bodyHtml

      H.div ! A.id "footer_container" $
        H.div ! A.id "footer_content" $
          "© Justin Le 2013"

