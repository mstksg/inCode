{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Layout (viewLayout) where

import Control.Applicative
import Control.Monad.Reader
import Data.Monoid
import Text.Blaze.Html5 ((!))
import Web.Blog.Render
import Web.Blog.SiteData

import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Web.Scotty as S

viewLayout :: SiteRender H.Html -> SiteRender H.Html
viewLayout body = do
  pageData' <- ask
  bodyHtml <- body
  title <- createTitle

  testUrl <- renderUrl "/"

  return $ H.docTypeHtml $ do

    H.head $ do
      H.title (title)
      H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
      sequence_ (pageDataHeaders pageData')

    H.body $ do

      H.div ! A.id "header_container" $
        H.div ! A.id "header_content" $ do
          H.toHtml testUrl
          -- H.toHtml testUrl
          -- mempty

      H.div ! A.id "main_container" $
        H.div ! A.id "main_content" $
          bodyHtml

      H.div ! A.id "footer_container" $
        H.div ! A.id "footer_content" $
          "© Justin Le 2013"

createTitle :: SiteRender H.Html
createTitle = do
  pageData' <- ask
  let
    siteTitle = siteDataTitle $ pageSiteData $ pageData'
    pageTitle = pageDataTitle $ pageData'
    combined   = case pageTitle of
      Just title -> T.concat [siteTitle, " - ", title]
      Nothing    -> siteTitle
  return $ H.toHtml combined
 
