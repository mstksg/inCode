{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Layout (viewLayout) where

import Control.Monad.Reader
import Data.Monoid
import Text.Blaze.Html5 ((!))
import Web.Blog.Render
import Web.Blog.SiteData

import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

viewLayout :: HtmlRender -> HtmlRender
viewLayout body = do
  pageData' <- ask
  bodyHtml <- body
  renderedTitle <- renderTitle

  -- agent <- lift $ S.reqHeader "User-Agent"

  return $ H.docTypeHtml $ do

    H.head $ do
      H.title (renderedTitle)
      H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
      sequence_ (pageDataHeaders pageData')

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

renderTitle :: HtmlRender
renderTitle = do
  pageData' <- ask
  let
    siteTitle = siteDataTitle $ pageSiteData $ pageData'
    pageTitle = pageDataTitle $ pageData'
    combined   = case pageTitle of
      Just title -> T.concat [siteTitle, " - ", title]
      Nothing    -> siteTitle
  return $ H.toHtml combined
    


  
