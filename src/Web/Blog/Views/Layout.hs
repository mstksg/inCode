{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Layout (viewLayout) where

import Control.Applicative
import Control.Monad.Reader
import Data.Monoid
import Text.Blaze.Html5 ((!))
import Web.Blog.Render
import Web.Blog.SiteData
import Web.Blog.Views.Sidebar

import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal as I
import qualified Web.Scotty as S

viewLayout :: SiteRender H.Html -> SiteRender H.Html
viewLayout body = do
  pageData' <- ask
  bodyHtml <- body
  sidebarHtml <- viewSidebar
  title <- createTitle

  cssUrl <- renderUrl "/css/gridiculous.css"

  return $ H.docTypeHtml $ do

    H.head $ do

      H.title title
      H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"

      H.link ! A.href (I.textValue cssUrl) ! A.rel "stylesheet" ! A.type_ "text/css"
      H.link ! A.rel "author" ! A.href (I.textValue $ siteDataAuthorRel $ pageSiteData pageData')

      sequence_ (pageDataHeaders pageData')

    H.body ! A.class_ "grid w960" $ do
      
      -- H.div ! A.id "body_grid" ! A.class_ "grid w960" $ do

        H.div ! A.id "header_container" ! A.class_ "row" $
          H.div ! A.id "header_content" ! A.class_ "c12" $
            mempty
        
        H.div ! A.id "main_container" ! A.class_ "row" $ do

          H.div ! A.id "sidebar" ! A.class_ "c3" $ 
            sidebarHtml

          H.div ! A.id "main_content" ! A.class_ "c9 end" ! I.customAttribute "role" "main" $
            bodyHtml

        H.div ! A.id "footer_container" ! A.class_ "row" $

          H.div ! A.id "footer_content" ! A.class_ "c12" $
            H.preEscapedToHtml ("&copy; Justin Le 2013" :: T.Text)

createTitle :: SiteRender H.Html
createTitle = do
  pageData' <- ask
  let
    siteTitle = siteDataTitle $ pageSiteData pageData'
    pageTitle = pageDataTitle pageData'
    combined   = case pageTitle of
      Just title -> T.concat [siteTitle, " - ", title]
      Nothing    -> siteTitle
  return $ H.toHtml combined
 
