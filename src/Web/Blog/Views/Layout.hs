{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Layout (viewLayout, viewLayoutEmpty) where

import Control.Monad.Reader
import Data.Maybe                            (fromMaybe)
import Data.Monoid
import Text.Blaze.Html5                      ((!))
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.Sidebar
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as I

viewLayout :: SiteRender H.Html -> SiteRender H.Html
viewLayout body = do
  pageData' <- ask
  bodyHtml <- body
  sidebarHtml <- viewSidebar
  title <- createTitle

  cssList <- mapM renderUrl
    [
      "/css/toast.css"
    , "/css/font.css"
    -- , "/css/jquery.tocify.css"
    -- , "/css/pepper-grinder/jquery-ui-1.10.3.custom.min.css"
    , fromMaybe "/css/main.min.css" $ pageDataCss pageData'
    ]


  return $ H.docTypeHtml $ do

    H.head $ do

      H.title title
      H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width,initial-scale=1.0"

      forM_ cssList $ \u ->
        H.link ! A.href (I.textValue u) ! A.rel "stylesheet" ! A.type_ "text/css"

      H.link ! A.rel "author" ! A.href (I.textValue $ siteDataAuthorRel $ pageSiteData pageData')

      -- renderFonts [("Sorts+Mill+Goudy",["400","400italic"])
      --             ,("Lato",["400","700"])]
      -- H.link ! A.href 
        -- (I.textValue "http://fonts.googleapis.com/css?family=Lora:400,700,400italic,700italic|Playfair+Display:400,700,900,400italic,700italic,900italic") !
        -- (I.textValue "http://fonts.googleapis.com/css?family=Lora:400,700,400italic,700italic") !
        -- (I.textValue "http://fonts.googleapis.com/css?family=Vollkorn:400,700,400italic,700italic") !
        -- (I.textValue "http://fonts.googleapis.com/css?family=Prociono") !
        -- A.rel "stylesheet" ! A.type_ "text/css"

      H.script ! A.type_ "text/javascript" ! A.src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" $
        mempty
      -- H.script ! A.type_ "text/javascript" ! A.src "/js/jquery-ui-1.10.3.custom.min.js" $
      --   mempty
      -- H.script ! A.type_ "text/javascript" ! A.src "/js/jquery.tocify.min.js" $
      --   mempty

      sequence_ (pageDataHeaders pageData')

    H.body $ do
      
        H.div ! A.id "header-container" $
          H.div ! A.id "header-content" $
            mempty
        
        H.div ! A.id "body-container" ! A.class_ "container" $
          H.div ! A.id "body-grid" ! A.class_ "grid" $ do

            H.div ! A.id "sidebar-container" ! A.class_ "unit one-of-four" $ 
              sidebarHtml

            H.div ! A.id "main-container" ! A.class_ "unit three-of-four" ! I.customAttribute "role" "main" $
              bodyHtml

        H.div ! A.id "footer-container" $
          H.div ! A.id "footer-content" $
            H.div ! A.class_ "tile" $
              H.preEscapedToHtml ("&copy; Justin Le 2013" :: T.Text)

viewLayoutEmpty :: SiteRender H.Html
viewLayoutEmpty = viewLayout $ return mempty

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
 
-- renderFonts :: [(T.Text,[T.Text])] -> H.Html
-- renderFonts fs = H.link ! A.href l ! A.rel "stylesheet" ! A.type_ "text/css"
 --  where
 --    l = I.textValue $ T.concat $ map makeFont fs
 --    makeFont (n,ts) = T.append n $ T.intersperse ',' ts
