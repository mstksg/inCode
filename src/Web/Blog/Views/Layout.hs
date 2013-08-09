{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Layout (viewLayout, viewLayoutEmpty) where

import Control.Monad.Reader
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
    ["/css/reset.css"
    ,"/css/gridiculous.css"
    ,"/css/main.css"
    ]


  return $ H.docTypeHtml $ do

    H.head $ do

      H.title title
      H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"

      forM_ cssList $ \u ->
        H.link ! A.href (I.textValue u) ! A.rel "stylesheet" ! A.type_ "text/css"

      H.link ! A.rel "author" ! A.href (I.textValue $ siteDataAuthorRel $ pageSiteData pageData')

      -- renderFonts [("Sorts+Mill+Goudy",["400","400italic"])
      --             ,("Lato",["400","700"])]
      H.link ! A.href 
        (I.textValue "http://fonts.googleapis.com/css?family=Lato:400,700|Sorts+Mill+Goudy:400,400italic") !
        A.rel "stylesheet" ! A.type_ "text/css"

      sequence_ (pageDataHeaders pageData')

    H.body ! A.class_ "grid w960" $ do
      
        H.div ! A.id "header-container" ! A.class_ "row" $
          H.div ! A.id "header-content" ! A.class_ "c12" $
            mempty
        
        H.div ! A.id "body-container" ! A.class_ "row" $ do

          H.div ! A.id "sidebar-container" ! A.class_ "c3" $ 
            sidebarHtml

          H.div ! A.id "main-container" ! A.class_ "c9 end" ! I.customAttribute "role" "main" $
            bodyHtml

        H.div ! A.id "footer-container" ! A.class_ "row" $

          H.div ! A.id "footer-content" ! A.class_ "c12" $
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
