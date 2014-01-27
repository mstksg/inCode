module Web.Blog.Views.NotFound (viewNotFound) where

import "base" Prelude
import Web.Blog.Render
import Web.Blog.Types
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as I

viewNotFound :: SiteRender H.Html
viewNotFound = do
  homeUrl <- renderUrl "/"
  archiveUrl <- renderUrl "/entries"

  return $
    H.section H.! A.class_ "tile main-content single-page not-found-section unit span-grid" H.! mainSection $ do

      H.header $
        H.h1 "Not Found"

      H.hr

      H.div H.! A.class_ "copy-content" $
        H.p $ do
          "The page you were looking for was not found.  Sorry!  " :: H.Html
          "Maybe try going to the " :: H.Html
          H.a H.! A.href (I.textValue homeUrl) $ "homepage"
          " or checking out the " :: H.Html
          H.a H.! A.href (I.textValue archiveUrl) $ "archives" :: H.Html
          "?" :: H.Html

