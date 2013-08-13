{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Sidebar (viewSidebar) where

-- import Data.Monoid                           (mempty)
import Text.Blaze.Html5                      ((!))
import Web.Blog.Render
import Web.Blog.Types
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as I

viewSidebar :: SiteRender H.Html
viewSidebar = do
  homeUrl <- renderUrl "/"
  archiveUrl <- renderUrl "/entries"
  aboutUrl <- renderUrl "/about"

  return $

    H.div ! A.class_ "sidebar-content tile" $ do
      H.a ! A.href (I.textValue homeUrl) ! A.class_ "home-link" $
        "home"

      H.p 
        "A blog about stuff, more things, and perhaps even more."

      H.nav $
        H.ul $ do
          H.li $
            H.a ! A.href (I.textValue archiveUrl) $
              "archives"
          H.li $
            H.a ! A.href (I.textValue aboutUrl) $
              "me"

    -- H.div ! A.class_ "tile toc" $ mempty


