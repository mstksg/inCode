{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Sidebar (viewSidebar) where

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

    H.ul $ do
      H.li $
        H.a ! A.href (I.textValue homeUrl) $
          "home"
      H.li $
        H.a ! A.href (I.textValue archiveUrl) $
          "entries"
      H.li $
        H.a ! A.href (I.textValue aboutUrl) $
          "about"


