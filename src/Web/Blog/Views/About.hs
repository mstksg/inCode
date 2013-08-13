{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.About (
    viewAbout
  ) where

import Text.Blaze.Html5                      ((!))
import Web.Blog.Types
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

viewAbout :: SiteRender H.Html
viewAbout = return $
  H.section ! A.class_ "tile single-page about-section" $ do
    H.header $
      H.h1 "About me"

    H.hr

    H.div ! A.class_ "main-content copy-content" $
      H.p 
        "I'm a person."


