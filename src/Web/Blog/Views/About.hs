{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.About (
    viewAbout
  ) where

import Text.Blaze.Html5                      ((!))
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Util
import qualified Data.Foldable as Fo         (forM_)
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as I

viewAbout :: SiteRender H.Html
viewAbout = return $ do
  H.header $
    H.h1 "About me"
  H.div $
    H.p 
      "I'm a person."


