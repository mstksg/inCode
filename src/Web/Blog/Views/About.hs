module Web.Blog.Views.About (
    viewAbout
  ) where

import "base" Prelude
import Text.Blaze.Html5                      ((!))
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.Copy
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

viewAbout :: SiteRender H.Html
viewAbout = do
  copy <- viewCopyFile "About me" "copy/static/about.md"

  return $
    H.section ! A.class_ "tile main-content single-page about-section unit span-grid" ! mainSection $
      copy
