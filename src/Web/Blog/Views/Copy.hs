
module Web.Blog.Views.Copy (
    viewCopy
  , viewCopyFile
  ) where

import Data.Monoid
import Text.Blaze.Html5                         ((!))
import Web.Blog.Render
import Web.Blog.Types
import qualified Data.Text                      as T
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A


viewCopy :: T.Text -> H.Html -> SiteRender H.Html
viewCopy title copy =
  return $ do
    H.header $
      H.h1 $
        H.toHtml title
    H.hr
    H.div ! A.class_ "copy-content" $
      copy
    H.div ! A.class_ "clear" $
      mempty

viewCopyFile :: T.Text -> FilePath -> SiteRender H.Html
viewCopyFile title filepath = do
  copyHtml <- renderRawCopy filepath
  viewCopy title copyHtml
