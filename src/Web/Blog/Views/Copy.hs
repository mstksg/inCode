
module Web.Blog.Views.Copy (
    viewCopy
  , viewCopyFile
  ) where

-- import Control.Applicative                   ((<$>))
-- import Control.Monad.Reader
-- import Web.Blog.Models
-- import Web.Blog.Models.Util
-- import Web.Blog.Render
-- import Web.Blog.SiteData
-- import Web.Blog.Util                         (renderFriendlyTime, renderDatetimeTime)
-- import qualified Data.Map                    as M
-- import qualified Data.Text                   as T
-- import qualified Database.Persist.Postgresql as D
-- import qualified Text.Blaze.Internal         as I
import Control.Monad.IO.Class
import Data.Monoid
import Text.Blaze.Html5                         ((!))
import Web.Blog.Types
import qualified Data.Text                      as T
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import qualified Text.Pandoc                    as P


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
  copyMarkdown <- liftIO $ readFile filepath
  let
    copyPandoc = P.readMarkdown (P.def P.ReaderOptions) copyMarkdown
    copyHtml = P.writeHtml (P.def P.WriterOptions) copyPandoc
  viewCopy title copyHtml
