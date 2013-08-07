{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.TagIndex (
    viewTagIndex
  ) where

-- import Control.Applicative                   ((<$>))
-- import Data.Monoid
import Web.Blog.Render
-- import Web.Blog.SiteData
-- import Web.Blog.Util
-- import qualified Data.Map                    as M
-- import qualified Database.Persist.Postgresql as D
import Control.Monad.Reader
import Data.Maybe                               (isJust, fromJust)
import Text.Blaze.Html5                         ((!))
import Web.Blog.Models
import Web.Blog.Models.Types
import Web.Blog.Models.Util
import Web.Blog.Types
import Web.Blog.Views.Archive
import qualified Data.Foldable as Fo            (forM_)
import qualified Data.Text                      as T
-- import qualified Data.Traversable  as Tr        (mapM)
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import qualified Text.Blaze.Internal            as I

viewTagIndex :: [TagInfo] -> TagType -> SiteRender H.Html
viewTagIndex tagInfos tt = do

  nav <- viewArchiveNav $ Just $
    case tt of
      GeneralTag  -> ViewArchiveIndexTag
      CategoryTag -> ViewArchiveIndexCategory
      SeriesTag   -> ViewArchiveIndexSeries

  return $ do

    H.header $ do

      H.h1 $
        case tt of
          GeneralTag  -> "Tags"
          CategoryTag -> "Categories"
          SeriesTag   -> "Series"

      nav

    H.ul $
      mapM_ (tagIndexLi tt) tagInfos


tagIndexLi :: TagType -> TagInfo -> H.Html
tagIndexLi GeneralTag (TagInfo t c _) =
  H.li $ do
    H.a ! A.href (I.textValue $ renderUrl' $ tagPath t) $
      H.toHtml $ tagLabel' t
    " " :: H.Html
    H.toHtml $
      T.concat ["(",T.pack $ show c,")"]

tagIndexLi _ (TagInfo t c r) =
  H.li $ do
    H.header $
      H.a ! A.href (I.textValue $ renderUrl' $ tagPath t) $
        H.toHtml $ tagLabel' t
    H.div $
      Fo.forM_ (tagDescription t) $ \td ->
        H.toHtml td
    H.footer $ do
      H.div $
        H.toHtml $
          T.append (T.pack $ show c) " entries"
      Fo.forM_ r $ \(re,ru) ->
        H.div $ do
          H.preEscapedToHtml ("Most recent &mdash; " :: T.Text)
          H.a ! A.href (I.textValue $ renderUrl' ru) $
            H.toHtml $ entryTitle re
