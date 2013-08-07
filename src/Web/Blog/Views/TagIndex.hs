{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.TagIndex (
    viewTagIndex
  ) where

-- import Data.Monoid
-- import Web.Blog.Render
-- import Web.Blog.SiteData
-- import qualified Data.Map                 as M
import Control.Applicative                   ((<$>))
import Control.Monad.Reader
import Data.Maybe                            (fromMaybe, isJust, fromJust)
import Text.Blaze.Html5                      ((!))
import Web.Blog.Models
import Web.Blog.Models.Types
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Util
import Web.Blog.Views.Archive
import qualified Data.Text                   as T
import qualified Data.Traversable  as Tr     (mapM)
import qualified Database.Persist.Postgresql as D
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as I

viewTagIndex :: [(Tag,(T.Text,Int))] -> TagType -> SiteRender H.Html
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
          GeneralTag -> "Tags"
          CategoryTag -> "Categories"
          SeriesTag -> "Series"

      nav

    H.ul $
      forM_ tagInfos $ \tagInfo -> do
        let
          (t,(url,count)) = tagInfo

        H.li $ do
          H.div $
            H.a ! A.href (I.textValue url) $
              H.toHtml $ tagLabel' t
          H.div $
            H.toHtml $
              T.concat ["(",T.pack $ show count," entries)"]

