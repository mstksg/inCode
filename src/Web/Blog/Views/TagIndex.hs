{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.TagIndex (
    viewTagIndex
  ) where

import Data.Monoid                           (mempty)
import Text.Blaze.Html5                      ((!))
import Web.Blog.Models
import Web.Blog.Models.Types
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Util
import Web.Blog.Views.Archive
import qualified Data.Foldable as Fo         (forM_)
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as I

viewTagIndex :: [TagInfo] -> TagType -> SiteRender H.Html
viewTagIndex tagInfos tt = do
  let
    ulClass = case tt of
      GeneralTag -> "tag-list tile"
      CategoryTag -> "category-list"
      SeriesTag -> "series-list tile"


  nav <- viewArchiveNav $ Just $
    case tt of
      GeneralTag  -> ViewArchiveIndexTag
      CategoryTag -> ViewArchiveIndexCategory
      SeriesTag   -> ViewArchiveIndexSeries

  return $ 
    H.section $ do

      H.header ! A.class_ "tile" $ do

        H.nav 
          nav

        H.div ! A.class_ "clear" $ mempty

        H.h1 $
          case tt of
            GeneralTag  -> "Tags"
            CategoryTag -> "Categories"
            SeriesTag   -> "Series"

      H.ul ! A.class_ ulClass $
        mapM_ (tagIndexLi tt) tagInfos


tagIndexLi :: TagType -> TagInfo -> H.Html
tagIndexLi GeneralTag (TagInfo t c _) =
  H.li $ do
    H.a ! A.href (I.textValue $ renderUrl' $ tagPath t) $
      H.toHtml $ tagLabel' t
    " " :: H.Html
    H.toHtml $
      T.concat ["(",T.pack $ show c,")"]

tagIndexLi tt (TagInfo t c r) =
  H.li ! A.class_ liClass $ do
    H.header $ do
      H.h2 $
        H.a ! A.href (I.textValue $ renderUrl' $ tagPath t) $
          H.toHtml $ tagLabel' t
      H.div ! A.class_ "tag-entry-count" $
        H.toHtml $
          case tt of
            CategoryTag -> T.concat ["> ",T.pack $ show c," entries"]
            SeriesTag   -> T.concat ["(",T.pack $ show c," entries)"]
            _           -> mempty

    H.div ! A.class_ "tag-description" $
      Fo.forM_ (tagDescHtml t) id
    H.footer $
      Fo.forM_ r $ \(re,ru) ->
        H.div $ do
          H.preEscapedToHtml ("Most recent &mdash; " :: T.Text)
          H.a ! A.href (I.textValue $ renderUrl' ru) $
            H.toHtml $ entryTitle re
          " (" :: H.Html
          H.time
            ! A.datetime (I.textValue $ T.pack $ renderDatetimeTime $ entryPostedAt re)
            ! A.pubdate "" 
            ! A.class_ "pubdate"
            $ H.toHtml $ renderShortFriendlyTime $ entryPostedAt re
          ")" :: H.Html
  where
    liClass = case tt of
      CategoryTag -> "tile"
      _           -> ""
