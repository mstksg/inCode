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
      GeneralTag -> "tag-index tile tag-list"
      CategoryTag -> "category-index"
      SeriesTag -> "series-index tile"

  sidebarHtml <- viewArchiveSidebar $ Just $
    case tt of
      GeneralTag  -> ViewArchiveIndexTag
      CategoryTag -> ViewArchiveIndexCategory
      SeriesTag   -> ViewArchiveIndexSeries

  noTagsCopy <-
    if null tagInfos
      then
        renderRawCopy "copy/static/empty-taglist.md"
      else
        return mempty

  return $ do
    H.div ! A.class_ "archive-sidebar unit one-of-four" $
      sidebarHtml

    H.section ! A.class_ "archive-section unit three-of-four" ! mainSection $ do

      H.header ! A.class_ "tile" $

        H.h1 $
          case tt of
            GeneralTag  -> "Tags"
            CategoryTag -> "Categories"
            SeriesTag   -> "Series"

      H.ul ! A.class_ ulClass $
        if null tagInfos
          then
            noTagsCopy
          else
            mapM_ (tagIndexLi tt) tagInfos


tagIndexLi :: TagType -> TagInfo -> H.Html
tagIndexLi GeneralTag (TagInfo t c _) =
  H.li $
    H.a ! A.href (I.textValue $ renderUrl' $ tagPath t) ! A.class_ "tag-a-tag" $ do
      H.toHtml $ tagLabel' t
      " (" :: H.Html
      H.toHtml c
      ")" :: H.Html

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
          H.span ! A.class_ "recent-link" $ do
            H.preEscapedToHtml ("Most recent &mdash; " :: T.Text)
            H.a ! A.href (I.textValue $ renderUrl' ru) $
              H.toHtml $ entryTitle re
          Fo.forM_ (entryPostedAt re) $ \rt ->
            H.span ! A.class_ "recent-time" $ do
              " (" :: H.Html
              H.time
                ! A.datetime (I.textValue $ T.pack $ renderDatetimeTime rt)
                ! A.pubdate ""
                ! A.class_ "pubdate"
                $ H.toHtml $ renderShortFriendlyTime rt
              ")" :: H.Html
  where
    liClass = case tt of
      CategoryTag -> "tile"
      _           -> ""
