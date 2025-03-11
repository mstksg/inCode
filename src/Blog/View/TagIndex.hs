{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Blog.View.TagIndex where

import Blog.Types
import Blog.Util
import Blog.Util.Tag
import Blog.View
import Blog.View.Archive
import Control.Monad
import Data.String
import qualified Data.Text as T
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Pandoc as P

data TagIndexInfo = TII
  { tiiType :: TagType,
    tiiTags :: [(Tag, Maybe Entry)],
    tiiRecents :: [Entry]
  }

viewTagIndex :: (?config :: Config) => P.WriterOptions -> TagIndexInfo -> H.Html
viewTagIndex wopts TII {..} = do
  H.div ! A.class_ "archive-sidebar unit one-of-four" $
    viewArchiveSidebar tiiRecents (Just (AIndTagged tiiType))

  H.section ! A.class_ "archive-section unit three-of-four" ! mainSection $ do
    H.header ! A.class_ "tile" $
      H.h1 $
        case tiiType of
          GeneralTag -> "Tags"
          CategoryTag -> "Categories"
          SeriesTag -> "Series"

    H.ul ! A.class_ ulClass $
      if null tiiTags
        then "No entries yet for this tag!"
        else mapM_ (uncurry (tagIndexLi wopts tiiType)) tiiTags
  where
    ulClass = case tiiType of
      GeneralTag -> "tag-index tile tag-list"
      CategoryTag -> "category-index"
      SeriesTag -> "series-index tile"

tagIndexLi ::
  (?config :: Config) =>
  P.WriterOptions ->
  TagType ->
  Tag ->
  Maybe Entry ->
  H.Html
tagIndexLi wopts tt t@Tag {..} recent =
  H.li ! A.class_ liClass $
    case tt of
      GeneralTag ->
        H.a
          ! A.href (H.textValue $ renderUrl (T.pack (tagUrl t)))
          ! A.class_ "tag-a-tag"
          $ do
            H.toHtml (tagPrettyLabel t)
            H.preEscapedToHtml ("&nbsp;" :: T.Text)
            "(" :: H.Html
            H.toHtml (show (length tagEntries))
            ")" :: H.Html
      _ -> do
        H.header $ do
          H.h2 $
            H.a ! A.href (H.textValue $ renderUrl (T.pack (tagUrl t))) $
              H.toHtml (tagPrettyLabel t)
          H.div ! A.class_ "tag-entry-count" $
            H.toHtml $
              case tt of
                CategoryTag -> "> " ++ show (length tagEntries) ++ " entries"
                SeriesTag -> "(" ++ show (length tagEntries) ++ " entries)"

        H.div ! A.class_ "tag-description" $
          sequence_ (htmlDescription wopts t)

        H.footer $
          forM_ recent $ \Entry {..} -> do
            H.div $ do
              H.span ! A.class_ "recent-link" $ do
                H.preEscapedToHtml ("Most recent &mdash; " :: T.Text)
                H.a ! A.href (fromString $ renderUrl' entryCanonical) $
                  H.toHtml entryTitle
              forM_ entryPostTime $ \posted ->
                H.span ! A.class_ "recent-time" $ do
                  "(" :: H.Html
                  H.time
                    ! A.datetime (H.textValue $ T.pack $ renderDatetimeTime posted)
                    ! A.pubdate ""
                    ! A.class_ "pubdate"
                    $ H.toHtml (renderShortFriendlyTime posted)
                  ")" :: H.Html
  where
    liClass = case tt of
      CategoryTag -> "tile"
      _ -> ""
