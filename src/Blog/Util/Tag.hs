{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Blog.Util.Tag where

import Blog.Types
import Blog.Util
import Blog.View
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Char
import Data.List (sortBy)
import Data.Maybe
import Data.Ord
import Data.String
import qualified Data.Text as T
import Hakyll
import System.FilePath
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Pandoc as P

htmlDescription :: Tag -> Maybe H.Html
htmlDescription =
  fmap
    ( either (error . show) id
        . P.runPure
        . (P.writeHtml5 entryWriterOpts <=< P.readMarkdown entryReaderOpts)
    )
    . tagDescription

tagPrettyLabel :: Tag -> String
tagPrettyLabel t = f (tagPrettyLabelLower t)
  where
    f = case tagType t of
      GeneralTag -> id
      CategoryTag -> map toUpper
      SeriesTag -> id

tagPrettyLabelLower :: Tag -> String
tagPrettyLabelLower Tag {..} = c : T.unpack tagLabel
  where
    c = case tagType of
      GeneralTag -> '#'
      CategoryTag -> '@'
      SeriesTag -> '+'

tagLink ::
  (?config :: Config) =>
  (Tag -> String) ->
  Tag ->
  H.Html
tagLink f t =
  H.a
    ! A.href (H.textValue $ renderUrl (T.pack (tagUrl t)))
    ! A.class_ (tagLiClass t)
    ! A.title (fromString $ plainDescription' t)
    $ H.toHtml (f t)

fetchTag :: TagType -> T.Text -> Compiler Tag
fetchTag tt tl = loadSnapshotBody (fromFilePath turl) "tag"
  where
    turl = mkTagUrl tt $ T.unpack tl

tagSlug :: Tag -> T.Text
tagSlug = genSlug maxBound . tagLabel

tagTypeDescPath :: TagType -> String
tagTypeDescPath GeneralTag = "copy/tags/tags"
tagTypeDescPath CategoryTag = "copy/tags/categories"
tagTypeDescPath SeriesTag = "copy/tags/series"

mkTagUrl :: TagType -> String -> FilePath
mkTagUrl tt i = "entries" </> dir </> (p ++ genSlug' maxBound i) <.> "html"
  where
    p = case tt of
      GeneralTag -> ""
      CategoryTag -> "@"
      SeriesTag -> "+"
    dir = case tt of
      GeneralTag -> "tagged"
      CategoryTag -> "category"
      SeriesTag -> "series"

tagUrl :: Tag -> FilePath
tagUrl t = mkTagUrl (tagType t) (T.unpack (tagSlug t))

plainDescription :: Tag -> Maybe String
plainDescription t = either (error . show) (fmap T.unpack) . P.runPure . runMaybeT $ do
  Just desc <- return $ tagDescription t
  P.Pandoc m b <- lift $ P.readMarkdown entryReaderOpts desc
  b0 : _ <- return b
  lift $ P.writePlain entryWriterOpts (P.Pandoc m [b0])

plainDescription' :: Tag -> String
plainDescription' t = fromMaybe (tagPrettyLabel t) (plainDescription t)

tagLi ::
  (?config :: Config) =>
  Tag ->
  H.Html
tagLi t@Tag {..} =
  H.li
    $ H.a
      ! A.href (fromString (renderUrl' (tagUrl t)))
      ! A.class_ (tagLiClass t)
    $ H.toHtml
    $ tagPrettyLabel t

tagLiClass :: Tag -> H.AttributeValue
tagLiClass t = H.textValue $
  case tagType t of
    GeneralTag -> "tag-a-tag"
    CategoryTag -> "tag-a-category"
    SeriesTag -> "tag-a-series"

tagTypePrefix :: TagType -> T.Text
tagTypePrefix GeneralTag = "#"
tagTypePrefix CategoryTag = "@"
tagTypePrefix SeriesTag = "+"

filterTags :: TagType -> [Tag] -> [Tag]
filterTags tt = filter ((== tt) . tagType)

sortTags :: [Tag] -> [Tag]
sortTags =
  sortBy
    ( comparing tagType
        <> comparing (T.map toLower . tagLabel)
    )

data TagSortType
  = TSLabel
  | TSCount
  | TSRecent
  deriving (Show)

tsCompare ::
  TagSortType ->
  (Tag, Maybe Entry) ->
  (Tag, Maybe Entry) ->
  Ordering
tsCompare tt = ttComparer <> comparing (tagLabel . fst)
  where
    ttComparer =
      case tt of
        TSLabel -> \_ _ -> EQ
        TSCount -> flip $ comparing (length . tagEntries . fst)
        TSRecent -> flip $ comparing (fmap entryPostTime . snd)

teCompare ::
  TagType ->
  Entry ->
  Entry ->
  Ordering
teCompare = \case
  GeneralTag -> flip $ comparing entryPostTime
  CategoryTag -> flip $ comparing entryPostTime
  SeriesTag -> comparing entryPostTime
