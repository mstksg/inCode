{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blog.Util.Tag where

import           Blog.Types
import           Blog.Util
import           Blog.View
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.String
import           Hakyll
import           System.FilePath
import           Text.Blaze.Html5            ((!))
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Pandoc                 as P
import qualified Text.Pandoc.Error           as P


htmlDescription :: Tag -> Maybe H.Html
htmlDescription = fmap ( P.writeHtml entryWriterOpts
                       . P.handleError
                       . P.readMarkdown entryReaderOpts
                       . T.unpack
                       )
                . tagDescription


tagPrettyLabel :: Tag -> String
tagPrettyLabel t = f (tagPrettyLabelLower t)
  where
    f = case tagType t of
          GeneralTag  -> id
          CategoryTag -> map toUpper
          SeriesTag   -> id

tagPrettyLabelLower :: Tag -> String
tagPrettyLabelLower Tag{..} = c : T.unpack tagLabel
  where
    c = case tagType of
          GeneralTag  -> '#'
          CategoryTag -> '@'
          SeriesTag   -> '+'

tagLink
    :: (?config :: Config)
    => (Tag -> String)
    -> Tag
    -> H.Html
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
tagTypeDescPath GeneralTag  = "copy/tags/tags"
tagTypeDescPath CategoryTag = "copy/tags/categories"
tagTypeDescPath SeriesTag   = "copy/tags/series"

mkTagUrl :: TagType -> String -> FilePath
mkTagUrl tt i = "entries" </> dir </> (p ++ genSlug' maxBound i)
  where
    p   = case tt of
            GeneralTag  -> ""
            CategoryTag -> "@"
            SeriesTag   -> "+"
    dir = case tt of
            GeneralTag  -> "tagged"
            CategoryTag -> "category"
            SeriesTag   -> "series"

tagUrl :: Tag -> FilePath
tagUrl t = mkTagUrl (tagType t) (T.unpack (tagSlug t))

plainDescription :: Tag -> Maybe String
plainDescription t = do
    desc         <- T.unpack <$> tagDescription t
    let P.Pandoc m b = P.handleError $ P.readMarkdown entryReaderOpts desc
    b0           <- listToMaybe b
    return $ P.writePlain entryWriterOpts (P.Pandoc m [b0])

plainDescription' :: Tag -> String
plainDescription' t = fromMaybe (tagPrettyLabel t) (plainDescription t)

tagLi
    :: (?config :: Config)
    => Tag
    -> H.Html
tagLi t@Tag{..} = H.li $
  H.a
    ! A.href (fromString (renderUrl' (tagUrl t)))
    ! A.class_ (tagLiClass t) $
      H.toHtml $ tagPrettyLabel t

tagLiClass :: Tag -> H.AttributeValue
tagLiClass t = H.textValue $
  case tagType t of
    GeneralTag  -> "tag-a-tag"
    CategoryTag -> "tag-a-category"
    SeriesTag   -> "tag-a-series"


tagTypePrefix :: TagType -> T.Text
tagTypePrefix GeneralTag  = "#"
tagTypePrefix CategoryTag = "@"
tagTypePrefix SeriesTag   = "+"

filterTags :: TagType -> [Tag] -> [Tag]
filterTags tt = filter ((== tt) . tagType)

sortTags :: [Tag] -> [Tag]
sortTags = sortBy (comparing tagPrettyLabelLower)
