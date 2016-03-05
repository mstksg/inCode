{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blog.Compiler.Tag where

import           Blog.Types
import           Blog.Util
import           Blog.View
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

tagCompiler :: TagType -> String -> Pattern -> Compiler (Item String)
tagCompiler tt tLab p = do
    Tag{..} <- fmap itemBody . saveSnapshot "tag" =<< compileTag tt tLab p

    let sorted = sortBy (flip $ comparing entryPostTime)
               . filter (isJust . entryPostTime)
               $ tagEntries

        d = T.unpack <$> maybeToList tagDescription

    makeItem . unlines $ d ++ map (T.unpack . entryTitle) sorted

compileTag :: TagType -> String -> Pattern -> Compiler (Item Tag)
compileTag tt tLab p = do
    tDesc <- fmap listToMaybe
           . loadAll
           . fromString
           $ tagTypeDescPath tt </> genSlug' maxBound tLab <.> "md"
    tDescP <- mapM (readPandocWith entryReaderOpts) tDesc

    let tLab'   = T.pack tLab
        tDescP' = flip fmap tDescP $ \pd ->
                    case itemBody pd of
                      P.Pandoc m (P.Header 1 _ _:bs)
                          -> P.Pandoc m bs
                      pd' -> pd'
        tDescMd = T.pack . P.writeMarkdown entryWriterOpts <$> tDescP'

    entries <- map itemBody <$> loadAllSnapshots p "entry"

    makeItem $ Tag { tagLabel       = tLab'
                   , tagType        = tt
                   , tagDescription = tDescMd
                   , tagEntries     = entries
                   }

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

tagPrettyLabel :: Tag -> String
tagPrettyLabel Tag{..} = c : T.unpack tagLabel
  where
    c = case tagType of
          GeneralTag  -> '#'
          CategoryTag -> '@'
          SeriesTag   -> '+'


plainDescription :: Tag -> Maybe String
plainDescription t = do
    desc         <- T.unpack <$> tagDescription t
    P.Pandoc m b <- eToM $ P.readMarkdown entryReaderOpts desc
    b0           <- listToMaybe b
    return $ P.writePlain entryWriterOpts (P.Pandoc m [b0])
  where
    eToM = either (const Nothing) Just

plainDescription' :: Tag -> String
plainDescription' t = fromMaybe (tagPrettyLabel t) (plainDescription t)

filterTags :: TagType -> [Tag] -> [Tag]
filterTags tt = filter ((== tt) . tagType)

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

tagLink
    :: (?config :: Config)
    => (Tag -> String)
    -> Tag
    -> H.Html
tagLink f t =
  H.a
    ! A.href (H.textValue $ renderUrl (T.pack (tagUrl t)))
    ! A.title (fromString $ plainDescription' t)
    $ H.toHtml (f t)

