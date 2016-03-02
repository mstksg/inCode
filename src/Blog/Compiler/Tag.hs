{-# LANGUAGE RecordWildCards #-}

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
import qualified Data.Text       as T
import qualified Text.Pandoc     as P

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
    unsafeCompiler . putStrLn $ tagTypeDescPath tt </> genSlug' maxBound tLab <.> "md"
    unsafeCompiler $ print tDesc
    tDescP <- mapM (readPandocWith entryReaderOpts) tDesc

    let tLab'   = T.pack tLab
        tDescP' = flip fmap tDescP $ \pd ->
                    case itemBody pd of
                      P.Pandoc m (P.Header 1 _ _:bs)
                          -> P.Pandoc m bs
                      p   -> p
        tDescMd = T.pack . P.writeMarkdown entryWriterOpts <$> tDescP'

    entries <- map itemBody <$> loadAllSnapshots p "entry"

    makeItem $ Tag { tagLabel       = tLab'
                   , tagType        = tt
                   , tagDescription = tDescMd
                   , tagSlug        = genSlug maxBound tLab'
                   , tagEntries     = entries
                   }


-- data Tag = Tag
--     { tagLabel       :: !T.Text
--     , tagType        :: !TagType
--     , tagDescription :: !(Maybe T.Text)
--     , tagSlug        :: !T.Text
--     , tagEntries     :: [Entry]
--     }
--   deriving (Show, Generic, Typeable)


tagTypeDescPath :: TagType -> String
tagTypeDescPath GeneralTag  = "copy/tags/tags"
tagTypeDescPath CategoryTag = "copy/tags/categories"
tagTypeDescPath SeriesTag   = "copy/tags/series"
