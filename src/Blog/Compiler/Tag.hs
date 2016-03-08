{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blog.Compiler.Tag where

import           Blog.Compiler.Archive
import           Blog.Types
import           Blog.Util
import           Blog.Util.Tag
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.String
import           Hakyll
import           System.FilePath
import qualified Data.Text             as T
import qualified Text.Pandoc           as P

tagCompiler
    :: (?config :: Config)
    => TagType
    -> String
    -> Pattern
    -> Compiler (Item String)
tagCompiler tt tLab p = do
    t@Tag{..} <- fmap itemBody . saveSnapshot "tag" =<< compileTag tt tLab p

    let sorted = map (fromFilePath . entrySourceFile)
               . sortBy (flip $ comparing entryPostTime)
               . filter (isJust . entryPostTime)
               $ tagEntries

    archiveCompiler (ADTagged t sorted)

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

