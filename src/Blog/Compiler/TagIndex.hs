{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog.Compiler.TagIndex where

import           Blog.Compiler.Entry
import           Blog.Types
import           Blog.Util
import           Blog.Util.Tag
import           Blog.View
import           Blog.View.TagIndex
import           Data.Bifunctor
import           Data.Default
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Time.LocalTime
import           Data.Traversable
import           Hakyll
import           Hakyll.Web.Blaze
import           System.FilePath
import           Text.Read           (readMaybe)
import qualified Data.Text           as T
import qualified Text.Pandoc         as P
import qualified Text.Pandoc.Error   as P
import qualified Text.Pandoc.Walk    as P


tagIndexCompiler
    :: (?config :: Config)
    => TagType
    -> [(String, [Identifier])]
    -> [Identifier]
    -> Compiler (Item String)
tagIndexCompiler tt tmap recents = do
    tmap' <- forM tmap $ \(s, es) -> do
      t      <- fetchTag tt (T.pack s)
      recent <- listToMaybe . sortEntries
            <$> traverse (flip loadSnapshotBody "entry") es
      return (t, recent)

    recents' <- traverse (flip loadSnapshotBody "entry") recents

    let tmapSort = sortBy f tmap'
        tii = TII tt tmapSort recents'
        title = case tt of
                  GeneralTag  -> "Tags"
                  CategoryTag -> "Categories"
                  SeriesTag   -> "Series List"
        pd    = def { pageDataTitle = Just title
                    , pageDataCss   = ["/css/page/archive.css"]
                    }

    blazeCompiler pd (viewTagIndex tii)
  where
    f = case tt of
          GeneralTag  -> flip $ comparing (length . tagEntries . fst)
          CategoryTag -> comparing (tagLabel . fst)
          SeriesTag   -> comparing (fmap entryPostTime . snd)

