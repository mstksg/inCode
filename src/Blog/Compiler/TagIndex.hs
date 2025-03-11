{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog.Compiler.TagIndex where

import Blog.Compiler.Entry
import Blog.Types
import Blog.Util
import Blog.Util.Tag
import Blog.View.TagIndex
import Data.Default
import Data.List (sortBy)
import Data.Maybe
import qualified Data.Text as T
import Data.Traversable
import Hakyll
import Hakyll.Web.Blaze

tagIndexCompiler ::
  (?config :: Config) =>
  TagType ->
  [(String, [Identifier])] ->
  Compiler (Item String)
tagIndexCompiler tt tmap = do
  tmap' <- forM tmap $ \(s, es) -> do
    t <- fetchTag tt (T.pack s)
    recent <-
      listToMaybe . sortEntries
        <$> traverse (flip loadSnapshotBody "entry") es
    return (t, recent)

  recents <- getRecentEntries
  wopts <- entryWriterOpts

  let sorter = indexSorter tt
      tmapSort = sortBy (tsCompare sorter) tmap'
      tii = TII tt tmapSort recents
      title = case tt of
        GeneralTag -> "Tags"
        CategoryTag -> "Categories"
        SeriesTag -> "Series List"
      pd =
        def
          { pageDataTitle = Just title,
            pageDataCss = ["/css/page/archive.css"]
          }

  blazeCompiler pd (viewTagIndex wopts tii)

indexSorter :: TagType -> TagSortType
indexSorter GeneralTag = TSCount
indexSorter CategoryTag = TSCount -- should this be Label, Recent?
indexSorter SeriesTag = TSRecent
