{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog.Compiler.TagIndex where

import           Blog.Compiler.Entry
import           Blog.Types
import           Blog.Util.Tag
import           Blog.View.TagIndex
import           Data.Default
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Traversable
import           Hakyll
import           Hakyll.Web.Blaze
import qualified Data.Text              as T


tagIndexCompiler
    :: (?config :: Config)
    => TagType
    -> [(String, [Identifier])]
    -> Compiler (Item String)
tagIndexCompiler tt tmap = do
    tmap' <- forM tmap $ \(s, es) -> do
      t      <- fetchTag tt (T.pack s)
      recent <- listToMaybe . sortEntries
            <$> traverse (flip loadSnapshotBody "entry") es
      return (t, recent)

    recents <- getRecentEntries

    let sorter  = indexSorter tt
        tmapSort = sortBy (tsCompare sorter) tmap'
        tii = TII tt tmapSort recents
        title = case tt of
                  GeneralTag  -> "Tags"
                  CategoryTag -> "Categories"
                  SeriesTag   -> "Series List"
        pd    = def { pageDataTitle = Just title
                    , pageDataCss   = ["/css/page/archive.css"]
                    }

    blazeCompiler pd (viewTagIndex tii)


data TagSortType = TSLabel
                 | TSCount
                 | TSRecent
  deriving Show

tsCompare
    :: TagSortType
    -> (Tag, Maybe Entry)
    -> (Tag, Maybe Entry)
    -> Ordering
tsCompare TSLabel  =        comparing (tagLabel . fst)
tsCompare TSCount  = flip $ comparing (length . tagEntries . fst)
tsCompare TSRecent = flip $ comparing (fmap entryPostTime . snd)

indexSorter :: TagType -> TagSortType
indexSorter GeneralTag  = TSCount
indexSorter CategoryTag = TSCount   -- should this be Label, Recent?
indexSorter SeriesTag   = TSRecent
