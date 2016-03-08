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

    let tmapSort = sortBy f tmap'
        tii = TII tt tmapSort recents
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

