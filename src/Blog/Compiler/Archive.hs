{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog.Compiler.Archive where

import           Blog.Compiler.Entry
import           Blog.Types
import           Blog.View.Archive
import           Data.Default
import           Hakyll
import           Hakyll.Web.Blaze
import qualified Data.Text           as T

archiveCompiler
    :: (?config :: Config)
    => ArchiveData Identifier
    -> Compiler (Item String)
archiveCompiler ad = do
    ad'     <- traverse ((compileTE =<<) . flip loadSnapshotBody "entry") ad
    recents <- getRecentEntries
    let title = T.pack (archiveTitle ad')
        ai    = AI ad' recents
        pd    = def { pageDataTitle = Just title
                    , pageDataCss   = ["/css/page/archive.css"]
                    , pageDataJs    = ["/js/disqus_count.js"]
                    }
    blazeCompiler pd (viewArchive ai)

