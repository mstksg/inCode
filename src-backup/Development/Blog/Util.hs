module Development.Blog.Util (startupHelpers, backupEntries) where

import "base" Prelude
import Config.SiteData
import Control.Monad                       (unless)
import Development.Blog.Util.BackupEntries
import Development.Blog.Util.Compass
import Development.Blog.Util.Fay
import Development.Blog.Util.LoadEntries
import Development.Blog.Util.LoadTags
import Web.Blog.Database
import Web.Blog.Types

entriesDir :: FilePath
entriesDir = "copy/entries"

tagsDir :: FilePath
tagsDir = "copy/tags"

fayDir :: (FilePath, FilePath)
fayDir = ("fay", "tmp/static/js")

startupHelpers :: IO ()
startupHelpers = do
    compileCompass
    unless (siteDataPrecompileFay siteData) (uncurry compileFay fayDir)
    runDB blogMigrate
    loadEntries entriesDir
    loadTags tagsDir

