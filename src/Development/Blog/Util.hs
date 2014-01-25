module Development.Blog.Util (startupHelpers, backupEntries) where

import "base" Prelude
import Development.Blog.Util.BackupEntries
import Development.Blog.Util.Compass
import Development.Blog.Util.Fay
import Development.Blog.Util.LoadEntries
import Development.Blog.Util.LoadTags
import Web.Blog.Database

entriesDir :: FilePath
entriesDir = "copy/entries"

tagsDir :: FilePath
tagsDir = "copy/tags"

fayDir :: FilePath
fayDir = "fay"

startupHelpers :: IO ()
startupHelpers = do
    compileCompass
    compileFay fayDir
    runDB blogMigrate
    loadEntries entriesDir
    loadTags tagsDir

