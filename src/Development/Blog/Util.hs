module Development.Blog.Util (startupHelpers, backupEntries) where

import Development.Blog.Util.BackupEntries
import Development.Blog.Util.Compass
import Development.Blog.Util.LoadEntries
import Development.Blog.Util.LoadTags
import Web.Blog.Database

entriesDir :: FilePath
entriesDir = "copy/entries"

tagsDir :: FilePath
tagsDir = "copy/tags"

startupHelpers :: IO ()
startupHelpers = do
    compileCompass
    runDB blogMigrate
    loadEntries entriesDir
    loadTags tagsDir

