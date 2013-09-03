module Development.Blog.Util (startupHelpers) where

import Development.Blog.Util.Compass
import Development.Blog.Util.LoadEntries
import Web.Blog.Database

entriesDir :: FilePath
entriesDir = "copy/entries"

startupHelpers :: IO ()
startupHelpers = do
    compileCompass
    runDB blogMigrate
    loadEntries entriesDir

