module Development.Blog.Util (startupHelpers) where

import Development.Blog.Util.Compass
import Web.Blog.Database

startupHelpers :: IO ()
startupHelpers = do
    compileCompass
    runDB blogMigrate

