module Main (main) where

import "base" Prelude
import Development.Blog.Util.LoadEntries

entriesDir :: FilePath
entriesDir = "copy/entries"

main :: IO ()
main = loadEntries entriesDir
