module Main (main) where

import "base" Prelude
import Development.Blog.Util.Fay
import Web.Blog.Types
import Config.SiteData

fayDir :: FilePath
fayDir = "fay"

outDir :: FilePath
outDir = if siteDataPrecompileFay siteData
            then "static/js"
            else "tmp/static/js"

main :: IO ()
main = compileFay fayDir outDir
