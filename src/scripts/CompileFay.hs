module Main (main) where

import "base" Prelude
import Development.Blog.Util.Fay

fayDir :: FilePath
fayDir = "fay"

main :: IO ()
main = compileFay fayDir
