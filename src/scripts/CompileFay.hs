module Main (main) where

import Fay
import "base" Prelude
import Data.Default

main :: IO ()
main = compileFromTo def "fay/Blog/Entry.hs" (Just "tmp/entry.js")
