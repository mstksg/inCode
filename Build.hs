#!/usr/bin/env stack
-- stack --install-ghc runghc --package shake --package jle-utils --package lens

import Control.Lens hiding        ((<.>))
import Control.Lens.Cons
import Control.Monad.IO.Class
import Data.Char
import Data.Foldable
import Data.List
import Data.String
import Development.Shake
import Development.Shake.FilePath
import JUtils.GHPages

opts = shakeOptions { shakeFiles     = "_build"
                    , shakeVersion   = "1.0"
                    , shakeVerbosity = Loud
                    , shakeThreads   = 0
                    }

psReq :: FilePath -> FilePath
psReq f = "_purescript" </> (f <.> "js")

psExes :: [String]
psExes = ["entry"]

main :: IO ()
main = do
    shakeArgs opts $ do
      want ["build"]

      "build" ~> do
        need (psReq <$> psExes)
        unit $ cmd "stack run -- blog-build" "build"
        liftIO $ updatePages "_site" Nothing

      "rebuild" ~> do
        need ["purescript"]
        unit $ cmd "stack run -- blog-build" "rebuild"
        liftIO $ updatePages "_site" Nothing

      "purescript" ~>
        need (psReq <$> psExes)

      "_purescript/*.js" %> \out -> do
        let exName = over _head toUpper $ takeBaseName out
        need ["app-purescript" </> exName <.> "purs"]
        unit $ cmd "pulp build"
                   "--main" exName
                   "--src-path" "app-purescript"
                   "--to" out

      "clean" ~> do
        removeFilesAfter "_build" ["//*"]
        removeFilesAfter "_purescript" ["//*"]
        unit $ cmd "stack run -- blog-build" "clean"
