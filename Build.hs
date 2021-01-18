#!/usr/bin/env stack
-- stack --install-ghc runghc --package shake --package jle-utils --package lens

import           Control.Lens hiding        ((<.>))
import           Control.Lens.Cons
import           Control.Monad.IO.Class
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.String
import           Development.Shake
import           Development.Shake.FilePath
import           JUtils.GHPages
import qualified Data.Text                  as T

opts = shakeOptions { shakeFiles     = "_build"
                    , shakeVersion   = "1.0"
                    , shakeVerbosity = Loud
                    , shakeThreads   = 0
                    }

psReq :: FilePath -> FilePath
psReq f = "_purescript" </> (f <.> "js")

psExes :: [String]
psExes = ["entry","gol"]

main :: IO ()
main = shakeArgs opts $ do
    want ["build"]

    "build" ~> do
      need (psReq <$> psExes)
      unit $ cmd "stack run -- blog-build" "build"
      liftIO $ updatePages "_site" Nothing Nothing

    "rebuild" ~> do
      need ["purescript"]
      unit $ cmd "stack run -- blog-build" "rebuild"
      liftIO $ updatePages "_site" Nothing Nothing

    "bower_components/.installed" %> \t -> do
      need ["bower.json"]
      unit $ cmd "bower" "install"
      cmd "touch" t

    "purescript" ~>
      need (psReq <$> psExes)

    "_purescript/*.js" %> \out -> do
      let exName = over _head toUpper $ takeBaseName out
      need [ "bower_components/.installed"
           , "app-purescript" </> exName <.> "purs"
           ]
      unit $ cmd "pulp build"
                 "--main" exName
                 "--src-path" "app-purescript"
                 "--to" out

    "clean" ~> do
      removeFilesAfter "_build" ["//*"]
      unit $ cmd "bower cache" "clean"
      removeFilesAfter "bower_components" ["//*"]
      removeFilesAfter "_purescript" ["//*"]
      unit $ cmd "stack run -- blog-build" "clean"
