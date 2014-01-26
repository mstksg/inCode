module Development.Blog.Util.Fay (compileFay) where

import "base" Prelude
import Control.Monad
import Data.Default
import Data.List
import Fay
import Fay.Types
import System.Directory
import System.FilePath

outputBase :: FilePath
outputBase = "tmp/static/js"

fayConfig :: CompileConfig
fayConfig = def
  { configPrettyPrint = False
  -- , configOptimize = True
  , configPackages = ["fay-text","fay-jquery","fay-base","fay-ref"]
  , configPackageConf = Just ".cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d"
  -- , configDirectoryIncludes = [(Just "fay-text",".cabal-sandbox/share/x86_64-linux-ghc-7.6.3/fay-text-0.3.0.1")]
  , configDirectoryIncludes = [(Nothing,".cabal-sandbox/share/x86_64-linux-ghc-7.6.3/")]
  -- , configExportRuntime = False
  -- , configRuntimePath = Just runtimePath
  }
  where
    runtimePath = "static/js/fay-runtime.min.js"

compileFay :: FilePath -> IO ()
compileFay fayBase = compileFayDir fayBase ""

compileFayDir :: FilePath -> FilePath -> IO ()
compileFayDir fayBase fayDir = do
  let
    fullPath = fayBase </> fayDir
    outPath = outputBase </> fayDir

  createDirectoryIfMissing True outPath

  fayFiles <- getDirectoryContents fullPath

  forM_ fayFiles $ \ff ->
    unless ("." `isPrefixOf` ff) $ do
      let
        fullFF = fullPath </> ff
      isDir <- doesDirectoryExist fullFF
      if isDir
        then compileFayDir fayBase (fayDir </> ff)
        else do
          let
            fullOut = toJsName $ outPath </> ff
          putStrLn $ concat ["Compiling ",fullFF," to ",fullOut,"..."]
          compileFromTo
            fayConfig
            fullFF
            (Just fullOut)



