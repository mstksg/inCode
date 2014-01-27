module Development.Blog.Util.Fay (compileFay) where

import "base" Prelude
import Config.SiteData
import Control.Monad
import Data.Maybe (fromJust, isJust)
import Data.Default
import Data.List
import Fay
import System.Directory
import System.FilePath
import Web.Blog.Types

outputBase :: FilePath
outputBase = "tmp/static/js"

fayPackages :: [String]
fayPackages = [ "fay-text 0.3.0.1"
              , "fay-jquery 0.6.0.2"
              , "fay-base 0.19"
              , "fay-ref 0.1.0.0"
              ]

includePackage :: String -> (Maybe String, FilePath)
includePackage package = (packageName, sharePath)
  where
    pname:pversion:_ = words package
    packageName = Just pname
    sharePath = fromJust (siteDataShareLibs siteData)
                  </> (pname ++ "-" ++ pversion)
                  </> "src"


fayConfig :: CompileConfig
fayConfig = def
  { configPrettyPrint = False
  , configOptimize = True
  -- , configPackages = ["fay-text","fay-jquery","fay-base","fay-ref"]
  , configPackageConf = siteDataPackageConf siteData
  , configDirectoryIncludes = map includePackage fayPackages
  , configExportRuntime = False
  , configRuntimePath = Just runtimePath
  }
  where
    runtimePath = "static/js/fay-runtime.min.js"

compileFay :: FilePath -> IO ()
compileFay fayBase =  when (isJust (siteDataPackageConf siteData))
                        (compileFayDir fayBase "")

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



