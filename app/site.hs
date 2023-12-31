{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Blog.App
import Blog.Types
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Zones
import Dhall
import Dhall.Pretty
import Hakyll
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Terminal as PP
import System.Environment
import System.FilePath
import System.IO

configPath :: T.Text
configPath = "./config/site-data.dhall"

main :: IO ()
main = do
  c <- input interpretConfig configPath
  let ?config = c

  workingDir <- fromMaybe "." <$> lookupEnv "HAKYLL_DIR"

  PP.renderIO stdout
    . fmap annToAnsiStyle
    . PP.layoutSmart layoutOpts
    . prettyExpr
    =<< inputExpr configPath

  putStrLn ""

  tz <- loadTZFromDB (T.unpack $ confEntryTZ ?config)
  hakyllWith
    ( defaultConfiguration
        { inMemoryCache = False,
          storeDirectory = workingDir </> "_cache",
          tmpDirectory = workingDir </> "_cache" </> "temp",
          providerDirectory = workingDir
        }
    )
    $ app tz
