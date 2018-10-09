{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Blog.App
import           Blog.Types
import           Data.Time.LocalTime
import           Dhall
import           Dhall.Pretty
import           Hakyll
import           System.IO
import qualified Data.Text                                 as T
import qualified Data.Text.Prettyprint.Doc                 as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PP

configPath :: T.Text
configPath = "./config/site-data.dhall"

main :: IO ()
main = do
    znow <- getZonedTime

    c@Config{..} <- input interpretConfig configPath
    let ?config = c

    PP.renderIO stdout
        . fmap annToAnsiStyle
        . PP.layoutSmart layoutOpts
        . prettyExpr
      =<< inputExpr configPath

    putStrLn ""

    hakyll $ app znow
