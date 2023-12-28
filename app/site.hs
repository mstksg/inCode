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
import qualified Data.Text                     as T
import qualified Prettyprinter                 as PP
import qualified Prettyprinter.Render.Terminal as PP

configPath :: T.Text
configPath = "./config/site-data.dhall"

main :: IO ()
main = do
    znow <- getZonedTime

    c <- input interpretConfig configPath
    let ?config = c

    PP.renderIO stdout
        . fmap annToAnsiStyle
        . PP.layoutSmart layoutOpts
        . prettyExpr
      =<< inputExpr configPath

    putStrLn ""

    hakyllWith (defaultConfiguration { inMemoryCache = False}) $ app znow
