{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Blog.App
import           Blog.Types
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict
import           Data.Time.LocalTime
import           Dhall
import           Dhall.Pretty
import           Hakyll
import           System.Exit
import           System.IO
import qualified Data.Text                                 as T
import qualified Data.Text.Encoding                        as T
import qualified Data.Text.IO                              as T
import qualified Data.Text.Lazy                            as TL
import qualified Data.Text.Lazy.IO                         as TL
import qualified Data.Text.Prettyprint.Doc                 as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PP
import qualified Data.Yaml                                 as Y
import qualified Data.Yaml.Pretty                          as Y

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
