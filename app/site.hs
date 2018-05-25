{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

import           Blog.App
import           Blog.Types
import           Control.Exception
import           Data.Time.LocalTime
import           Hakyll
import qualified Data.Text.Encoding  as T
import qualified Data.Text.IO        as T
import qualified Data.Yaml           as Y
import qualified Data.Yaml.Pretty    as Y

main :: IO ()
main = do
    znow <- getZonedTime

    c@Config{..} <- either throwIO return
                =<< Y.decodeFileEither "config/site-data.yaml"
    let ?config = c

    T.putStrLn $ T.decodeUtf8 (Y.encodePretty Y.defConfig c)

    hakyll $ app znow
