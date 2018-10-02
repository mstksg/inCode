{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

import           Blog.App
import           Blog.Types
import           Control.Exception
import           Control.Monad
import           Data.Time.LocalTime
import           Dhall
import           Hakyll
import           System.Exit
import qualified Data.Text.Encoding  as T
import qualified Data.Text.IO        as T
import qualified Data.Yaml           as Y
import qualified Data.Yaml.Pretty    as Y

main :: IO ()
main = do
    znow <- getZonedTime

    c@Config{..} <- input interpretConfig "./config/site-data.dhall"
    -- c2           <-       either throwIO return
    --             =<< Y.decodeFileEither "config/site-data.yaml"
    -- unless (c == c2) $ do
    --   putStrLn "config do not match"
    --   print c
    --   print c2
    --   exitFailure
    let ?config = c

    T.putStrLn $ T.decodeUtf8 (Y.encodePretty Y.defConfig c)

    hakyll $ app znow
