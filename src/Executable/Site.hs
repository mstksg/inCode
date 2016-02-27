{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}

import Blog.Types
import Control.Exception
import Data.Default
import Data.Foldable
import Data.List
import Hakyll
import Hakyll.Web.Redirect
import Hakyll.Web.Sass
import System.FilePath
import Text.Jasmine
import Text.Sass
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Yaml               as Y

main :: IO ()
main = do
    c@Config{..} <- either throwIO return
                =<< Y.decodeFileEither "config/site-data.yaml"

    hakyll $ do
      match "static/**" $ do
        route   $ gsubRoute "static/" (\_ -> "")
        compile copyFileCompiler

      match "css/**" $ do
        route   idRoute
        compile compressCssCompiler

      match "scss/**" $ do
        route   $ gsubRoute "scss" (\_ -> "css")
        compile $ sassCompilerWith def{ sassIncludePaths = Just ["scss"] }

      match "js/**" $ do
        route   idRoute
        compile compressJsCompiler

      forM_ confBlobs $ \b -> do
        match "code-samples/**" $ do
          route   $ gsubRoute ".hs" (\_ -> ".hs.html")
          let f p | ".hs.html" `isSuffixOf` p = dropEnd 5 p
                  | otherwise                 = p
          compile $ redirectCompiler (T.pack . (T.unpack b </>) . f . toFilePath)

compressJsCompiler :: Compiler (Item String)
compressJsCompiler = fmap f <$> getResourceString
  where
    f :: String -> String
    f = TL.unpack . TL.decodeUtf8 . minify . TL.encodeUtf8 . TL.pack

dropEnd :: Int -> [a] -> [a]
dropEnd i xs = zipWith const xs (drop i xs)
