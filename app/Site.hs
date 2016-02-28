{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}

import           Blog.Types
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Default
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Traversable
import           Hakyll
import           Hakyll.Web.Redirect
import           Hakyll.Web.Sass
import           System.FilePath
import           Text.Jasmine
import           Text.Sass
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
import qualified Data.Yaml                 as Y

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

      match "copy/entries/*" $ do
          route . metadataRoute $ \m ->
            case M.lookup "slug" m of
                   Nothing -> gsubRoute "copy/entries/" (\_ -> "entry/ident/")
                                `composeRoutes`
                                setExtension ""
                   Just s  -> constRoute ("entry" </> s)
          compile $ pandocCompiler
          -- compile copyFileCompiler
          -- compile $ pandocCompiler
          --     >>= loadAndApplyTemplate "templates/post.html"    postCtx
          --     >>= loadAndApplyTemplate "templates/default.html" postCtx
          --     >>= relativizeUrls

      homePag <- buildPaginateWith mkPage "copy/entries/*" (\i -> fromFilePath ("home" </> show i))
      preprocess $ print homePag
      paginateRules homePag $ \i p -> do
        preprocess $ print i *> print p
        route idRoute
        compile copyFileCompiler

      forM_ confBlobs $ \b -> do
        match "code-samples/**" $ do
          route   $ gsubRoute ".hs" (\_ -> ".hs.html")
          let f p | ".hs.html" `isSuffixOf` p = dropEnd 5 p
                  | otherwise                 = p
          compile $ redirectCompiler (T.pack . (T.unpack b </>) . f . toFilePath)
  where
    mkPage :: MonadMetadata m => [Identifier] -> m [[Identifier]]
    mkPage ids = do
      withDates <- fmap catMaybes
                 . forM ids $ \i -> runMaybeT $ do
        dString <- MaybeT $ getMetadataField i "date"
        d <- maybe mzero return
           $ parseTime defaultTimeLocale "%Y/%m/%d %X" dString
        return (d :: LocalTime, i)
      let sorted = map snd
                 . sortBy (comparing fst)
                 $ withDates
      return $ paginateEvery 4 sorted


compressJsCompiler :: Compiler (Item String)
compressJsCompiler = fmap f <$> getResourceString
  where
    f :: String -> String
    f = TL.unpack . TL.decodeUtf8 . minify . TL.encodeUtf8 . TL.pack

dropEnd :: Int -> [a] -> [a]
dropEnd i xs = zipWith const xs (drop i xs)
