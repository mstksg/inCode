{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Blog.App where

import Blog.Compiler.Archive
import Blog.Compiler.Entry
import Blog.Compiler.Home
import Blog.Compiler.Redirect
import Blog.Compiler.Tag
import Blog.Compiler.TagIndex
import Blog.Rule.Archive
import Blog.Types
import Blog.Util
import Blog.Util.Sass
import Blog.Util.Tag
import Blog.View
import Blog.View.Feed
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Default
import Data.Foldable
import Data.List (sortBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time.LocalTime
import Data.Time.Zones
import Data.Traversable
import Data.Void
import Hakyll
import Hakyll.Web.Dhall
import Hakyll.Web.Sass
import Numeric.Natural
import qualified Skylighting.Core as Sky
import System.FilePath
import Text.Jasmine
import Text.Read (readMaybe)

app ::
  (?config :: Config) =>
  TZ ->
  Rules ()
app tz = do
  match "static/**" $ do
    route $ gsubRoute "static/" (const "")
    compile copyFileCompiler

  match "config/**.dhall" $ do
    route idRoute
    compile $ dExprCompiler @Void

  create ["CNAME"] $ do
    route idRoute
    compile . makeItem . T.unpack $ hostBase confHostInfo <> "\n"

  match "css/**" $ do
    route idRoute
    compile compressCssCompiler

  match "scss/**" $ do
    route $ gsubRoute "scss" (const "css")
    compile $
      sassCompilerWith
        def
          { sassIncludePaths = Just ["scss"],
            sassFunctions = Just $ renderSassUrl : concat (sassFunctions def),
            sassOutputStyle = SassStyleCompressed
          }

  match "js/**" $ do
    route idRoute
    case confEnvType of
      ETDevelopment -> compile copyFileCompiler
      ETProduction -> compile compressJsCompiler

  match "_purescript/**" $ do
    route $ gsubRoute "_purescript/" (const "purescript/")
    -- I guess we really don't need to compress if the build system already
    -- outputs these compressed
    case confEnvType of
      ETDevelopment -> compile copyFileCompiler
      ETProduction -> compile compressJsCompiler

  match "copy/tags/**" $ do
    route mempty
    compile getResourceString

  match "copy/static/**" $ do
    route mempty
    compile getResourceString

  match "latex/templates/*" $ do
    route mempty
    compile getResourceString

  match "aux/syntax/dhall.xml" $ do
    route mempty
    compile kateSyntaxCompiler

  forM_ confCodeSamples $ \samplesDir -> do
    let pat =
          fromGlob (T.unpack samplesDir </> "**.hs")
            .||. fromGlob (T.unpack samplesDir </> "**.txt")
            .||. fromGlob (T.unpack samplesDir </> "**.nix")
    match pat $ do
      route mempty
      compile getResourceString

  match "copy/entries/*" $ do
    route mempty
    compile $ do
      _ <- saveSnapshot "entry" =<< compileEntry
      getResourceString

  match "copy/entries/*" . version "markdown" $ do
    route $ routeEntry `composeRoutes` setExtension "md"
    compile entryMarkdownCompiler
  match "copy/entries/*" . version "latex" $ do
    route $ routeEntry `composeRoutes` setExtension "tex"
    compile entryLaTeXCompiler

  match "copy/entries/*" . version "id" $ do
    route routeIdEntry
    compile $ compileIdEntry "html" Nothing
  match "copy/entries/*" . version "id-index" $ do
    route $
      routeIdEntry
        `composeRoutes` gsubRoute ".html" (const "/index.html")
    compile $ compileIdEntry "html" Nothing
  match "copy/entries/*" . version "id-markdown" $ do
    route $ routeIdEntry `composeRoutes` setExtension "md"
    compile $ compileIdEntry "md" (Just "markdown")
  match "copy/entries/*" . version "id-latex" $ do
    route $ routeIdEntry `composeRoutes` setExtension "tex"
    compile $ compileIdEntry "tex" (Just "latex")

  hist <- buildHistoryWith ymByField ("copy/entries/*" .&&. hasNoVersion) $
    \y m -> case m of
      Nothing -> fromFilePath ("entries/in" </> show y <.> "html")
      Just m' -> fromFilePath ("entries/in" </> show y </> show (mInt m') <.> "html")
  let entriesSorted =
        fold
          . M.unionsWith (++)
          . foldMap toList
          $ historyMap hist
  historyRules' hist $ \spec -> do
    route idRoute
    compile $ case spec of
      Left (y, mp) -> archiveCompiler (ADYear y mp)
      Right ((y, m), is) -> archiveCompiler (ADMonth y m is)
  historyRules' hist $ const indexRules

  create ["entries.html"] $ do
    route idRoute
    compile $ archiveCompiler (ADAll (historyMap hist))
  create ["entries.html"] indexRules

  tags <-
    buildTagsWith
      (tagsAt "tags")
      ("copy/entries/*" .&&. hasNoVersion)
      (fromFilePath . mkTagUrl GeneralTag)
  cats <-
    buildTagsWith
      (tagsAt "categories")
      ("copy/entries/*" .&&. hasNoVersion)
      (fromFilePath . mkTagUrl CategoryTag)
  sers <-
    buildTagsWith
      (tagsAt "series")
      ("copy/entries/*" .&&. hasNoVersion)
      (fromFilePath . mkTagUrl SeriesTag)
  let allTags =
        map ((GeneralTag,) . T.pack . fst) (tagsMap tags)
          ++ map ((CategoryTag,) . T.pack . fst) (tagsMap cats)
          ++ map ((SeriesTag,) . T.pack . fst) (tagsMap sers)

  create ["tags.html"] $ do
    route idRoute
    compile $ tagIndexCompiler GeneralTag (tagsMap tags)
  create ["tags.html"] indexRules
  create ["categories.html"] $ do
    route idRoute
    compile $ tagIndexCompiler CategoryTag (tagsMap cats)
  create ["categories.html"] indexRules
  create ["series.html"] $ do
    route idRoute
    compile $ tagIndexCompiler SeriesTag (tagsMap sers)
  create ["series.html"] indexRules

  forM_ [(GeneralTag, tags), (CategoryTag, cats), (SeriesTag, sers)] $
    \(tt, ts) -> do
      tagsRules ts $ \t p -> do
        route idRoute
        compile $ tagCompiler tt t p
      tagsRules ts $ \_ _ -> indexRules

  match "copy/entries/*" . version "html" $ do
    route routeEntry
    compile $ entryCompiler entriesSorted allTags
  match "copy/entries/*" . version "html-index" $ do
    route $
      routeEntry
        `composeRoutes` gsubRoute ".html" (const "/index.html")
    compile $ do
      i <- setVersion Nothing <$> getUnderlying
      c <- entryCanonical <$> loadSnapshotBody i "entry"
      redirectCompiler $ \_ -> renderUrl $ T.pack c

  homePag <-
    buildPaginateWith
      (mkHomePages (prefHomeEntries confBlogPrefs))
      ("copy/entries/*" .&&. hasNoVersion)
      (\i -> fromFilePath ("home" </> show i <.> "html"))
  let allPages = M.keys $ paginateMap homePag
  paginateRules homePag $ \i p -> do
    route idRoute
    compile $ homeCompiler allPages allTags i p
  paginateRules homePag $ \i _ ->
    if i == 1
      then version "index" $ do
        route $ gsubRoute ".html" (const "/index.html")
        compile $ redirectCompiler (\_ -> renderUrl "/index.html")
      else indexRules

  create ["home.html"] $ do
    route idRoute
    compile $ redirectCompiler (\_ -> renderUrl "/index.html")
  create ["home/index.html"] $ do
    route idRoute
    compile $ redirectCompiler (\_ -> renderUrl "/index.html")
  create ["index.html"] $ do
    route idRoute
    compile $ do
      home1 <- itemBody <$> loadSnapshot "home/1.html" "index"
      makeItem (home1 :: String)
  -- create ["cv.html"] $ do
  --   route   idRoute
  --   compile $ archiveCompiler (ADAll (historyMap hist))

  create ["rss.raw"] $ do
    deps <- makePatternDependency "copy/entries/*"
    rulesExtraDependencies [deps] $ do
      route idRoute
      compile $ do
        Just sorted <-
          fmap NE.nonEmpty
            . traverse (`loadSnapshotBody` "entry")
            . take (fromIntegral (prefFeedEntries confBlogPrefs))
            . reverse
            $ entriesSorted
        wopts <- entryWriterOpts
        makeItem . TL.unpack $ viewFeed wopts sorted tz

  create ["rss"] $ do
    route idRoute
    compile . makeItem . T.unpack . T.unlines $
      [ "<redirect>",
        "<newLocation>",
        confFeed,
        "</newLocation>",
        "</redirect>"
      ]
  where
    Config {..} = ?config
    tagsAt f i = do
      d <- (parseETime =<<) <$> getMetadataField i "date"
      case d :: Maybe LocalTime of
        Nothing -> return []
        Just _ -> maybe [] splitTags <$> getMetadataField i f

    routeEntry :: Routes
    routeEntry = metadataRoute $ \m ->
      maybe
        ( setExtension "html"
            `composeRoutes` gsubRoute "copy/entries/" (const "entry/ident/")
        )
        constRoute
        $ entryCanonical' m
    routeIdEntry :: Routes
    routeIdEntry = metadataRoute $ \m ->
      case entryIdInt m of
        Just x -> constRoute $ "entry/id" </> show x <.> "html"
        Nothing -> mempty
      where
        entryIdInt m = do
          eiString <- lookupString "entry-id" m
          eiDouble <- readMaybe eiString :: Maybe Double
          return (round eiDouble :: Integer)
    compileIdEntry ::
      (?config :: Config) =>
      String ->
      Maybe Snapshot ->
      Compiler (Item String)
    compileIdEntry ext = \case
      Nothing -> do
        m <- getMetadata =<< getUnderlying
        fp <-
          ("entry/ident" </>)
            . takeBaseName
            . toFilePath
            <$> getUnderlying
        let canonical = fromMaybe fp (entryCanonical' m) -<.> ext
        redirectCompiler $ \_ ->
          renderUrl $ T.pack canonical
      Just v ->
        makeItem
          =<< loadBody . setVersion (Just v)
          =<< getUnderlying

    entryCanonical' :: Metadata -> Maybe FilePath
    entryCanonical' m =
      asum
        [ (<.> "html") . ("entry" </>) <$> lookupString "slug" m,
          (<.> "html") . ("entry/ident" </>) <$> lookupString "identifier" m
        ]
    mkHomePages :: (MonadMetadata m) => Natural -> [Identifier] -> m [[Identifier]]
    mkHomePages n ids = do
      withDates <- fmap catMaybes
        . forM ids
        $ \i -> runMaybeT $ do
          dString <- MaybeT $ getMetadataField i "date"
          d <- maybe mzero return $ parseETime dString
          return (d :: LocalTime, i)
      let sorted =
            map snd
              . sortBy (comparing (Down . fst))
              $ withDates
      return $ paginateEvery (fromIntegral n) sorted
    indexRules = version "index" $ do
      route $ gsubRoute ".html" (const "/index.html")
      compile $ redirectCompiler (renderUrl . T.pack . toFilePath)

compressJsCompiler :: Compiler (Item String)
compressJsCompiler = fmap f <$> getResourceString
  where
    f :: String -> String
    f = TL.unpack . TL.decodeUtf8 . minify . TL.encodeUtf8 . TL.pack

kateSyntaxCompiler :: Compiler (Item Sky.Syntax)
kateSyntaxCompiler = do
  r <- getRoute =<< getUnderlying
  istr <- getResourceString
  for istr \str ->
    case Sky.parseSyntaxDefinitionFromText (fold r) (TL.pack str) of
      Left e -> fail e
      Right x -> pure x
