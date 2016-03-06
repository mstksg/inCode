{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

import           Blog.Compiler.Archive
import           Blog.Compiler.Entry
import           Blog.Compiler.Home
import           Blog.Compiler.Tag
import           Blog.Compiler.TagIndex
import           Blog.Rule.Archive
import           Blog.Types
import           Blog.Util
import           Blog.Util.Tag
import           Blog.View
import           Blog.View.Feed
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Default
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Time.LocalTime
import           Hakyll
import           Hakyll.Web.Redirect
import           Hakyll.Web.Sass
import           System.FilePath
import           Text.Jasmine
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
import qualified Data.Yaml                 as Y

main :: IO ()
main = do
    znow@(ZonedTime now tz) <- getZonedTime

    c@Config{..} <- either throwIO return
                =<< Y.decodeFileEither "config/site-data.yaml"
    let ?config = c

    lTempl <- readFile "latex/templates/default.latex"

    hakyll $ do
      match "static/**" $ do
        route   $ gsubRoute "static/" (\_ -> "")
        compile copyFileCompiler

      match "css/**" $ do
        route   idRoute
        compile compressCssCompiler

      match "scss/**" $ do
        route   $ gsubRoute "scss" (\_ -> "css")
        compile $ sassCompilerWith def { sassIncludePaths = Just ["scss"] }

      match "js/**" $ do
        route   idRoute
        compile compressJsCompiler

      match "copy/tags/**" $ do
        route   mempty
        compile getResourceString

      match "copy/static/**" $ do
        route   mempty
        compile getResourceString

      forM_ confCodeSamples $ \samplesDir -> do
        let pat = fromGlob $ T.unpack samplesDir </> "**"
        match pat $ do
          route   mempty
          compile getResourceString


      match "copy/entries/*" $ do
        route   mempty
        compile $ do
          _ <- saveSnapshot "entry" =<< compileEntry
          getResourceString

      match "copy/entries/*" . version "markdown" $ do
        route   $ routeEntry `composeRoutes` setExtension "md"
        compile entryMarkdownCompiler
      match "copy/entries/*" . version "latex" $ do
        route   $ routeEntry `composeRoutes` setExtension "tex"
        compile $ entryLaTeXCompiler lTempl

      match "copy/entries/*" . version "id-html" $ do
        route   routeIdEntry
        compile $ compileIdEntry ""
      match "copy/entries/*" . version "id-markdown" $ do
        route   $ routeIdEntry `composeRoutes` setExtension "md"
        compile $ compileIdEntry "md"
      match "copy/entries/*" . version "id-latex" $ do
        route   $ routeIdEntry `composeRoutes` setExtension "tex"
        compile $ compileIdEntry "tex"

      hist <- buildHistoryWith ymByField ("copy/entries/*" .&&. hasNoVersion)
                $ \y m -> case m of
                            Nothing -> fromFilePath ("entries/in" </> show y </> "index.html")
                            Just m' -> fromFilePath ("entries/in" </> show y </> show (mInt m'))
      let entriesSorted = sortBy (flip $ comparing fst) $ do
            (y, mes) <- M.toList $ historyMap hist
            (m, es)  <- M.toList mes
            e        <- es
            return ((y, m), e)
          recentEntries = map snd
                        . take (prefSidebarEntries confBlogPrefs)
                        $ entriesSorted
      historyRules' hist $ \spec -> do
        route idRoute
        compile $ do
          case spec of
            Left  (y     , mp) -> do
              archiveCompiler (ADYear y mp) recentEntries
            Right ((y, m), is) -> do
              archiveCompiler (ADMonth y m is) recentEntries

      create ["entries/index.html"] $ do
        route   idRoute
        compile $ archiveCompiler (ADAll (historyMap hist)) recentEntries

      tags <- buildTagsWith
                  (tagsAt "tags")
                  ("copy/entries/*" .&&. hasNoVersion)
                  (fromFilePath . mkTagUrl GeneralTag)
      cats <- buildTagsWith
                  (tagsAt "categories")
                  ("copy/entries/*" .&&. hasNoVersion)
                  (fromFilePath . mkTagUrl CategoryTag)
      sers <- buildTagsWith
                  (tagsAt "series")
                  ("copy/entries/*" .&&. hasNoVersion)
                  (fromFilePath . mkTagUrl SeriesTag)
      let allTags = map ((GeneralTag ,) . T.pack . fst) (tagsMap tags)
                 ++ map ((CategoryTag,) . T.pack . fst) (tagsMap cats)
                 ++ map ((SeriesTag  ,) . T.pack . fst) (tagsMap sers)

      create ["tags"] $ do
        route idRoute
        compile $ tagIndexCompiler GeneralTag  (tagsMap tags) recentEntries
      create ["categories"] $ do
        route idRoute
        compile $ tagIndexCompiler CategoryTag (tagsMap cats) recentEntries
      create ["series"] $ do
        route idRoute
        compile $ tagIndexCompiler SeriesTag   (tagsMap sers) recentEntries

      tagsRules tags $ \tag p -> do
        route   idRoute
        compile $ tagCompiler GeneralTag  tag p recentEntries
      tagsRules cats $ \cat p -> do
        route   idRoute
        compile $ tagCompiler CategoryTag cat p recentEntries
      tagsRules sers $ \ser p -> do
        route   idRoute
        compile $ tagCompiler SeriesTag   ser p recentEntries

      match "copy/entries/*" . version "html" $ do
          route   routeEntry
          compile $ entryCompiler now entriesSorted allTags

      homePag <- buildPaginateWith
                   (mkHomePages (prefHomeEntries confBlogPrefs))
                   ("copy/entries/*" .&&. hasNoVersion)
                   (\i -> fromFilePath ("home" </> show i))
      let allPages = M.keys $ paginateMap homePag
      paginateRules homePag $ \i p -> do
        route   idRoute
        compile $ homeCompiler allPages allTags i p

      create ["home/index.html"] $ do
        route   idRoute
        compile $ redirectCompiler (\_ -> renderUrl "/index.html")
      create ["index.html"] $ do
        route idRoute
        compile $ do
          home1 <- itemBody <$> loadSnapshot "home/1" "index"
          makeItem (home1 :: String)

      create ["rss.raw"] $ do
        route   idRoute
        compile $ do
          sorted <- traverse (flip loadSnapshotBody "entry")
                  . take (prefFeedEntries confBlogPrefs)
                  . map snd
                  $ entriesSorted
          makeItem $ viewFeed sorted tz (zonedTimeToUTC znow)

      create ["rss"] $ do
        route   idRoute
        compile . makeItem . T.unpack . T.unlines
          $ [ "<redirect>"
            , "<newLocation>"
            , confFeed
            , "</newLocation>"
            , "</redirect>"
            ]


  where
    tagsAt f i = do
        d <- (parseETime =<<) <$> getMetadataField i "date"
        case d :: Maybe LocalTime of
          Nothing -> return []
          Just _  -> maybe [] splitTags <$> getMetadataField i f

    routeEntry :: Routes
    routeEntry = metadataRoute $ \m ->
        maybe (setExtension ""
                     `composeRoutes`
                     gsubRoute "copy/entries/" (\_ -> "entry/ident/")
                  )
              constRoute
          $ entryCanonical m
    routeIdEntry :: Routes
    routeIdEntry = metadataRoute $ \m ->
        case M.lookup "entry-id" m of
          Just x  -> constRoute $ "entry/id" </> x
          Nothing -> mempty
    compileIdEntry
        :: (?config :: Config)
        => String
        -> Compiler (Item String)
    compileIdEntry ext = do
        m <- getMetadata =<< getUnderlying
        fp <- ("entry/ident" </>)
            . takeBaseName
            . toFilePath
          <$> getUnderlying
        let canonical = fromMaybe fp (entryCanonical m) <.> ext
        redirectCompiler $ \_ ->
          renderUrl $ T.pack canonical
    entryCanonical :: Metadata -> Maybe FilePath
    entryCanonical m = asum [("entry"</>) <$> M.lookup "slug" m
                            ,("entry/ident"</>) <$> M.lookup "identifier" m
                            ]
    mkHomePages :: MonadMetadata m => Int -> [Identifier] -> m [[Identifier]]
    mkHomePages n ids = do
      withDates <- fmap catMaybes
                 . forM ids $ \i -> runMaybeT $ do
        dString <- MaybeT $ getMetadataField i "date"
        d <- maybe mzero return $ parseETime dString
        return (d :: LocalTime, i)
      let sorted = map snd
                 . sortBy (flip $ comparing fst)
                 $ withDates
      return $ paginateEvery n sorted

compressJsCompiler :: Compiler (Item String)
compressJsCompiler = fmap f <$> getResourceString
  where
    f :: String -> String
    f = TL.unpack . TL.decodeUtf8 . minify . TL.encodeUtf8 . TL.pack

