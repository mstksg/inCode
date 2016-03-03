{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- import           Blog.View.Home
-- import           Control.Applicative
-- import           Data.Monoid
-- import           Data.Time.Format
-- import           Data.Traversable
-- import           Hakyll.Web.Blaze
-- import           Text.Read              (readMaybe)
-- import           Text.Sass
-- import qualified Text.Pandoc            as P
-- import qualified Text.Pandoc.Walk       as P
import           Blog.Compiler.Entry
import           Blog.Compiler.Tag
import           Blog.Rule.Archive
import           Blog.Types
import           Blog.Util
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
import           Data.Time.Clock
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
    now <- getCurrentTime
    tz  <- getCurrentTimeZone

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
        compile $ sassCompilerWith def{ sassIncludePaths = Just ["scss"] }

      match "js/**" $ do
        route   idRoute
        compile compressJsCompiler

      match "copy/tags/**" $ do
        route   mempty
        compile getResourceString

      match "copy/entries/*" $ do
          route   routeEntry
          compile $ do
            e <- saveSnapshot "entry" =<< compileEntry
            return $ T.unpack . entryContents <$> e

      match "copy/entries/*" . version "markdown" $ do
        route   $ routeEntry `composeRoutes` setExtension "md"
        compile entryMarkdownCompiler
      match "copy/entries/*" . version "latex" $ do
        route   $ routeEntry `composeRoutes` setExtension "tex"
        compile $ entryLaTeXCompiler lTempl

      match "copy/entries/*" . version "id" $ do
        route   routeIdEntry
        compile $ compileIdEntry ""
      match "copy/entries/*" . version "id-markdown" $ do
        route   $ routeIdEntry `composeRoutes` setExtension "md"
        compile $ compileIdEntry "md"
      match "copy/entries/*" . version "id-latex" $ do
        route   $ routeIdEntry `composeRoutes` setExtension "tex"
        compile $ compileIdEntry "tex"

      create ["entries/index.html"] $ do
        route idRoute
        compile $ do
          entries <- map itemBody
                      <$> loadAllSnapshots ("copy/entries/*" .&&. hasNoVersion)
                                           "entry"
          let sorted = sortBy (flip $ comparing entryPostTime)
                     . filter (isJust . entryPostTime)
                     $ entries
          makeItem . unlines $ map (T.unpack . entryTitle) sorted

      hist <- buildHistoryWith ymByField ("copy/entries/*" .&&. hasNoVersion)
                $ \y m -> case m of
                            Nothing -> fromFilePath ("entries/in" </> show y </> "index.html")
                            Just m' -> fromFilePath ("entries/in" </> show y </> show (mInt m'))
      historyRules hist $ \y m p -> do
        route idRoute
        compile $ do
          entries <- map itemBody <$> loadAllSnapshots p "entry"
          let sorted = sortBy (flip $ comparing entryPostTime)
                     . filter (isJust . entryPostTime)
                     $ entries
          makeItem . unlines . map (T.unpack . entryTitle)
            $ sorted

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

      create ["tags"] $ do
        route idRoute
        compile $ do
          makeItem . unlines . flip map (tagsMap tags) $ \(t,es) ->
            '#':t ++ " (" ++ show (length es) ++ ")"
      create ["categories"] $ do
        route idRoute
        compile $ do
          makeItem . unlines . flip map (tagsMap cats) $ \(t,es) ->
            '@':t ++ " (" ++ show (length es) ++ ")"
      create ["series"] $ do
        route idRoute
        compile $ do
          makeItem . unlines . flip map (tagsMap sers) $ \(t,es) ->
            '+':t ++ " (" ++ show (length es) ++ ")"

      tagsRules tags $ \tag p -> do
        route   idRoute
        compile $ tagCompiler GeneralTag  tag p
      tagsRules cats $ \cat p -> do
        route   idRoute
        compile $ tagCompiler CategoryTag cat p
      tagsRules sers $ \ser p -> do
        route   idRoute
        compile $ tagCompiler SeriesTag   ser p


      homePag <- buildPaginateWith
                   (mkHomePages (prefHomeEntries confBlogPrefs))
                   ("copy/entries/*" .&&. hasNoVersion)
                   (\i -> fromFilePath ("home" </> show i))
      paginateRules homePag $ \i p -> do
        route idRoute
        compile $ do
          contents <- map (fmap (T.unpack . entryLede))
                        <$> loadAllSnapshots p "entry"
          renders <- traverse (renderPandocWith entryReaderOpts entryWriterOpts)
                       contents
          render <- makeItem $ unlines (map itemBody renders)
          if i == 1
            then do
              _ <- saveSnapshot "index" render
              redirectCompiler (\_ -> renderUrl "/index.html")
            else do
              return render


      create ["index.html"] $ do
        route idRoute
        compile $ do
          home1 <- itemBody <$> loadSnapshot "home/1" "index"
          makeItem (home1 :: String)
        -- compile $ blazeCompiler (viewHome undefined)

      create ["rss.raw"] $ do
        route   idRoute
        compile $ do
          entries <- map itemBody
                 <$> loadAllSnapshots ("copy/entries/*" .&&. hasNoVersion)
                                        "entry"
          let sorted = take (prefFeedEntries confBlogPrefs)
                     . sortBy (flip $ comparing entryPostTime)
                     . filter (isJust . entryPostTime)
                     $ entries
          makeItem $ viewFeed sorted tz now

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

