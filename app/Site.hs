{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}

-- import           Data.Traversable
-- import           Text.Sass
import           Blog.Types
import           Blog.View
import           Blog.View.Home
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Default
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Time.Format
import           Data.Time.LocalTime
import           Hakyll
import           Hakyll.Web.Blaze
import           Hakyll.Web.Redirect
import           Hakyll.Web.Sass
import           System.FilePath
import           Text.Jasmine
import           Text.Read                 (readMaybe)
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
import qualified Data.Yaml                 as Y
import qualified Text.Pandoc               as P
import qualified Text.Pandoc.Walk          as P

main :: IO ()
main = do
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

      match "copy/entries/*" $ do
          route   routeEntry
          compile $ do
            e <- saveSnapshot "entry" =<< entryCompiler lTempl
            makeItem ("" :: String)

      match "copy/entries/*" . version "markdown" $ do
        route $ routeEntry `composeRoutes` setExtension "md"
        compile $ do
            i <- setVersion Nothing <$> getUnderlying
            e <- itemBody <$> loadSnapshot i "entry"
            makeItem $ T.unpack (entryMarkdown e)

      match "copy/entries/*" . version "latex" $ do
        route $ routeEntry `composeRoutes` setExtension "tex"
        compile $ do
            i <- setVersion Nothing <$> getUnderlying
            e <- itemBody <$> loadSnapshot i "entry"
            makeItem $ T.unpack (entryLaTeX e)

      -- homePag <- buildPaginateWith mkPage "copy/entries/*" (\i -> fromFilePath ("home" </> show i))
      -- paginateRules homePag $ \i p -> do
      --   route idRoute
      --   compile $ blazeCompiler (viewHome undefined)

  where
    routeEntry :: Routes
    routeEntry = metadataRoute $ \m ->
        fromMaybe (setExtension ""
                     `composeRoutes`
                     gsubRoute "copy/entries/" (\_ -> "entry/ident/")
                  )
          $ asum [ constRoute . ("entry" </>)       <$> M.lookup "slug" m
                 , constRoute . ("entry/ident" </>) <$> M.lookup "identifier" m
                 -- , constRoute . ("entry/id" </>)    <$> M.lookup "entry-id" m
                 ]
    mkPage :: MonadMetadata m => [Identifier] -> m [[Identifier]]
    mkPage ids = do
      withDates <- fmap catMaybes
                 . forM ids $ \i -> runMaybeT $ do
        dString <- MaybeT $ getMetadataField i "date"
        d <- maybe mzero return $ parseETime dString
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

entryCompiler :: (?config :: Config) => String -> Compiler (Item Entry)
entryCompiler templ = do
    i         <- getUnderlying
    eBody     <- getResourceBody
    ePandoc   <- readPandocWith readerOpts eBody
    let eContents  = T.pack . itemBody $ writePandocWith writerOpts ePandoc
        ePandocLede = flip fmap ePandoc $ \(P.Pandoc m bs) ->
                        P.Pandoc m . take 5 . takeWhile validLede $ bs
        eLede       = T.pack <$> writePandocWith writerOpts ePandocLede
    eTitle    <- T.unwords . T.lines . T.pack <$> getMetadataField' i "title"
    eCreate   <- (parseETime =<<) <$> getMetadataField i "create-time"
    ePost     <- (parseETime =<<) <$> getMetadataField i "post-time"
    eModified <- (parseETime =<<) <$> getMetadataField i "modified-time"
    eIdent    <- fmap T.pack <$> getMetadataField i "identifier"
    eSlug     <- fmap T.pack <$> getMetadataField i "slug"
    eOldSlugs <- maybe [] (map (T.pack . trim) . splitAll ",")
               <$> getMetadataField i "old-slugs"
    eId       <- (readMaybe =<<) <$> getMetadataField i "entry-id"

    eLaTeX    <- mkTex eTitle eContents ePost

    makeItem $ Entry { entryTitle      = eTitle
                     , entryContents   = eContents
                     , entryMarkdown   = mkMarkdown eTitle (itemBody ePandoc) ePost
                     , entryLaTeX      = itemBody eLaTeX
                     , entryLede       = itemBody eLede
                     , entrySourceFile = toFilePath i
                     , entryCreateTime = eCreate
                     , entryPostTime   = ePost
                     , entryModifyTime = eModified
                     , entryIdentifier = eIdent
                     , entrySlug       = eSlug
                     , entryOldSlugs   = eOldSlugs
                     , entryId         = eId
                     }
  where
    validLede b = case b of
                    P.Header {}      -> False
                    P.HorizontalRule -> False
                    _                -> True
    readerOpts = def { P.readerSmart = True }
    writerOpts = def { P.writerHtml5 = True
                     , P.writerHTMLMathMethod = P.WebTeX "http://chart.apis.google.com/chart?cht=tx&chf=bg,s,FFFFFF00&chl="
                     , P.writerHighlight = True
                     , P.writerVariables = [("geometry","margin=1in")
                                           ,("links-as-notes","true")]
                     }
    mkMarkdown :: T.Text -> P.Pandoc -> Maybe LocalTime -> T.Text
    mkMarkdown title body time =
        T.unlines [ title
                  , T.map (const '=') title
                  , T.empty
                  , T.concat [ "(Originally posted by "
                             , authorName (confAuthorInfo ?config)
                             , " ["
                             , renderUrl "/"
                             , "]"
                             , timeString
                             , ")"
                             ]
                  , T.empty
                  , T.pack (P.writeMarkdown writerOpts body)
                  ]
      where
        timeString = maybe T.empty ((" on " <>) . T.pack . renderShortFriendlyTime)
                   $ time
    mkTex :: T.Text -> T.Text -> Maybe LocalTime -> Compiler (Item T.Text)
    mkTex title body time = do
        body'    <- makeItem $ T.unpack mdHeader
        fmap toTex <$> readPandocWith readerOpts body'
      where
        toTex :: P.Pandoc -> T.Text
        toTex = T.pack
              . P.writeLaTeX opts
              . P.walk upgrade
              . P.bottomUp stripRules
        opts = writerOpts { P.writerStandalone = True
                          , P.writerTemplate   = templ
                          }
        mdHeader = T.unlines [ "% " <> title
                             , "% " <> authorName (confAuthorInfo ?config)
                             , eDate
                             , T.empty
                             , T.concat [ "*Originally posted on **"
                                        , "["
                                        , confTitle ?config
                                        , "]("
                                        , renderUrl "/"
                                        , ")**.*"
                                        ]
                             , T.empty
                             , body
                             ]
        eDate    = maybe T.empty (("% " <>) . T.pack . renderShortFriendlyTime)
                 $ time
        stripRules P.HorizontalRule = P.Null
        stripRules other = other
        upgrade (P.Header n t xs)      | n > 1 = P.Header (n-1) t xs
        upgrade (P.Div di@(_,is,_) xs) | "note" `elem` is = P.Div di (P.HorizontalRule : xs ++ [P.HorizontalRule])
        upgrade x               = x


-- entryTexFull :: Maybe String -> Entry -> T.Text
-- entryTexFull temp e = case temp of
--     Just t  -> let opts' = opts { P.writerStandalone = True
--                                 , P.writerTemplate   = t
--                                 }
--                in  T.pack . P.writeLaTeX opts' $ pd
--     Nothing -> T.pack . P.writeLaTeX opts $ pd
--   where
--     pd      = P.walk upgrade rawPd
--     rawPd   = entryPandoc (e { entryContent = mdHeader })
--     mdHeader = T.unlines [ T.append "% " eTitle
--                          , T.append "% " . authorInfoName . siteDataAuthorInfo $ siteData
--                          , eDate
--                          , T.empty
--                          , T.concat [ "*Originally posted on **"
--                                     , "["
--                                     , siteDataTitle siteData
--                                     , "]("
--                                     , renderUrl' "/"
--                                     , ")**.*"
--                                     ]
--                          , T.empty
--                          , entryContent e
--                          ]
--     eDate   = case entryPostedAt e of
--                 Just t  -> T.append "% " . T.pack . renderShortFriendlyTime $ t
--                 Nothing -> T.empty
--     eTitle  = T.unwords . T.lines $ entryTitle e
--     -- opts = pandocWriterOptions { P.writerNumberSections = True }
--     opts = pandocWriterOptions
--     upgrade (P.Header n t xs) | n > 1 = P.Header (n-1) t xs
--     upgrade (P.Div di@(_,is,_) xs) | "note" `elem` is = P.Div di (P.HorizontalRule : xs ++ [P.HorizontalRule])
--     upgrade x               = x

renderShortFriendlyTime :: LocalTime -> String
renderShortFriendlyTime = formatTime defaultTimeLocale "%B %-e, %Y"

parseETime :: String -> Maybe LocalTime
parseETime = parseTimeM True defaultTimeLocale "%Y/%m/%d %X"

-- data Entry = Entry
--     { entryTitle      :: T.Text
--     , entryContent    :: T.Text
--     , entryPandoc     :: Pandoc
--     , entrySourceFile :: Maybe FilePath
--     , entryCreateTime :: Maybe LocalTime
--     , entryPostTime   :: Maybe LocalTime
--     , entryModifyTime :: Maybe LocalTime
--     , entryIdentifier :: Maybe T.Text
--     , entrySlug       :: Maybe T.Text
--     , entryOldSlugs   :: [T.Text]
--     }
--   deriving (Show)


-- pandocReaderOptions = (P.def P.ReaderOptions)

-- pandocReaderOptions :: P.ReaderOptions
-- pandocReaderOptions = (P.def P.ReaderOptions)
--                       { P.readerSmart = True
--                       }

-- dropEnd :: Int -> [a] -> [a]
-- dropEnd i xs = zipWith const xs (drop i xs)
