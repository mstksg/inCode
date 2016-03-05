{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Blog.Compiler.Entry where

-- import           Data.Default
-- import           Data.Time.LocalTime
import           Blog.Types
import           Blog.Util
import           Blog.View
import           Data.Bifunctor
import           Data.Foldable
import           Data.Maybe             (fromMaybe)
import           Data.Monoid
import           Hakyll
import           System.FilePath
import           Text.Read              (readMaybe)
import qualified Data.Text              as T
import qualified Text.Pandoc            as P
import qualified Text.Pandoc.Walk       as P

compileEntry :: (?config :: Config) => Compiler (Item Entry)
compileEntry = do
    i         <- getUnderlying
    eBody     <- getResourceBody
    ePandoc   <- readPandocWith entryReaderOpts eBody
    let eContents   = T.pack . P.writeMarkdown entryWriterOpts <$> ePandoc
        ePandocLede = flip fmap ePandoc $ \(P.Pandoc m bs) ->
                        P.Pandoc m
                          . take (prefLedeMax (confBlogPrefs ?config))
                          . takeWhile validLede
                          $ bs
        eLede       = T.pack . P.writeMarkdown entryWriterOpts <$> ePandocLede
    eTitle    <- T.unwords . T.lines . T.pack <$> getMetadataField' i "title"
    eCreate   <- (parseETime =<<) <$> getMetadataField i "create-time"
    ePost     <- (parseETime =<<) <$> getMetadataField i "date"
    eModified <- (parseETime =<<) <$> getMetadataField i "modified-time"
    eIdent    <- fmap T.pack <$> getMetadataField i "identifier"
    eSlug     <- fmap T.pack <$> getMetadataField i "slug"
    eOldSlugs <- maybe [] (map (T.pack . trim) . splitAll ",")
               <$> getMetadataField i "old-slugs"
    eId       <- (readMaybe =<<) <$> getMetadataField i "entry-id"
    tags      <- maybe [] splitTags <$> getMetadataField i "tags"
    cats      <- maybe [] splitTags <$> getMetadataField i "categories"
    sers      <- maybe [] splitTags <$> getMetadataField i "series"

    makeItem $ Entry { entryTitle      = eTitle
                     , entryContents   = itemBody eContents
                     , entryLede       = itemBody eLede
                     , entrySourceFile = toFilePath i
                     , entryCreateTime = eCreate
                     , entryPostTime   = ePost
                     , entryModifyTime = eModified
                     , entryIdentifier = eIdent
                     , entrySlug       = eSlug
                     , entryOldSlugs   = eOldSlugs
                     , entryId         = eId
                     , entryCanonical  = mkCanonical eSlug eIdent (toFilePath i)
                     , entryTags       = (map . second) T.pack
                                       $ map (GeneralTag,)  tags
                                      ++ map (CategoryTag,) cats
                                      ++ map (SeriesTag,)   sers
                     }
  where
    validLede b = case b of
                    P.Header {}      -> False
                    P.HorizontalRule -> False
                    _                -> True

entryMarkdownCompiler :: (?config :: Config) => Compiler (Item String)
entryMarkdownCompiler = do
    i <- setVersion Nothing <$> getUnderlying
    Entry{..} <- itemBody <$> loadSnapshot i "entry"
    let timeString = maybe T.empty ((" on " <>) . T.pack . renderShortFriendlyTime)
                   $ entryPostTime
    makeItem . T.unpack . T.unlines
      $ [ entryTitle
        , T.map (const '=') entryTitle
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
        , entryContents
        ]

entryLaTeXCompiler :: (?config :: Config) => String -> Compiler (Item String)
entryLaTeXCompiler templ = do
    i <- setVersion Nothing <$> getUnderlying
    Entry{..} <- itemBody <$> loadSnapshot i "entry"
    let eDate    = maybe T.empty (("% " <>) . T.pack . renderShortFriendlyTime)
                 $ entryPostTime
        mdHeader = T.unlines [ "% " <> entryTitle
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
                             , entryContents
                             ]
    body <- makeItem $ T.unpack mdHeader
    fmap toTex <$> readPandocWith entryReaderOpts body
  where
    toTex :: P.Pandoc -> String
    toTex = P.writeLaTeX opts
          . P.walk upgrade
          . P.bottomUp stripRules
    stripRules P.HorizontalRule = P.Null
    stripRules other = other
    upgrade p = case p of
                  P.Header n t xs      | n > 1
                      -> P.Header (n - 1) t xs
                  P.Div di@(_,is,_) xs | "note" `elem` is
                      -> P.Div di (P.HorizontalRule : xs ++ [P.HorizontalRule])
                  _
                      -> p
    opts = entryWriterOpts { P.writerStandalone = True
                           , P.writerTemplate   = templ
                           }

mkCanonical
    :: Maybe T.Text
    -> Maybe T.Text
    -> FilePath
    -> FilePath
mkCanonical slug ident source =
    fromMaybe (source `replaceDirectory` "entry/ident")
  . asum
  $ [ ("entry" </>)       . T.unpack <$> slug
    , ("entry/ident" </>) . T.unpack <$> ident
    ]
