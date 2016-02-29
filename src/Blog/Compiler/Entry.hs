{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog.Compiler.Entry where

import           Blog.Types
import           Blog.Util
import           Blog.View
import           Data.Default
import           Data.Monoid
import           Data.Time.LocalTime
import           Hakyll
import           Text.Read           (readMaybe)
import qualified Data.Text           as T
import qualified Text.Pandoc         as P
import qualified Text.Pandoc.Walk    as P

compileEntry :: (?config :: Config) => Compiler (Item Entry)
compileEntry = do
    i         <- getUnderlying
    eBody     <- getResourceBody
    ePandoc   <- readPandocWith entryReaderOpts eBody
    let eContents  = T.pack . itemBody $ writePandocWith entryWriterOpts ePandoc
        ePandocLede = flip fmap ePandoc $ \(P.Pandoc m bs) ->
                        P.Pandoc m . take 5 . takeWhile validLede $ bs
        eLede       = T.pack <$> writePandocWith entryWriterOpts ePandocLede
    eTitle    <- T.unwords . T.lines . T.pack <$> getMetadataField' i "title"
    eCreate   <- (parseETime =<<) <$> getMetadataField i "create-time"
    ePost     <- (parseETime =<<) <$> getMetadataField i "post-time"
    eModified <- (parseETime =<<) <$> getMetadataField i "modified-time"
    eIdent    <- fmap T.pack <$> getMetadataField i "identifier"
    eSlug     <- fmap T.pack <$> getMetadataField i "slug"
    eOldSlugs <- maybe [] (map (T.pack . trim) . splitAll ",")
               <$> getMetadataField i "old-slugs"
    eId       <- (readMaybe =<<) <$> getMetadataField i "entry-id"

    makeItem $ Entry { entryTitle      = eTitle
                     , entryContents   = eContents
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
                  , T.pack (P.writeMarkdown entryWriterOpts body)
                  ]
      where
        timeString = maybe T.empty ((" on " <>) . T.pack . renderShortFriendlyTime)
                   $ time

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

entryReaderOpts :: P.ReaderOptions
entryReaderOpts =
    def { P.readerSmart = True }

entryWriterOpts :: P.WriterOptions
entryWriterOpts =
    def { P.writerHtml5 = True
        , P.writerHTMLMathMethod = P.WebTeX "http://chart.apis.google.com/chart?cht=tx&chf=bg,s,FFFFFF00&chl="
        , P.writerHighlight = True
        , P.writerVariables = [("geometry","margin=1in")
                              ,("links-as-notes","true")]
        }
