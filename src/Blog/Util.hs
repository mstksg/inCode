{-# LANGUAGE OverloadedStrings #-}

module Blog.Util where

import           Data.Char
import           Data.Default
import           Data.Time.Format
import           Hakyll
import           System.FilePath
import qualified Data.Text        as T
import qualified Text.Pandoc      as P


renderShortFriendlyTime :: FormatTime t => t -> String
renderShortFriendlyTime = formatTime defaultTimeLocale "%B %-e, %Y"

renderFriendlyTime :: FormatTime t => t -> String
renderFriendlyTime = formatTime defaultTimeLocale "%A %B %-e, %Y"

renderDatetimeTime :: FormatTime t => t -> String
renderDatetimeTime = formatTime defaultTimeLocale "%FT%XZ"


parseETime :: ParseTime t => String -> Maybe t
parseETime = parseTimeM True defaultTimeLocale "%Y/%m/%d %X"

splitTags :: String -> [String]
splitTags = map trim . splitAll ","

genSlug :: Int -> T.Text -> T.Text
genSlug w = squash
          . T.dropAround (== '-')
          . T.map replaceSymbols
          . T.toCaseFold
  where
    replaceSymbols s
      | isAlphaNum s = s
      | otherwise    = '-'
    squash = T.intercalate "-"
           . take w
           . filter (not . T.null)
           . T.split (== '-')

genSlug' :: Int -> String -> String
genSlug' w = T.unpack . genSlug w . T.pack

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

ghcjsReq :: String -> T.Text
ghcjsReq f = T.pack $ "/ghcjs" </> (f <.> "jsexe") </> "all.js"
