{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blog.Util where

import           Blog.Types
import           Control.Monad
import           Data.Char
import           Data.Default
import           Data.Maybe
import           Data.Monoid
import           Data.Time.Format
import           Hakyll
import qualified Data.Text                as T
import qualified Text.Pandoc              as P
import qualified Text.Pandoc.Highlighting as P


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
entryReaderOpts = def
    -- def { P.readerSmart = True }

entryWriterOpts :: P.WriterOptions
entryWriterOpts =
    def { 
        -- P.writerHtml5 = True
        -- , P.writerHTMLMathMethod = P.WebTeX "http://chart.apis.google.com/chart?cht=tx&chf=bg,s,FFFFFF00&chl="
        -- , P.writerHTMLMathMethod = P.WebTeX "https://latex.codecogs.com/gif.latex?"
          P.writerHTMLMathMethod = P.WebTeX "https://latex.codecogs.com/png.latex?"
        , P.writerHighlightStyle = Just P.tango
        , P.writerVariables = [("geometry","margin=1in")
                              ,("links-as-notes","true")]
        , P.writerColumns   = 80
        }

sourceBlobs :: Config -> Maybe T.Text
sourceBlobs = sourceBlobs' <=< confBlobs

sourceBlobs' :: Blobs -> Maybe T.Text
sourceBlobs' Blobs{..} = (blobsTree </!>) <$> blobsSourceBranch

renderBlobs :: Config -> Maybe T.Text
renderBlobs = renderBlobs' <=< confBlobs

renderBlobs' :: Blobs -> Maybe T.Text
renderBlobs' Blobs{..} = (blobsTree </!>) <$> blobsRenderBranch


(</!>) :: T.Text -> T.Text -> T.Text
b </!> f = let f' = fromMaybe f $ T.stripPrefix "/" f
               b' = fromMaybe b $ T.stripSuffix "/" b
           in  b' <> ('/' `T.cons` f')


