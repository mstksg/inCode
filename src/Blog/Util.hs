{-# LANGUAGE OverloadedStrings #-}

module Blog.Util where

import           Data.Char
import           Data.Time.Format
import qualified Data.Text           as T


renderShortFriendlyTime :: FormatTime t => t -> String
renderShortFriendlyTime = formatTime defaultTimeLocale "%B %-e, %Y"

parseETime :: ParseTime t => String -> Maybe t
parseETime = parseTimeM True defaultTimeLocale "%Y/%m/%d %X"


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
