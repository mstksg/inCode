module Web.Blog.Util where

import Data.Time
import System.Locale
import qualified Data.Text as T
import Data.Char (isAlphaNum)


renderFriendlyTime :: UTCTime -> String
renderFriendlyTime = formatTime defaultTimeLocale "%A %B %-e, %Y"

renderDatetimeTime :: UTCTime -> String
renderDatetimeTime = formatTime defaultTimeLocale "%FT%XZ"

renderShortFriendlyTime :: UTCTime -> String
renderShortFriendlyTime = formatTime defaultTimeLocale "%B %-e, %Y"

renderYearTime :: UTCTime -> String
renderYearTime = formatTime defaultTimeLocale "%Y"

renderMonthTime :: UTCTime -> String
renderMonthTime = formatTime defaultTimeLocale "%B"

renderYearPath :: UTCTime -> String
renderYearPath = formatTime defaultTimeLocale "/entries/in/%Y"

renderMonthPath :: UTCTime -> String
renderMonthPath = formatTime defaultTimeLocale "/entries/in/%Y/%-m"

genSlug :: Int -> T.Text -> T.Text
genSlug w = squash . T.dropAround isDash . T.map replaceSymbols . T.toCaseFold
  where
    isDash = (==) '-'
    replaceSymbols s =
      if isAlphaNum s
        then
          s
        else
          '-'
    squash = T.intercalate "-" . take w . filter (not . T.null) . T.split isDash
