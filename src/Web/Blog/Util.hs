module Web.Blog.Util where

import Data.Time
import System.Locale


renderFriendlyTime :: UTCTime -> String
renderFriendlyTime = formatTime defaultTimeLocale "%A %B %-e, %Y"

renderDatetimeTime :: UTCTime -> String
renderDatetimeTime = formatTime defaultTimeLocale "%FT%XZ"

renderYearTime :: UTCTime -> String
renderYearTime = formatTime defaultTimeLocale "%Y"

renderMonthTime :: UTCTime -> String
renderMonthTime = formatTime defaultTimeLocale "%B"

renderYearPath :: UTCTime -> String
renderYearPath = formatTime defaultTimeLocale "/entries/in/%Y"

renderMonthPath :: UTCTime -> String
renderMonthPath = formatTime defaultTimeLocale "/entries/in/%Y/%-m"
