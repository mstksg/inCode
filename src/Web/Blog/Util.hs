module Web.Blog.Util where

import Data.Time
import System.Locale

renderFriendlyTime :: UTCTime -> String
renderFriendlyTime = formatTime defaultTimeLocale "%A %B %-e, %Y"

renderDatetimeTime :: UTCTime -> String
renderDatetimeTime = formatTime defaultTimeLocale "%FT%XZ"
