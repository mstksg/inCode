
module Blog.Util where

import Data.Time.LocalTime
import Data.Time.Format


renderShortFriendlyTime :: FormatTime t => t -> String
renderShortFriendlyTime = formatTime defaultTimeLocale "%B %-e, %Y"

parseETime :: ParseTime t => String -> Maybe t
parseETime = parseTimeM True defaultTimeLocale "%Y/%m/%d %X"

