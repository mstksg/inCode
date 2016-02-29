
module Blog.Util where

import Data.Time.LocalTime
import Data.Time.Format


renderShortFriendlyTime :: LocalTime -> String
renderShortFriendlyTime = formatTime defaultTimeLocale "%B %-e, %Y"

parseETime :: String -> Maybe LocalTime
parseETime = parseTimeM True defaultTimeLocale "%Y/%m/%d %X"

