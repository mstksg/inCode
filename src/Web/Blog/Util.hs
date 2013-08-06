module Web.Blog.Util where

import Data.Time
import System.Locale
-- import qualified Data.Text as T
-- import qualified Text.Pandoc as P
-- import qualified Text.Blaze.Html5 as H


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

-- markdownToHtml :: T.Text -> H.Html
-- markdownToHtml = P.writeHtml (P.def P.WriterOptions) .
--   P.readMarkdown (P.def P.ReaderOptions) . T.unpack
