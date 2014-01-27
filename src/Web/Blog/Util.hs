module Web.Blog.Util where

import "base" Prelude
import Data.Char                        (isAlphaNum)
import Data.List                        (intersperse)
import Data.Time
import System.Locale
import qualified Data.Text              as T
import qualified Text.Pandoc            as P
import qualified Text.Pandoc.Builder    as P
import qualified Text.Pandoc.Shared     as P


renderFriendlyTime :: TimeZone -> UTCTime -> String
renderFriendlyTime tz = formatTime defaultTimeLocale "%A %B %-e, %Y" . zonedTime
  where
    zonedTime = utcToZonedTime tz

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

genSlugSuffix :: Int -> Int -> T.Text -> T.Text
genSlugSuffix w s = (`T.append` (slugSuffix !! s)) . genSlug w
  where
    slugSuffix = "" : map (T.pack . show) ([-2,-3..] :: [Int])

stripPandoc :: P.Pandoc -> T.Text
stripPandoc (P.Pandoc _ bs) = T.pack $ P.stringify inls
  where
    inls = concatMap grabInls . intersperse (P.Plain [P.Space]) $ bs
    grabInls :: P.Block -> [P.Inline]
    grabInls (P.Plain inls') = inls'
    grabInls (P.Para inls') = inls'
    grabInls (P.CodeBlock _ str) = P.toList $ P.text str
    grabInls (P.RawBlock _ str) = P.toList $ P.text str
    grabInls (P.BlockQuote bs') = concatMap grabInls bs'
    grabInls (P.OrderedList _ bss ) =
      concatMap ((++) (P.toList $ P.text " * ") . concatMap grabInls) bss
    grabInls (P.BulletList bss) =
      concatMap ((++) (P.toList $ P.text " * ") . concatMap grabInls) bss
    grabInls (P.DefinitionList ds) = concatMap mapDs ds
      where
        mapDs (inls', bss) =
          concat
            [ P.toList $ P.text " * "
            , inls'
            , P.toList $ P.text ": "
            , concatMap (concatMap grabInls) bss
            ]
    grabInls (P.Header _ _ inls') = inls'
    grabInls (P.HorizontalRule) = P.toList $ P.text "---"
    grabInls (P.Table cap _ _ _ _) = cap
    grabInls _ = []


