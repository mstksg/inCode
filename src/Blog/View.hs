{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blog.View where

-- import           Data.Foldable
-- import           Data.String
-- import qualified Text.Blaze.Internal      as H
import           Blog.Types
import           Blog.Util
import           Data.Monoid
import           System.FilePath
import           Text.Blaze.Html5            ((!))
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Pandoc                 as P
import qualified Text.Pandoc.Error           as P

mainSection :: H.Attribute
mainSection = H.customAttribute "role" "main"

renderUrl :: (?config :: Config) => T.Text -> T.Text
renderUrl u | hasP      = u
            | otherwise = T.pack $
                            T.unpack (urlBase ?config) </> T.unpack u
  where
    hasP = length (T.splitOn "//" u) > 1

renderUrl' :: (?config :: Config) => String -> String
renderUrl' = T.unpack . renderUrl . T.pack

urlBase :: Config -> T.Text
urlBase (confHostInfo->HostInfo{..})
    = "http://"
   <> hostBase
   <> maybe mempty (T.pack . (':':) . show) hostPort
   <> maybe mempty (T.cons '/') hostRoot


copyToHtml :: String -> H.Html
copyToHtml = P.writeHtml entryWriterOpts
           . P.handleError
           . P.readMarkdown entryReaderOpts

copySection :: T.Text -> H.Html -> H.Html
copySection title copy = do
    H.header $
      H.h1 $
        H.toHtml title
    H.hr
    H.div ! A.class_ "copy-content" $
      copy
    H.div ! A.class_ "clear" $
      mempty

copyToHtmlString :: String -> String
copyToHtmlString = P.writeHtmlString entryWriterOpts
                 . P.handleError
                 . P.readMarkdown entryReaderOpts

