{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Entry (viewEntry) where

-- import Data.Maybe
import Control.Monad.Reader
import Data.Monoid
import Text.Blaze.Html5 ((!))
import Text.Pandoc
import Web.Blog.Models
import Web.Blog.Render
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Time
import System.Locale
import Web.Blog.SiteData
import Control.Applicative ((<$>))

viewEntry :: Entry -> [T.Text] -> SiteRender H.Html
viewEntry entry tags = do
  siteData' <- pageSiteData <$> ask

  return $ 

    H.article $ do
      
      H.header $ do

        H.h1 $ H.toHtml $ entryTitle entry

        H.section ! A.class_ "entry-details" $ do

          H.h4 $ H.toHtml $ entryDescription entry

          H.html "by "

          H.a ! A.class_ "author" $ H.toHtml $ siteDataAuthor siteData'

          H.div ! A.class_ "article-time" $
            H.toHtml $ renderTime $ entryPostedAt entry

          H.ul ! A.class_ "article-tags" $
            forM_ tags $ \t ->
              H.li $ H.toHtml t

      H.div ! A.class_ "main-content" $

        H.preEscapedToHtml $ writeHtmlString (def WriterOptions) $
          readMarkdown (def ReaderOptions) $ T.unpack $ entryContent entry

      H.footer
        mempty

      H.div ! A.class_ "post-entry" $
        mempty

renderTime :: UTCTime -> String
renderTime = formatTime defaultTimeLocale "%A %B %-e, %Y"

