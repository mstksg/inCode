{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Entry (viewEntry) where

-- import Data.Maybe
-- import Web.Blog.Render
-- import Web.Blog.SiteData
-- import qualified Data.Text.Lazy as L
import Control.Applicative ((<$>))
import Control.Monad.Reader
import Data.Monoid
import Text.Blaze.Html5 ((!))
import Text.Pandoc
import Web.Blog.Models
import Web.Blog.Types
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal as I
import Data.Maybe

viewEntry :: Entry -> [T.Text] -> Maybe T.Text -> Maybe T.Text -> SiteRender H.Html
viewEntry entry tags prevUrl nextUrl = do
  siteData' <- pageSiteData <$> ask

  return $ 

    H.article $ do
      
      H.header $ do

        H.h1 $ H.toHtml $ entryTitle entry

        H.section ! A.class_ "entry-details" $ do

          -- TODO: Move description to article maybe, for Pocket?
          H.h4 $ H.toHtml $ entryDescription entry

          H.toHtml ("by " :: T.Text)

          H.a ! A.class_ "author" $ H.toHtml $ siteDataAuthor siteData'

          H.time
            ! A.datetime (I.textValue $ T.pack $ renderDatetimeTime $ entryPostedAt entry)
            ! A.pubdate "" 
            ! A.class_ "pubdate"
            $ H.toHtml $ renderFriendlyTime $ entryPostedAt entry

          H.ul ! A.class_ "article-tags" $
            forM_ tags $ \t ->
              H.li $ H.toHtml t

      H.div ! A.class_ "main-content" $

        H.preEscapedToHtml $ writeHtmlString (def WriterOptions) $
          readMarkdown (def ReaderOptions) $ T.unpack $ entryContent entry

      H.footer $
        H.ul $ do
          when (isJust prevUrl) $
            H.li ! A.class_ "prev-li" $
              H.a ! A.href (I.textValue $ fromJust prevUrl) $
                "Previous"
          when (isJust nextUrl) $
            H.li ! A.class_ "next-li" $
              H.a ! A.href (I.textValue $ fromJust nextUrl) $
                "Next"

      H.div ! A.class_ "post-entry" $
        mempty
