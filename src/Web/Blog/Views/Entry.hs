{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Entry (viewEntry) where

-- import Data.Maybe
-- import Text.Pandoc
-- import Web.Blog.Render
-- import Web.Blog.SiteData
-- import qualified Data.Text.Lazy              as L
-- import qualified Database.Persist.Postgresql as D
import Control.Applicative                      ((<$>))
import Control.Monad.Reader
import Data.Maybe
import Data.Monoid
import Text.Blaze.Html5                         ((!))
import Web.Blog.Models
import Web.Blog.Models.Util
import Web.Blog.Types
import Web.Blog.Util                            (renderFriendlyTime, renderDatetimeTime)
import qualified Data.Map                       as M
import qualified Data.Text                      as T
import qualified Text.Blaze.Html5               as H
import qualified Text.Blaze.Html5.Attributes    as A
import qualified Text.Blaze.Internal            as I

viewEntry :: Entry -> [T.Text] -> Maybe Entry -> Maybe Entry -> SiteRender H.Html
viewEntry entry tags prevEntry nextEntry = do
  siteData' <- pageSiteData <$> ask
  pageDataMap' <- pageDataMap <$> ask

  return $ 

    H.article $ do
      
      H.header $ do

        H.h1 $ H.toHtml $ entryTitle entry

        H.p $ do

          H.toHtml ("by " :: T.Text)

          H.a ! A.class_ "author" $ H.toHtml $ siteDataAuthor siteData'

          H.toHtml (" " :: T.Text)

          H.time
            ! A.datetime (I.textValue $ T.pack $ renderDatetimeTime $ entryPostedAt entry)
            ! A.pubdate "" 
            ! A.class_ "pubdate"
            $ H.toHtml $ renderFriendlyTime $ entryPostedAt entry

          H.ul $
            forM_ tags $ \t ->
              H.li $ H.toHtml t

      H.div ! A.class_ "main-content" $

        entryHtml entry 

      H.footer $
        H.nav $
          H.ul $ do
            when (isJust prevEntry) $
              H.li $ do
                H.preEscapedToHtml ("Previous &mdash; " :: T.Text)
                H.a ! A.href (I.textValue $ pageDataMap' M.! "prevUrl") $
                  H.toHtml $ entryTitle $ fromJust prevEntry

            when (isJust nextEntry) $
              H.li $ do
                H.preEscapedToHtml ("Next &mdash; " :: T.Text)
                H.a ! A.href (I.textValue $ pageDataMap' M.! "nextUrl") $
                  H.toHtml $ entryTitle $ fromJust nextEntry

      H.div ! A.class_ "post-entry" $
        mempty
