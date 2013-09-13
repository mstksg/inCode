{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Entry (viewEntry) where

import Control.Applicative                   ((<$>))
import Control.Monad.Reader
import Data.List                             (intersperse)
import Data.Maybe
import Data.Monoid
import Data.Time                             (getCurrentTime)
import Text.Blaze.Html5                      ((!))
import Web.Blog.Models
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Util                         (renderFriendlyTime, renderDatetimeTime)
import Web.Blog.Views.Social
import qualified Data.Foldable as Fo         (forM_)
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as I

viewEntry :: Entry -> [Tag] -> Maybe Entry -> Maybe Entry -> SiteRender H.Html
viewEntry entry tags prevEntry nextEntry = do
  siteData' <- pageSiteData <$> ask
  npUl <- nextPrevUrl prevEntry nextEntry
  aboutUrl <- renderUrl "/about"
  socialButtonsHtml <- viewSocialShare
  now <- liftIO getCurrentTime

  let
    isPosted =
      case entryPostedAt entry of
        Just t -> t <= now
        Nothing -> False


  return $

    H.div ! A.class_ "entry-section unit span-grid" ! mainSection $ do

      H.article ! A.class_ "tile article" $ do

        H.header $ do

          -- npUl

          unless isPosted $
            H.div ! A.class_ "unposted-banner" $
              "Unposted entry"

          H.h1 ! A.id "title" $
            H.toHtml $ entryTitle entry

          H.p ! A.class_ "entry-info" $ do

            "by " :: H.Html

            H.a ! A.class_ "author" ! A.href (I.textValue aboutUrl) $
              H.toHtml $ authorInfoName $ siteDataAuthorInfo siteData'

            H.span ! A.class_ "info-separator" $
              H.preEscapedToHtml
                (" &diams; " :: T.Text)

            Fo.forM_ (entryPostedAt entry) $ \t ->
              H.time
                ! A.datetime (I.textValue $ T.pack $ renderDatetimeTime t)
                ! A.pubdate ""
                ! A.class_ "pubdate"
                $ H.toHtml $ renderFriendlyTime t

          H.p $ do
            "Posted in " :: H.Html
            categoryList (filter isCategoryTag tags)
            H.span ! A.class_ "info-separator" $
              H.preEscapedToHtml
                (" &diams; " :: T.Text)
            H.a ! A.class_ "comment-link" ! A.href "#disqus_thread" $ "Comments"

        H.hr

        H.aside ! A.class_ "contents-container" $ do
          H.h5 ! A.id "contents-header" $
            "Contents"
          H.div ! A.id "toc" $ mempty

        H.div ! A.class_ "main-content copy-content" $
          entryHtml entry

        H.footer $ do

          H.ul ! A.class_ "entry-series" $
            forM_ (filter isSeriesTag tags) $ \t ->
              seriesLi t

          H.ul ! A.class_ "tag-list" $
            forM_ tags $ \t ->
              tagLi t

          socialButtonsHtml

          npUl


      H.div ! A.class_ "post-entry" $
        H.div ! A.class_ "tile" $ do
          H.div ! A.id "disqus_thread" $ mempty

          H.noscript $ do
            "Please enable JavaScript to view the " :: H.Html
            H.a ! A.href "http://disqus.com/?ref_noscript" $
              "comments powered by Disqus." :: H.Html
            H.br

          H.a ! A.href "http://disqus.com" ! A.class_ "dsq-brlink" $ do
            "Comments powered by " :: H.Html
            H.span ! A.class_ "logo-disqus" $
                "Disqus" :: H.Html


    -- H.script ! A.type_ "text/javascript" $
    --   tocifyJs
      -- smartLayers


nextPrevUrl :: Maybe Entry -> Maybe Entry -> SiteRender H.Html
nextPrevUrl prevEntry nextEntry = do
  pageDataMap' <- pageDataMap <$> ask

  return $
    H.nav ! A.class_ "next-prev-links" $
      H.ul $ do
        when (isJust prevEntry) $
          H.li ! A.class_ "prev-entry-link" $ do
            H.preEscapedToHtml ("&larr; " :: T.Text)
            H.a ! A.href (I.textValue $ pageDataMap' M.! "prevUrl") $
              H.toHtml $ entryTitle $ fromJust prevEntry
            " (Previous)" :: H.Html

        when (isJust nextEntry) $
          H.li ! A.class_ "next-entry-link" $ do
            "(Next) " :: H.Html
            H.a ! A.href (I.textValue $ pageDataMap' M.! "nextUrl") $
              H.toHtml $ entryTitle $ fromJust nextEntry
            H.preEscapedToHtml (" &rarr;" :: T.Text)

categoryList :: [Tag] -> H.Html
categoryList ts = sequence_ hinter
  where
    hlist = map catLink ts
    hinter = intersperse ", " hlist
    catLink t =
      H.a H.! A.href (I.textValue $ renderUrl' $ tagPath t) $
        H.toHtml $ tagLabel t


seriesLi :: Tag -> H.Html
seriesLi t = H.li $
  H.div $ do
    "This entry is a part of a series called " :: H.Html
    H.b $
      H.toHtml $ T.concat ["\"",tagLabel t,"\""]
    ".  Find the rest of the entries in this series at its " :: H.Html
    H.a ! A.href (I.textValue $ renderUrl' $ tagPath t) $
      " series history" :: H.Html
    "." :: H.Html
