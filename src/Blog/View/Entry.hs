{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blog.View.Entry where

import           Blog.Types
import           Blog.Util
import           Blog.Util.Tag
import           Blog.View
import           Blog.View.Social
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Time.LocalTime
import           System.FilePath
import           Text.Blaze.Html5            ((!))
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A


data EntryInfo = EI
    { eiEntry     :: Entry
    , eiTags      :: [Tag]
    , eiPrevEntry :: Maybe Entry
    , eiNextEntry :: Maybe Entry
    }
  deriving (Show)

viewEntry
    :: (?config :: Config)
    => EntryInfo
    -> H.Html
viewEntry EI{..} = do
    H.div ! A.class_ "entry-section unit span-grid" ! mainSection $ do
      H.article ! A.class_ "tile article" $ do
        H.header $ do
          unless isPosted $
            H.div ! A.class_ "unposted-banner" $
              "Unposted entry"

          H.h1 ! A.id "title" $
            H.toHtml $ entryTitle eiEntry

          H.p ! A.class_ "entry-info" $ do

            "by " :: H.Html

            H.a ! A.class_ "author" ! A.href (H.textValue aboutUrl) $
              H.toHtml $ authorName (confAuthorInfo ?config)

            forM_ (entryPostTime eiEntry) $ \t -> do

              H.span ! A.class_ "info-separator" $
                H.preEscapedToHtml
                  (" &diams; " :: T.Text)

              H.time
                ! A.datetime (H.textValue (T.pack (renderDatetimeTime t)))
                ! A.pubdate ""
                ! A.class_ "pubdate"
                $ H.toHtml (renderFriendlyTime t)

          H.p $ do

            H.span ! A.class_ "source-info" $ do
              forM_ (renderBlobUrl (T.pack (entrySourceFile eiEntry))) $ \u -> do
                H.a
                  ! A.class_ "source-link"
                  ! A.href (H.textValue u)
                  $ "Source"

                H.span ! A.class_ "info-separator" $
                  H.preEscapedToHtml
                    (" &diams; " :: T.Text)

              H.a
                ! A.class_ "source-link"
                ! A.href (fromString (renderUrl' (entryCanonical eiEntry <.> "md")))
                $ "Markdown"

              H.span ! A.class_ "info-separator" $
                H.preEscapedToHtml
                  (" &diams; " :: T.Text)

              H.a
                ! A.class_ "source-link"
                ! A.href (fromString (renderUrl' (entryCanonical eiEntry <.> "tex")))
                $ "LaTeX"

              H.span ! A.class_ "info-separator" $
                H.preEscapedToHtml
                  (" &diams; " :: T.Text)

            "Posted in " :: H.Html
            categoryList (filterTags CategoryTag eiTags)
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
          copyToHtml $ T.unpack (entryContents eiEntry)

        H.footer $ do

          H.ul ! A.class_ "entry-series" $
            mapM_ seriesLi (filterTags SeriesTag eiTags)

          H.ul ! A.class_ "tag-list" $
            mapM_ tagLi eiTags

          viewSocialShare

          nextPrevUrl eiPrevEntry eiNextEntry

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
  where
    aboutUrl = renderUrl "/"
    -- isPosted = maybe False (<= eiNow) (entryPostTime eiEntry)
    isPosted = isJust $ entryPostTime eiEntry


nextPrevUrl
    :: (?config :: Config)
    => Maybe Entry
    -> Maybe Entry
    -> H.Html
nextPrevUrl prevEntry nextEntry =
    H.nav ! A.class_ "next-prev-links" $
      H.ul $ do
        forM_ prevEntry $ \Entry{..} ->
          H.li ! A.class_ "prev-entry-link" $ do
            H.preEscapedToHtml ("&larr; " :: T.Text)
            H.a ! A.href (fromString (renderUrl' entryCanonical)) $
              H.toHtml entryTitle
            " (Previous)" :: H.Html

        forM_ nextEntry $ \Entry{..} ->
          H.li ! A.class_ "next-entry-link" $ do
            "(Next) " :: H.Html
            H.a ! A.href (fromString (renderUrl' entryCanonical)) $
              H.toHtml entryTitle
            H.preEscapedToHtml (" &rarr;" :: T.Text)

categoryList
    :: (?config :: Config)
    => [Tag]
    -> H.Html
categoryList = sequence_
             . intersperse ", "
             . map (tagLink (T.unpack . tagLabel))

seriesLi
    :: (?config :: Config)
    => Tag
    -> H.Html
seriesLi t = H.li $
  H.div $ do
    "This entry is a part of a series called " :: H.Html
    H.b $
      H.toHtml $ "\"" <> tagLabel t <> "\""
    ".  Find the rest of the entries in this series at its " :: H.Html
    tagLink (\_ -> " series history") t
    "." :: H.Html
