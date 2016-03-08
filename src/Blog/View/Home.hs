{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blog.View.Home where

import           Blog.Compiler.Entry
import           Blog.Types
import           Blog.Util
import           Blog.Util.Tag
import           Blog.View
import           Blog.View.Social
import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Data.String
import           Text.Blaze.Html5            ((!))
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

data HomeInfo = HI
    { hiPageNum    :: Int
    , hiPrevPage   :: Maybe FilePath
    , hiNextPage   :: Maybe FilePath
    , hiEntries    :: [TaggedEntry]
    , hiAllTags    :: [Tag]
    , hiLinksCopy  :: String
    , hiBannerCopy :: String
    }
  deriving (Show)

viewHome :: (?config :: Config) => HomeInfo -> H.Html
viewHome HI{..} =
    H.section ! A.class_ "home-section" ! mainSection $ do

      H.header ! A.class_ "tile unit span-grid" $
        H.section ! A.class_ "home-banner" $ do
          if hiPageNum == 1
            then
              copySection (confTitle ?config) (copyToHtml hiBannerCopy)
            else
              H.h1 ! A.class_ "home-banner-history" $
                H.a ! A.href (H.textValue (renderUrl "/")) $
                  H.toHtml (confTitle ?config)

          H.aside ! A.class_ "social-follows" $ do
            "Follow me on: " :: H.Html
            viewSocialFollow

      H.div ! A.class_ "unit three-of-four" $
        entryList hiEntries hiPrevPage hiNextPage hiPageNum

      H.nav ! A.class_ "unit one-of-four home-sidebar" $ do
        H.div ! A.class_ "tile home-links" $
          copyToHtml hiLinksCopy
        H.div ! A.class_ "tile home-tags" $
          viewTags hiAllTags


entryList
    :: (?config :: Config)
    => [TaggedEntry]
    -> Maybe FilePath
    -> Maybe FilePath
    -> Int
    -> H.Html
entryList eList prevPage nextPage pageNum = do
    H.div ! A.class_ "tile" $
      H.h2 ! A.class_ "recent-header" $ do
        "Recent Entries" :: H.Html
        when (pageNum > 1) $ do
          " (Page " :: H.Html
          H.toHtml pageNum
          ")" :: H.Html

    H.ul $
      forM_ (sortTaggedEntries eList) $ \TE{..} -> do
        let entryUrl   = T.pack $ renderUrl' (entryCanonical teEntry)
            commentUrl = entryUrl <> "#disqus_thread"
        H.li $
          H.article ! A.class_ "tile" $ do
            H.header $ do
              forM_ (entryPostTime teEntry) $ \t ->
                H.time
                  ! A.datetime (H.textValue $ T.pack (renderDatetimeTime t))
                  ! A.pubdate ""
                  ! A.class_ "pubdate"
                  $ H.toHtml (renderFriendlyTime t)
              H.h3 $
                H.a ! A.href (H.textValue entryUrl) $
                  H.toHtml $ entryTitle teEntry

            H.div ! A.class_ "entry-lede copy-content" $ do
              copyToHtml $ T.unpack (entryLede teEntry)
              H.p $ do
                H.a ! A.href (H.textValue entryUrl) ! A.class_ "link-readmore" $
                  H.preEscapedToHtml
                    ("Read more &hellip; " :: T.Text)
                " " :: H.Html
                H.a ! A.href (H.textValue commentUrl) ! A.class_ "link-comment" $
                  "Comments"

            H.footer $
              H.ul ! A.class_ "tag-list" $
                mapM_ tagLi teTags

    forM_ (prevPage <|> nextPage) $ \_ ->
      H.footer ! A.class_ "tile home-footer" $
        H.nav $ do
          H.ul $ do
            forM_ nextPage $ \nlink ->
              H.li ! A.class_ "home-next" $
                H.a ! A.href (fromString (renderUrl' nlink)) $
                  H.preEscapedToHtml ("&larr; Older" :: T.Text)

            forM_ prevPage $ \plink ->
              H.li ! A.class_ "home-prev" $
                H.a ! A.href (fromString (renderUrl' plink)) $
                  H.preEscapedToHtml ("Newer &rarr;" :: T.Text)
          H.div ! A.class_ "clear" $ ""

viewTags :: (?config :: Config) => [Tag] -> H.Html
viewTags tags =
    H.ul $
      forM_ tagLists $ \(tt, heading,link,class_) ->
        H.li ! A.class_ class_ $ do
          H.h3 $
            H.a ! A.href (H.textValue $ renderUrl link) $
              heading
          H.ul $
            forM_ (filterTags tt (sortTags tags)) $ \t ->
              H.li $ do
                tagLink tagPrettyLabelLower t
                H.preEscapedToHtml ("&nbsp;" :: T.Text)
                H.span $ do
                  "(" :: H.Html
                  H.toHtml (show (length (tagEntries t)))
                  ")" :: H.Html
  where
    tagLists = [(CategoryTag, "Topics", "/categories","home-category-list")
               ,(GeneralTag , "Tags"  , "/tags"      ,"home-tags-list"    )]
