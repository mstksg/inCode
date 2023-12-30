{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Blog.View.Home where

import           Blog.Compiler.Entry
import           Blog.Types
import           Blog.Util
import           Blog.Util.Tag
import           Blog.View
import           Blog.View.Social
import           Control.Applicative
import           Control.Monad
import           Data.List (sortBy)
import           Data.String
import           Text.Blaze.Html5            ((!))
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Pandoc.Definition      as P

data HomeInfo = HI
    { hiPageNum    :: !Int
    , hiPrevPage   :: !(Maybe FilePath)
    , hiNextPage   :: !(Maybe FilePath)
    , hiEntries    :: ![TaggedEntry]
    , hiAllTags    :: ![Tag]
    , hiLinksCopy  :: !P.Pandoc
    , hiBannerCopy :: !P.Pandoc
    , hiPatrons    :: !PatronList
    }
  deriving (Show)

viewHome :: (?config :: Config) => HomeInfo -> H.Html
viewHome HI{..} =
    H.section ! A.class_ "home-section" ! mainSection $ do

      H.header ! A.class_ "tile unit span-grid" $
        H.section ! A.class_ "home-banner" $
          if hiPageNum == 1
            then do
              copySection (Just (confTitle ?config)) (copyToHtml hiBannerCopy)

              H.aside ! A.class_ "social-follows" $ do
                "Follow or support me on: " :: H.Html
                viewSocialFollow
            else
              H.h1 ! A.class_ "home-banner-history" $
                H.a ! A.href (H.textValue (renderUrl "/")) $
                  H.toHtml (confTitle ?config)

      H.div ! A.class_ "unit three-of-four" $
        entryList hiEntries hiPrevPage hiNextPage hiPageNum

      H.nav ! A.class_ "unit one-of-four home-sidebar" $ do
        H.div ! A.class_ "tile" $ do
          H.div ! A.class_ "home-links" $
            copyToHtml hiLinksCopy
          H.div ! A.class_ "home-patrons" $ do
            H.p "Special thanks to my supporters on Patreon!"
            H.ul . forM_ (M.toList hiPatrons) $ \(pName, PatronInfo{..}) ->
              H.li $ case patronTwitter of
                Nothing -> H.toHtml pName
                Just pt -> do
                  let turl = "https://twitter.com/" <> pt
                  H.a ! A.href (H.textValue turl) $
                    H.toHtml pName
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

    H.ul . forM_ (sortTaggedEntries eList) $ \TE{..} -> do
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
            copyToHtml (entryLede teEntry)
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
    H.ul . forM_ tagLists $ \(tt, heading,link,class_, sorttype) ->
      H.li ! A.class_ class_ $ do
        let tList = filterTags tt
                  . map fst
                  . sortBy (tsCompare sorttype)
                  $ ((,Nothing) <$> tags)
        H.h3 $
          H.a ! A.href (H.textValue $ renderUrl link) $
            heading
        H.ul . forM_ tList $ \t ->
          H.li $ do
            tagLink tagPrettyLabelLower t
            H.preEscapedToHtml ("&nbsp;" :: T.Text)
            H.span $ do
              "(" :: H.Html
              H.toHtml (show (length (tagEntries t)))
              ")" :: H.Html
  where
    tagLists = [(CategoryTag, "Topics", "/categories.html","home-category-list", TSLabel)
               ,(GeneralTag , "Tags"  , "/tags.html"      ,"home-tags-list"    , TSCount)]
