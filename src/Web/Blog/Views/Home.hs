{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Home (viewHome) where

import Config.SiteData
import Control.Applicative                   ((<$>))
import Control.Monad.Reader
import Data.Time                             (getCurrentTimeZone)
import Text.Blaze.Html5                      ((!))
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Models.Types
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Util                         (renderFriendlyTime, renderDatetimeTime)
import Web.Blog.Views.Copy
import Web.Blog.Views.Social
import qualified Data.Foldable               as Fo
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Database.Persist.Postgresql as D
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as I

viewHome :: [(D.Entity Entry,(T.Text,[Tag]))] -> Int -> SiteRender H.Html
viewHome eList pageNum = do
  pageDataMap' <- pageDataMap <$> ask
  bannerCopy <- viewCopyFile (siteDataTitle siteData) "copy/static/home-banner.md"
  linksHtml <- viewLinks
  tagsHtml <- viewTags
  homeUrl <- renderUrl "/"
  socialFollowsHtml <- viewSocialFollow
  entryListHtml <- entryList eList pageDataMap' pageNum


  return $
    H.section ! A.class_ "home-section" ! mainSection $ do

      H.header ! A.class_ "tile unit span-grid" $
        H.section ! A.class_ "home-banner" $ do
          if pageNum == 1
            then
              bannerCopy
            else
              H.h1 ! A.class_ "home-banner-history" $
                H.a ! A.href (I.textValue homeUrl) $
                  H.toHtml $ siteDataTitle siteData
          H.aside ! A.class_ "social-follows" $ do
            "Follow me on: " :: H.Html
            socialFollowsHtml

      H.div ! A.class_ "unit three-of-four" $
        entryListHtml

      H.nav ! A.class_ "unit one-of-four home-sidebar" $ do
        H.div ! A.class_ "tile home-links" $
          linksHtml
        H.div ! A.class_ "tile home-tags" $
          tagsHtml


entryList :: [(D.Entity Entry,(T.Text,[Tag]))] -> PageDataMap -> Int -> SiteRender H.Html
entryList eList pageDataMap' pageNum = do
  tz <- liftIO getCurrentTimeZone

  return $ do
    H.div ! A.class_ "tile" $
      H.h2 ! A.class_ "recent-header" $ do
        "Recent Entries" :: H.Html
        when (pageNum > 1) $ do
          " (Page " :: H.Html
          H.toHtml pageNum
          ")" :: H.Html

    H.ul $
      forM_ eList $ \(D.Entity _ e,(u,ts)) -> do
        let
          commentUrl = T.append u "#disqus_thread"
        H.li $
          H.article ! A.class_ "tile" $ do
            H.header $ do
              Fo.forM_ (entryPostedAt e) $ \t ->
                H.time
                  ! A.datetime (I.textValue $ T.pack $ renderDatetimeTime t)
                  ! A.pubdate ""
                  ! A.class_ "pubdate"
                  $ H.toHtml $ renderFriendlyTime tz t
              H.h3 $
                H.a ! A.href (I.textValue u) $
                  H.toHtml $ entryTitle e

            H.div ! A.class_ "entry-lede copy-content" $ do
              entryLedeHtml e
              H.p $ do
                H.a ! A.href (I.textValue u) ! A.class_ "link-readmore" $
                  H.preEscapedToHtml
                    ("Read more &hellip; " :: T.Text)
                " " :: H.Html
                H.a ! A.href (I.textValue commentUrl) ! A.class_ "link-comment" $
                  "Comments"

            H.footer $
              H.ul ! A.class_ "tag-list" $
                forM_ ts $ \t ->
                  tagLi t

    let
      hasNextPrev = not $
        null $ L.intersect ["nextPage","prevPage"] $ M.keys pageDataMap'

    when hasNextPrev $
      H.footer ! A.class_ "tile home-footer" $
        H.nav $ do
          H.ul $ do
            Fo.forM_ (M.lookup "nextPage" pageDataMap') $ \nlink ->
              H.li ! A.class_ "home-next" $
                H.a ! A.href (I.textValue nlink) $
                  H.preEscapedToHtml ("&larr; Older" :: T.Text)

            Fo.forM_ (M.lookup "prevPage" pageDataMap') $ \plink ->
              H.li ! A.class_ "home-prev" $
                H.a ! A.href (I.textValue plink) $
                  H.preEscapedToHtml ("Newer &rarr;" :: T.Text)
          H.div ! A.class_ "clear" $ ""

viewLinks :: SiteRender H.Html
viewLinks = renderRawCopy "copy/static/home-links.md"

viewTags :: SiteRender H.Html
viewTags = do
  tags <- liftIO $ runDB $ getTagInfoList GeneralTag TagSortCount False
  cats <- liftIO $ runDB $ getTagInfoList CategoryTag TagSortLabel False
  let
    tagLists = [("Topics","/categories","home-category-list",cats)
               ,("Tags","/tags","home-tags-list",tags)]

  return $
    H.ul $
      forM_ tagLists $ \(heading,link,class_,tagList) ->
        H.li ! A.class_ class_ $ do
          H.h3 $
            H.a ! A.href (I.textValue $ renderUrl' link) $
              heading
          H.ul $
            forM_ tagList $ \tagInfo ->
              H.li $
                H.a ! A.href (I.textValue $ renderUrl' $ tagPath $ tagInfoTag tagInfo) $
                  H.toHtml $ tagLabel'' $ tagInfoTag tagInfo



