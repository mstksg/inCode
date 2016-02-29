{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog.View.Home where

import Blog.Types
import Blog.View
import Text.Blaze.Html5                      ((!))
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as H
import qualified Text.Blaze.Internal         as H

data HomeInfo = HI
    { hiPageNum  :: Int
    , hiPrevPage :: Maybe T.Text
    , hiNextPage :: Maybe T.Text
    , hiBanner   :: Maybe H.Html
    , hiEntries  :: [HomeEntry]
    }

data HomeEntry = HE
    { heEntry :: Entry
    , heLink  :: T.Text
    , heTags  :: [Tag]
    }

viewHome :: (?config :: Config) => HomeInfo -> H.Html
viewHome HI{..} =
    H.section ! H.class_ "home-section" ! mainSection $ do

      H.header ! H.class_ "tile unit span-grid" $
        H.section ! H.class_ "home-banner" $ do
          if hiPageNum == 1
            then
              bannerCopy
            else
              H.h1 ! H.class_ "home-banner-history" $
                H.a ! H.href (H.textValue (renderUrl "/")) $
                  H.toHtml (confTitle ?config)

          H.aside ! H.class_ "social-follows" $ do
            "Follow me on: " :: H.Html
            socialFollowsHtml

      H.div ! H.class_ "unit three-of-four" $
        entryListHtml

      H.nav ! H.class_ "unit one-of-four home-sidebar" $ do
        H.div ! H.class_ "tile home-links" $
          linksHtml
        H.div ! H.class_ "tile home-tags" $
          tagsHtml
  where
  -- bannerCopy <- viewCopyFile (siteDataTitle siteData) "copy/static/home-banner.md"
    bannerCopy = mempty
    socialFollowsHtml = mempty
    entryListHtml = mempty
    linksHtml = mempty
    tagsHtml = mempty

  -- pageDataMap' <- pageDataMap <$> ask
  -- linksHtml <- viewLinks
  -- tagsHtml <- viewTags
  -- homeUrl <- renderUrl "/"
  -- socialFollowsHtml <- viewSocialFollow
  -- entryListHtml <- entryList eList pageDataMap' pageNum




-- entryList :: [(KeyMapPair Entry,(T.Text,[Tag]))] -> PageDataMap -> Int -> SiteRender H.Html
-- entryList eList pageDataMap' pageNum = do
--   tz <- liftIO getCurrentTimeZone

--   return $ do
--     H.div ! H.class_ "tile" $
--       H.h2 ! H.class_ "recent-header" $ do
--         "Recent Entries" :: H.Html
--         when (pageNum > 1) $ do
--           " (Page " :: H.Html
--           H.toHtml pageNum
--           ")" :: H.Html

--     H.ul $
--       forM_ eList $ \((_,e),(u,ts)) -> do
--         let
--           commentUrl = T.append u "#disqus_thread"
--         H.li $
--           H.article ! H.class_ "tile" $ do
--             H.header $ do
--               Fo.forM_ (entryPostedAt e) $ \t ->
--                 H.time
--                   ! H.datetime (H.textValue $ T.pack $ renderDatetimeTime t)
--                   ! H.pubdate ""
--                   ! H.class_ "pubdate"
--                   $ H.toHtml $ renderFriendlyTime tz t
--               H.h3 $
--                 H.a ! H.href (H.textValue u) $
--                   H.toHtml $ entryTitle e

--             H.div ! H.class_ "entry-lede copy-content" $ do
--               entryLedeHtml e
--               H.p $ do
--                 H.a ! H.href (H.textValue u) ! H.class_ "link-readmore" $
--                   H.preEscapedToHtml
--                     ("Read more &hellip; " :: T.Text)
--                 " " :: H.Html
--                 H.a ! H.href (H.textValue commentUrl) ! H.class_ "link-comment" $
--                   "Comments"

--             H.footer $
--               H.ul ! H.class_ "tag-list" $
--                 forM_ ts $ \t ->
--                   tagLi t

--     let
--       hasNextPrev = not $
--         null $ L.intersect ["nextPage","prevPage"] $ M.keys pageDataMap'

--     when hasNextPrev $
--       H.footer ! H.class_ "tile home-footer" $
--         H.nav $ do
--           H.ul $ do
--             Fo.forM_ (M.lookup "nextPage" pageDataMap') $ \nlink ->
--               H.li ! H.class_ "home-next" $
--                 H.a ! H.href (H.textValue nlink) $
--                   H.preEscapedToHtml ("&larr; Older" :: T.Text)

--             Fo.forM_ (M.lookup "prevPage" pageDataMap') $ \plink ->
--               H.li ! H.class_ "home-prev" $
--                 H.a ! H.href (H.textValue plink) $
--                   H.preEscapedToHtml ("Newer &rarr;" :: T.Text)
--           H.div ! H.class_ "clear" $ ""

-- viewLinks :: SiteRender H.Html
-- viewLinks = renderRawCopy "copy/static/home-links.md"

-- viewTags :: SiteRender H.Html
-- viewTags = do
--   tags <- liftIO $ runDB $ getTagInfoList GeneralTag TagSortCount False
--   cats <- liftIO $ runDB $ getTagInfoList CategoryTag TagSortLabel False
--   let
--     tagLists = [("Topics","/categories","home-category-list",cats)
--                ,("Tags","/tags","home-tags-list",tags)]

--   return $
--     H.ul $
--       forM_ tagLists $ \(heading,link,class_,tagList) ->
--         H.li ! H.class_ class_ $ do
--           H.h3 $
--             H.a ! H.href (H.textValue $ renderUrl' link) $
--               heading
--           H.ul $
--             forM_ tagList $ \(TagInfo t d _) ->
--               H.li $ do
--                 H.a
--                   ! H.href (H.textValue $
--                     renderUrl' $ tagPath t )
--                   ! H.title (H.textValue $
--                     fromMaybe mempty (tagDescriptionStripped t))
--                   $ H.toHtml $ tagLabel'' t
--                 H.preEscapedToHtml ("&nbsp;" :: T.Text)
--                 H.span $ do
--                   "(" :: H.Html
--                   H.toHtml d
--                   ")" :: H.Html
