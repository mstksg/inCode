{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Social (viewSocialShare, viewSocialFollow) where

-- import Control.Applicative                ((<$>))
-- import Control.Monad.Reader
-- import Control.Monad.Trans
-- import Data.List                          (intersperse)
-- import Data.Maybe
-- import Data.Time                          (getCurrentTime)
-- import Text.Heredoc
-- import Web.Blog.Models
-- import Web.Blog.Models.Util
-- import Web.Blog.Util                      (renderFriendlyTime, renderDatetimeTime)
-- import qualified Data.Map                 as M
import Data.Monoid
import Text.Blaze.Html5                      ((!))
import Web.Blog.Render
import Web.Blog.SiteData
import Web.Blog.Types
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as I

viewSocialShare :: SiteRender H.Html
viewSocialShare = return $
  H.aside ! A.class_ "social-buttons" $ do
    addThisLine
    H.div ! A.class_ "custom-social-buttons" $ do
      H.div ! A.class_ "custom-social-button" $
        I.customParent (I.textTag "su:badge")
          ! I.customAttribute "layout" "1"
          $ mempty
      H.div ! A.class_ "custom-social-button" $
        H.a
          ! A.href "http://www.reddit.com/submit"
          ! A.onclick (
            I.textValue $
              T.concat
                [ "window.location = 'http://www.reddit.com/submit?url='"
                , "+ encodeURIComponent(window.location); return false"])
          $ H.img
            ! A.src "http://www.reddit.com/static/spreddit7.gif"
            ! A.alt "submit to reddit"

addThisLine :: H.Html
addThisLine =
  H.div ! A.class_ "addthis_toolbox addthis_default_style addthis-buttons" $ do
    H.a
      ! A.class_ "addthis_button_facebook_like"
      ! I.customAttribute "fb:like:layout" "button_count"
      $ mempty
    H.a
      ! A.class_ "addthis_button_tweet"
      $ mempty
    H.a
      ! A.class_ "addthis_button_google_plusone"
      ! I.customAttribute "g:plusone:size" "medium"
      $ mempty
    H.a
      ! A.class_ "addthis_counter addthis_pill_style"
      $ mempty

-- viewSocialFollow' :: SiteRender H.Html
-- viewSocialFollow' = return $
--   H.div ! A.class_ "addthis_toolbox addthis_default_style" $ do
--     H.a
--       ! A.class_ "addthis_button_facebook_follow"
--       ! I.customAttribute "addthis:userid" "mstksg"
--       $ mempty
--     H.a
--       ! A.class_ "addthis_button_twitter_follow"
--       ! I.customAttribute "addthis:userid" "mstk"
--       $ mempty
--     H.a
--       ! A.class_ "addthis_button_linkedin_follow"
--       ! I.customAttribute "addthis:userid" "lejustin"
--       $ mempty
--     H.a
--       ! A.class_ "addthis_button_google_follow"
--       ! I.customAttribute "addthis:userid" "107705320197444500140"
--       $ mempty

viewSocialFollow :: SiteRender H.Html
viewSocialFollow = do
  rssUrl <- renderUrl "/rss"
  return $
    H.ul ! A.class_ "social-follows-list" $ do
      H.li $
        H.a
          ! A.class_ "social-follow-facebook"
          ! A.title "Friend me on Facebook!"
          ! A.href "http://www.facebook.com/mstksg" $
          "Facebook"
      H.li $
        H.a
          ! A.class_ "social-follow-twitter"
          ! A.title "Follow me on Twitter!"
          ! A.href "https://twitter.com/intent/user?user_id=907281"
          ! A.onclick (I.textValue twitterFollowJs) $
          "Twitter"
      H.li $
        H.a
          ! A.class_ "social-follow-gplus"
          ! A.title "Follow me on Google+!"
          ! A.href "https://plus.google.com/107705320197444500140" $
          "Google+"
      H.li $
        H.a
          ! A.class_ "social-follow-linkedin"
          ! A.title "Connect with me on LinkedIn!"
          ! A.href "http://www.linkedin.com/in/lejustin" $
          "LinkedIn"
      H.li $
        H.a
          ! A.class_ "social-follow-rss"
          ! A.title "Subscribe to my RSS Feed!"
          ! A.href (I.textValue rssUrl) $
          "RSS"
      H.li $
        H.a
          ! A.class_ "social-follow-email"
          ! A.title "Subscribe to the mailing list!"
          ! A.href (I.textValue emailUrl) $
          "Mailing list"
  where
    twitterFollowJs = T.unlines
      [ "window.open("
      , "  'http://twitter.com/intent/user?user_id=907281',"
      , "  'facebook-share-dialog',"
      , "  'width=550,height=520');"
      , "return false;" ]
    emailUrl = T.append
      "http://feedburner.google.com/fb/a/mailverify?loc=en_US&uri=" $
      developerAPIsFeedburner $ siteDataDeveloperAPIs siteData

-- viewSocial :: SiteRender H.Html
-- viewSocial = do
--   url <- lift getCurrUrl
--   return $
--     H.aside ! A.class_ "social-buttons" $ do
--       H.div! A.class_ "social-like" $ do
--         H.div $
--           facebookLike url
--         H.div
--           gPlusPlus
--       H.div ! A.class_ "social-share" $ do
--         H.div $
--           facebookShare url
--         H.div
--           gPlusShare
--         H.div
--           twitterShare

-- facebookLike :: T.Text -> H.Html
-- facebookLike url =
--   H.div
--     ! A.class_ "fb-like"
--     ! I.dataAttribute "href" (I.textValue url)
--     ! I.dataAttribute "width" "450"
--     ! I.dataAttribute "layout" "button_count"
--     ! I.dataAttribute "show-faces" "true"
--     ! I.dataAttribute "send" "false"
--     $ mempty

-- gPlusPlus :: H.Html
-- gPlusPlus =
--   H.div
--     ! A.class_ "g-plusone"
--     ! I.dataAttribute "size" "medium"
--     $ mempty

-- facebookShare :: T.Text -> H.Html
-- facebookShare url =
--   H.a
--     ! A.onclick (I.textValue fullJs)
--     ! A.class_ "facebook-share-link"
--     ! A.href (I.textValue fullUrl)
--     $ "Share on Facebook"
--   where
--     fullUrl = T.append "https://www.facebook.com/sharer/sharer.php?u=" url
--     fullJs = T.unlines
--       [ "window.open("
--       , "  'https://www.facebook.com/sharer/sharer.php?u='+encodeURIComponent(location.href),"
--       , "  'facebook-share-dialog',"
--       , "  'width=626,height=436');"
--       , "return false;" ]


-- gPlusShare :: H.Html
-- gPlusShare =
--   H.div
--     ! A.class_ "g-plus"
--     ! I.dataAttribute "action" "share"
--     -- ! I.dataAttribute "annotation" "bubble"
--     ! I.dataAttribute "annotation" "none"
--     $ mempty

-- twitterShare :: H.Html
-- twitterShare =
--   H.a
--     ! A.href "https://twitter.com/share"
--     ! A.class_ "twitter-share-button"
--     ! I.dataAttribute "via" "mstk"
--     -- ! I.dataAttribute "related" "recommends"
--     -- ! I.dataAttribute "hashtags" "tags"
--     ! I.dataAttribute "count" "none"
--     $ "Tweet"

