{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blog.View.Social where

import           Blog.Types
import           Text.Blaze.Html5            ((!))
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as H

viewSocialShare :: H.Html
viewSocialShare =
    H.aside ! A.class_ "social-buttons" $ do
      addThisLine
      H.div ! A.class_ "custom-social-buttons" $ do
        H.div ! A.class_ "custom-social-button" $
          H.customParent (H.textTag "su:badge")
            ! H.customAttribute "layout" "1"
            $ mempty
        H.div ! A.class_ "custom-social-button" $
          H.a
            ! A.href "http://www.reddit.com/submit"
            ! A.onclick (
              H.textValue $
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
      ! H.customAttribute "fb:like:layout" "button_count"
      $ mempty
    H.a
      ! A.class_ "addthis_button_tweet"
      $ mempty
    H.a
      ! A.class_ "addthis_button_google_plusone"
      ! H.customAttribute "g:plusone:size" "medium"
      $ mempty
    H.a
      ! A.class_ "addthis_counter addthis_pill_style"
      $ mempty

viewSocialFollow :: (?config :: Config) => H.Html
viewSocialFollow =
    H.ul ! A.class_ "social-follows-list" $ do
      H.li $
        H.ul ! A.class_ "social-follows-list-social" $ do
          H.li $
            H.a
              ! A.class_ "social-follow-facebook"
              ! A.title "Friend me on Facebook!"
              ! A.href (H.textValue facebookUrl) $
              "Facebook"
          H.li $
            H.a
              ! A.class_ "social-follow-twitter"
              ! A.title "Follow me on Twitter!"
              ! A.href (H.textValue twitterUrl)
              ! A.onclick (H.textValue twitterFollowJs) $
              "Twitter"
          H.li $
            H.a
              ! A.class_ "social-follow-gplus"
              ! A.title "Add me on Google+!"
              ! A.href (H.textValue gPlusUrl) $
              "Google+"
          H.li $
            H.a
              ! A.class_ "social-follow-linkedin"
              ! A.title "Connect with me on LinkedIn!"
              ! A.href (H.textValue linkedInUrl) $
              "LinkedIn"
          H.li $
            H.a
              ! A.class_ "social-follow-github"
              ! A.title "Fork me on Github!"
              ! A.href (H.textValue githubUrl) $
              "Github"
      H.li $
        H.ul ! A.class_ "social-follows-list-site" $ do
          H.li $
            H.a
              ! A.class_ "social-follow-rss"
              ! A.title "Subscribe to my RSS Feed!"
              ! A.href (H.textValue rssUrl) $
              "RSS"
          H.li $
            H.a
              ! A.class_ "social-follow-email"
              ! A.title "Subscribe to the mailing list!"
              ! A.href (H.textValue emailUrl) $
              "Mailing list"
  where
    Config{..} = ?config
    twitterFollowJs = T.unlines
      [ "window.open("
      , "  'http://twitter.com/intent/user?user_id=907281',"
      , "  'facebook-share-dialog',"
      , "  'width=550,height=520');"
      , "return false;" ]
    socialUrl base field = T.append base . field $ confAuthorInfo
    facebookUrl = socialUrl "https://facebook.com/" authorFacebook
    twitterUrl = socialUrl "https://twitter.com/intent/user?user_id=" authorTwitter
    gPlusUrl = socialUrl "https://plus.google.com/" authorGPlus
    linkedInUrl = socialUrl "https://linkedin.com/in/" authorGithub
    githubUrl = socialUrl "https://github.com/" authorLinkedIn
    emailUrl = T.append "https://feedburner.google.com/fb/a/mailverify?loc=en_US&uri="
             $ devFeedburner confDeveloperAPIs
    rssUrl = confFeed
