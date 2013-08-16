{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Social (viewSocial) where

-- import Control.Applicative                ((<$>))
-- import Control.Monad.Reader
-- import Data.List                          (intersperse)
-- import Data.Maybe
-- import Data.Time                          (getCurrentTime)
-- import Text.Heredoc
-- import Web.Blog.Models
-- import Web.Blog.Models.Util
-- import Web.Blog.Util                      (renderFriendlyTime, renderDatetimeTime)
-- import qualified Data.Map                 as M
import Control.Monad.Trans
import Data.Monoid
import Text.Blaze.Html5                      ((!))
import Web.Blog.Render
import Web.Blog.Types
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as I

viewSocial :: SiteRender H.Html
viewSocial = do
  url <- lift getCurrUrl
  return $
    H.div ! A.class_ "social-buttons" $ do
      H.div! A.class_ "social-like" $ do
        H.div $
          facebookLike url
        H.div
          gPlusPlus
      H.div ! A.class_ "social-share" $ do
        H.div $
          facebookShare url
        H.div
          gPlusShare
        H.div
          twitterShare

facebookLike :: T.Text -> H.Html
facebookLike url =
  H.div
    ! A.class_ "fb-like"
    ! I.dataAttribute "href" (I.textValue url)
    ! I.dataAttribute "width" "450"
    ! I.dataAttribute "layout" "button_count"
    ! I.dataAttribute "show-faces" "true"
    ! I.dataAttribute "send" "false"
    $ mempty

gPlusPlus :: H.Html
gPlusPlus =
  H.div
    ! A.class_ "g-plusone"
    ! I.dataAttribute "size" "medium"
    $ mempty

facebookShare :: T.Text -> H.Html
facebookShare url =
  H.a
    ! A.onclick (I.textValue fullJs)
    ! A.class_ "facebook-share-link"
    ! A.href (I.textValue fullUrl)
    $ "Share on Facebook"
  where
    fullUrl = T.append "https://www.facebook.com/sharer/sharer.php?u=" url
    fullJs = T.unlines
      [ "window.open("
      , "  'https://www.facebook.com/sharer/sharer.php?u='+encodeURIComponent(location.href),"
      , "  'facebook-share-dialog',"
      , "  'width=626,height=436');"
      , "return false;" ]


gPlusShare :: H.Html
gPlusShare =
  H.div
    ! A.class_ "g-plus"
    ! I.dataAttribute "action" "share"
    -- ! I.dataAttribute "annotation" "bubble"
    ! I.dataAttribute "annotation" "none"
    $ mempty

twitterShare :: H.Html
twitterShare =
  H.a
    ! A.href "https://twitter.com/share"
    ! A.class_ "twitter-share-button"
    ! I.dataAttribute "via" "mstk"
    -- ! I.dataAttribute "related" "recommends"
    -- ! I.dataAttribute "hashtags" "tags"
    ! I.dataAttribute "count" "none"
    $ "Tweet"

