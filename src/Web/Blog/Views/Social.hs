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
  return $ do
    facebookLike url
    gPlusPlus
    facebookShare
    gPlusShare
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

facebookShare :: H.Html
facebookShare =
  H.a
    ! A.onclick (I.textValue fullJs)
    ! A.href "#"
    $ "Share on Facebook"
  where
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
    ! I.dataAttribute "annotation" "bubble"
    $ mempty

twitterShare :: H.Html
twitterShare =
  H.a
    ! A.href "https://twitter.com/share"
    ! A.class_ "twitter-share-button"
    ! I.dataAttribute "via" "mstk"
    -- ! I.dataAttribute "related" "recommends"
    -- ! I.dataAttribute "hashtags" "tags"
    -- ! I.dataAttribute "count" "none"
    $ "Tweet"

