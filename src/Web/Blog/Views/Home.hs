{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Home (viewHome) where

import Control.Applicative                   ((<$>))
import Control.Monad.Reader
import Text.Blaze.Html5                      ((!))
import Web.Blog.Models
import Web.Blog.Models.Util
import Web.Blog.SiteData
import Web.Blog.Types
import Web.Blog.Util                         (renderFriendlyTime, renderDatetimeTime)
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Database.Persist.Postgresql as D
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as I

viewHome :: [(D.Entity Entry,(T.Text,[Tag]))] -> SiteRender H.Html
viewHome eList = do
  pageDataMap' <- pageDataMap <$> ask

  return $ 
    H.section ! A.class_ "home-section" $ do

      H.header ! A.class_ "tile" $ 

        H.section $ do
          H.h1 $ H.toHtml $ siteDataTitle siteData
          H.p
            "Welcome to my blog."

      H.ul $
        forM_ eList $ \eData -> do
          let
            (D.Entity _ e,(u,ts)) = eData
            commentUrl = T.append u "#disqus_thread"

          H.li $
            H.article ! A.class_ "tile" $ do

              H.header $ do
                H.time
                  ! A.datetime (I.textValue $ T.pack $ renderDatetimeTime $ entryPostedAt e)
                  ! A.pubdate "" 
                  ! A.class_ "pubdate"
                  $ H.toHtml $ renderFriendlyTime $ entryPostedAt e

                H.h2 $ 
                  H.a ! A.href (I.textValue u) $
                    H.toHtml $ entryTitle e


              H.div ! A.class_ "entry-lede copy-content" $ do
                entryLedeHtml e
                H.p $ do
                  H.a ! A.href (I.textValue u) ! A.class_ "link-readmore" $
                    "Read more"
                  " " :: H.Html
                  H.a ! A.href (I.textValue commentUrl) ! A.class_ "link-comment" $
                    ""

              H.footer $
                H.ul ! A.class_ "tag-list" $
                  forM_ ts $ \t ->
                    tagLi t


      H.footer ! A.class_ "tile" $ 

        H.nav $ do
          H.ul $ do

            case M.lookup "nextPage" pageDataMap' of
              Just nlink -> 
                H.li ! A.class_ "home-next" $
                  H.a ! A.href (I.textValue nlink) $
                    H.preEscapedToHtml ("&larr; Older" :: T.Text)
              _ -> return ()

            case M.lookup "prevPage" pageDataMap' of
              Just plink -> 
                H.li ! A.class_ "home-prev" $
                  H.a ! A.href (I.textValue plink) $
                    H.preEscapedToHtml ("Newer &rarr;" :: T.Text)
              _ -> return ()

          H.div ! A.class_ "clear" $ ""

