{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Archive (viewArchive) where

import Data.Maybe (fromMaybe)
-- import Data.Monoid
-- import Web.Blog.Render
import Control.Applicative                   ((<$>))
import Control.Monad.Reader
import Text.Blaze.Html5                      ((!))
import Web.Blog.Models
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.SiteData
import Web.Blog.Types
import Web.Blog.Util                         (renderFriendlyTime, renderDatetimeTime)
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Database.Persist.Postgresql as D
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as I

viewArchive :: [(D.Entity Entry,(T.Text,[Tag]))] -> SiteRender H.Html
viewArchive eList = do
  pageTitle <- pageDataTitle <$> ask

  return $ do

    H.header $
      H.h1 $ H.toHtml $ fromMaybe "Entries" pageTitle

    H.div $ 
      
      forM_ eList $ \eData -> do
        let
          (D.Entity _ e,(u,ts)) = eData

        H.article $ do

          H.a ! A.href (I.textValue u) $
            H.h2 $ H.toHtml $ entryTitle e

          H.time
            ! A.datetime (I.textValue $ T.pack $ renderDatetimeTime $ entryPostedAt e)
            ! A.pubdate "" 
            ! A.class_ "pubdate"
            $ H.toHtml $ renderFriendlyTime $ entryPostedAt e

          H.ul $
            forM_ ts $ \t ->
              tagLi t

        


