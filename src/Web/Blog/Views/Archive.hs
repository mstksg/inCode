{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Archive (viewArchive) where

-- import Data.Maybe
-- import Data.Monoid
-- import Web.Blog.Render
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

viewArchive :: [(D.Entity Entry,(T.Text,[Tag]))] -> SiteRender H.Html
viewArchive eList = do
  pageDataMap' <- pageDataMap <$> ask

  return $ do

    H.header $
      H.h1 "Entries"
        


