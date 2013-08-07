{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.TagIndex (
    routeTagIndex
  ) where

-- import Control.Applicative                ((<$>))
-- import Control.Monad.Reader
-- import Control.Monad.State
-- import Control.Monad.Trans
-- import Control.Monad.Trans                (lift)
-- import Data.Char                          (isDigit)
-- import Web.Blog.SiteData
-- import qualified Data.Text.Lazy           as L
-- import qualified Text.Blaze.Html5         as H
-- import qualified Web.Scotty               as S
import Control.Monad.IO.Class
import Data.Time
import System.Locale
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Models.Types
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.TagIndex
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Database.Persist.Postgresql as D

routeTagIndex :: TagType -> RouteEither
routeTagIndex tt = do
  tagInfos <- liftIO $ runDB $ getTagInfoList tt

  let
    view = viewTagIndex tagInfos tt

  return $ Right (view,pageData)
