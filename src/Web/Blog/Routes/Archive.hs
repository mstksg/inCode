{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.Archive (routeArchive) where

-- import Control.Monad.Reader
-- import Control.Monad.Trans
import Control.Applicative                   ((<$>))
import Control.Monad.IO.Class
import Control.Monad.Trans                   (lift)
import Data.Char                             (isDigit)
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Views.Archive
import Web.Blog.SiteData
import Web.Blog.Types
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as L
import qualified Database.Persist.Postgresql as D
import qualified Text.Blaze.Html5            as H
import qualified Web.Scotty                  as S
import qualified Data.Map as M
import Control.Monad.State

routeArchive :: RouteEither
routeArchive = do
  eList <- liftIO $ runDB $
    postedEntries [] >>= mapM wrapEntryData

  let
    view = viewArchive eList
    pageData' = pageData { pageDataTitle = Just "Entries" }

  return $ Right (view, pageData)


