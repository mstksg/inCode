{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes (
  route
  ) where

-- import Control.Monad (when)
-- import Data.Maybe 
import Data.Monoid
-- import qualified Database.Persist as D
import Control.Applicative ((<$>))
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Char (isDigit)
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Render
import Web.Blog.Routes.Entry
import Web.Blog.Routes.Home
import Web.Blog.SiteData
import Web.Blog.Types
import Web.Blog.Views
import qualified Web.Scotty as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Database.Persist.Postgresql as DP
import qualified Text.Blaze.Html5 as H

route :: S.ScottyM ()
route = do
  
  S.get "/" $ 
    routeEither routeHome

  S.get "/entry/:entryIdent" $
    routeEither routeEntry 

  S.get "/not-found" $
    siteRenderAction viewLayoutEmpty pageData

  S.notFound $ S.redirect "/not-found"

routeEither :: RouteEither -> S.ActionM ()
routeEither r = do
  routeResult <- r
  case routeResult of
    Left re -> S.redirect re
    Right (v,d) -> siteRenderActionLayout v d

siteRenderActionLayout :: SiteRender H.Html -> PageData -> S.ActionM ()
siteRenderActionLayout view = siteRenderAction (viewLayout view)

