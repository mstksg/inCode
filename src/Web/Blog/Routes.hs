{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes (
  route
  ) where

-- import Control.Monad (when)
-- import Data.Maybe 
-- import qualified Database.Persist as D
import Control.Applicative ((<$>))
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Char (isDigit)
import Data.List (find)
import Data.Monoid
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Render
import Web.Blog.Routes.Entry
import Web.Blog.Routes.Home
import Web.Blog.Routes.NotFound
import Web.Blog.SiteData
import Web.Blog.Types
import Web.Blog.Views
import Network.HTTP.Types.Status
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Database.Persist.Postgresql as DP
import qualified Text.Blaze.Html5 as H
import qualified Web.Scotty as S

route :: S.ScottyM ()
route = do
  
  S.get "/" $ 
    routeEither routeHome

  S.get "/e/:entryIdent" $ do
    eIdent <- S.param "entryIdent"
    S.redirect $ L.append "/entry/" eIdent

  S.get "/e/id/:entryIdent" $ do
    eIdent <- S.param "entryIdent"
    S.redirect $ L.append "/entry/id/" eIdent

  S.get "/entry/id/:eId" $
    routeEither routeEntryId

  S.get "/entry/:entryIdent" $
    routeEither routeEntry 

  S.get "/not-found" $ do
    S.status notFound404
    routeEither routeNotFound

  S.notFound $
    S.redirect "/not-found"


routeEither :: RouteEither -> S.ActionM ()
routeEither r = do
  routeResult <- r
  case routeResult of
    Left re -> 
      -- TODO: get this status stuff working?
      -- if L.isPrefixOf "/not-found" re
      --   then
      --     S.status notFound404
      --   else
      --     S.status movedPermanently301
      S.redirect re
    Right (v,d) -> siteRenderActionLayout v d

siteRenderActionLayout :: SiteRender H.Html -> PageData -> S.ActionM ()
siteRenderActionLayout view = siteRenderAction (viewLayout view)

