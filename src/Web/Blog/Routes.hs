{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes (route) where

import Control.Monad.Reader
-- import Data.List                  (isSuffixOf)
import Network.HTTP.Types.Status
-- import System.Directory           (doesFileExist)
-- import System.FilePath
import Web.Blog.SiteData
import Web.Blog.Models.Types
import Web.Blog.Render
import Web.Blog.Routes.About
import Web.Blog.Routes.Archive
import Web.Blog.Routes.Entry
import Web.Blog.Routes.Home
import Web.Blog.Routes.Feed
import Web.Blog.Routes.NotFound
import Web.Blog.Routes.TagIndex
import Web.Blog.Types
import Web.Blog.Views.Layout
import qualified Data.Text        as T
import qualified Data.Text.Lazy   as L
import qualified Text.Blaze.Html5 as H
import qualified Web.Scotty       as S


route :: S.ScottyM ()
route = do
  homeRoutes
  entryRoutes
  archiveRoutes
  indexRoutes
  utilRoutes
  miscRoutes


homeRoutes :: S.ScottyM ()
homeRoutes = do
  S.get "/" $
    routeEither $ routeHome 1

  forM_ ["/home","/home/1"] $ \r ->
    S.get r $
      routeEither $ return $ Left "/"

  S.get "/home/:page" $ do
    page <- S.param "page"
    when (page < 1) S.next
    routeEither $ routeHome page

  S.get "/about" $
    routeEither routeAbout

entryRoutes :: S.ScottyM ()
entryRoutes = do
  forM_ ["/","/id"] $ \r -> do
    let
      cap = "/e" ++ (L.unpack r ++ "/:entryIdent")
      red ident = L.append "/entry" $ L.append r $ L.pack ident

    S.get (S.capture cap) $ do
      eIdent <- S.param "entryIdent"
      S.redirect $ red eIdent

  S.get "/entry/id/:eId" $
    routeEither routeEntryId

  S.get "/entry/:entryIdent" $
    routeEither routeEntrySlug

archiveRoutes :: S.ScottyM ()
archiveRoutes = do
  S.get "/entries" $
    routeEither routeArchiveAll

  S.get (S.regex "^/entries/category/@(.*)$") $ do
    category <- S.param "1"
    routeEither $ routeArchiveTag CategoryTag $ T.pack category

  S.get (S.regex "^/entries/series/\\+(.*)$") $ do
    series <- S.param "1"
    routeEither $ routeArchiveTag SeriesTag $ T.pack series

  S.get "/entries/tagged/:tag" $ do
    tag <- S.param "tag"
    routeEither $ routeArchiveTag GeneralTag $ T.pack tag

  S.get "/entries/in" $
    routeEither $ return $ Left "/entries"

  S.get "/entries/in/:year" $ do
    year <- S.param "year"
    when (year < 1) S.next
    routeEither $ routeArchiveYear year

  S.get "/entries/in/:year/:month" $ do
    year <- S.param "year"
    month <- S.param "month"
    when (year < 1) S.next
    when (month < 1 || month > 12) S.next
    routeEither $ routeArchiveMonth year month

indexRoutes :: S.ScottyM ()
indexRoutes = do
  S.get "/tags" $
    routeEither $ routeTagIndex GeneralTag

  S.get "/categories" $
    routeEither $ routeTagIndex CategoryTag

  S.get "/series" $
    routeEither $ routeTagIndex SeriesTag

utilRoutes :: S.ScottyM ()
utilRoutes = do

  S.get "/rss" $ do
    S.status movedPermanently301
    S.redirect $ L.append
      "http://feeds.feedburner.com/" $
      L.fromStrict $ developerAPIsFeedburner $ siteDataDeveloperAPIs siteData
  S.get "/rss.raw" $ do
    -- now <- liftIO getCurrentTime
    (v,d) <- routeFeed
    ran <- runReaderT v d
    S.text ran
    S.header "Content-Type" "application/rss+xml"




miscRoutes :: S.ScottyM ()
miscRoutes = do
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

