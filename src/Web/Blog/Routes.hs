module Web.Blog.Routes (route) where

import "base" Prelude
import Config.SiteData
import Control.Monad.Reader
import Development.Blog.Util      (backupEntries)
import Network.HTTP.Types.Status
import Web.Blog.Models.Types
import Web.Blog.Render
import Web.Blog.Routes.About
import Web.Blog.Routes.Archive
import Web.Blog.Routes.Entry
import Web.Blog.Routes.Feed
import Web.Blog.Routes.Home
import Web.Blog.Routes.NotFound
import Web.Blog.Routes.TagIndex
import Web.Blog.Types
import Web.Blog.Views.Layout
import qualified Data.Text        as T
import qualified Data.Text.Lazy   as L
import qualified Text.Blaze.Html5 as H
import qualified Web.Scotty       as S


route :: SiteDatabase -> S.ScottyM ()
route db = do
  homeRoutes db
  entryRoutes db
  archiveRoutes db
  indexRoutes db
  utilRoutes db
  miscRoutes db


homeRoutes :: SiteDatabase -> S.ScottyM ()
homeRoutes db = do
  S.get "/" $
    routeDatabase db $ routeHome 1

  forM_ ["/home","/home/1"] $ \r ->
    S.get r $
      routeDatabase db . return $ siteLeft "/"

  S.get "/home/:page" $ do
    page <- S.param "page"
    when (page < 1) S.next
    routeDatabase db $ routeHome page

  S.get "/about" $
    routeDatabase db routeAbout

entryRoutes :: SiteDatabase -> S.ScottyM ()
entryRoutes db = do
  forM_ ["/","/id"] $ \r -> do
    let
      cap = "/e" ++ (L.unpack r ++ "/:entryIdent")
      red ident = L.append "/entry" $ L.append r $ L.pack ident

    S.get (S.capture cap) $ do
      eIdent <- S.param "entryIdent"
      permanentRedirect $ red eIdent

  S.get "/entry/id/:eId" $
    routeDatabase db routeEntryId

  S.get "/entry/:entryIdent" $
    routeDatabase db routeEntrySlug

archiveRoutes :: SiteDatabase -> S.ScottyM ()
archiveRoutes db = do
  S.get "/entries" $
    routeDatabase db routeArchiveAll

  S.get (S.regex "^/entries/category/@(.*)$") $ do
    category <- S.param "1"
    routeDatabase db $ routeArchiveTag CategoryTag $ T.pack category

  S.get (S.regex "^/entries/series/\\+(.*)$") $ do
    series <- S.param "1"
    routeDatabase db $ routeArchiveTag SeriesTag $ T.pack series

  S.get "/entries/tagged/:tag" $ do
    tag <- S.param "tag"
    routeDatabase db $ routeArchiveTag GeneralTag $ T.pack tag

  S.get "/entries/in" $
    routeDatabase db . return $ siteLeft "/entries"

  S.get "/entries/in/:year" $ do
    year <- S.param "year"
    when (year < 1) S.next
    routeDatabase db $ routeArchiveYear year

  S.get "/entries/in/:year/:month" $ do
    year <- S.param "year"
    month <- S.param "month"
    when (year < 1) S.next
    when (month < 1 || month > 12) S.next
    routeDatabase db $ routeArchiveMonth year month

indexRoutes :: SiteDatabase -> S.ScottyM ()
indexRoutes db = do
  S.get "/tags" $
    routeDatabase db $ routeTagIndex GeneralTag

  S.get "/categories" $
    routeDatabase db $ routeTagIndex CategoryTag

  S.get "/series" $
    routeDatabase db $ routeTagIndex SeriesTag

utilRoutes :: SiteDatabase -> S.ScottyM ()
utilRoutes _ = do

  S.get "/rss" $ do
    S.status movedPermanently301
    S.header "Location" $ L.append
      "http://feeds.feedburner.com/" $
      L.fromStrict $
        developerAPIsFeedburner $ siteDataDeveloperAPIs siteData
  S.get "/rss.raw" $ do
    -- now <- liftIO getCurrentTime
    (v,d) <- routeFeed
    ran <- runReaderT v d
    S.text ran
    S.header "Content-Type" "application/rss+xml"

  S.get "/entry-backups" $ do
    b <- liftIO backupEntries
    S.text $ L.fromStrict b

  S.get (S.regex "/source/(.*)$") $
    case siteDataPublicBlobs siteData of
      Nothing -> permanentRedirect "/not-found"
      Just blob -> do
        p <- S.param "1"
        S.status movedPermanently301
        S.header "Location" $ L.append
          (L.fromStrict blob)
          p




miscRoutes :: SiteDatabase -> S.ScottyM ()
miscRoutes db = do
  S.get "/not-found" $ do
    S.status notFound404
    routeDatabase db routeNotFound

  S.notFound $
    permanentRedirect "/not-found"

routeDatabase :: SiteDatabase -> RouteDatabase -> S.ActionM ()
routeDatabase db r = do
  routeResult <- r
  blankPageData <- genPageData

  case runReaderT routeResult (db, blankPageData) of
    Left re -> do
      S.status $
        if "/not-found" `L.isPrefixOf` re
          then
            notFound404
          else
            movedPermanently301
      url <- extractSiteRender $ renderUrl $ L.toStrict re
      S.header "Location" $ L.fromStrict url
    Right (v,d) -> siteRenderActionLayout v d


-- routeEither :: RouteEither -> S.ActionM ()
-- routeEither r = do
--   routeResult <- r
--   case routeResult of
--     Left re -> do
--       S.status $
--         if "/not-found" `L.isPrefixOf` re
--           then
--             notFound404
--           else
--             movedPermanently301

      -- url <- extractSiteRender $ renderUrl $ L.toStrict re
      -- S.header "Location" $ L.fromStrict url
    -- Right (v,d) -> siteRenderActionLayout v d

siteRenderActionLayout :: SiteRender H.Html -> PageData -> S.ActionM ()
siteRenderActionLayout view = siteRenderAction (viewLayout view)

permanentRedirect :: L.Text -> S.ActionM ()
permanentRedirect url = do
  S.status movedPermanently301
  S.header "Location" url

