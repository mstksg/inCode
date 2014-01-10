{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.Entry (routeEntrySlug, routeEntryId) where

-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.Maybe
-- import Web.Blog.Database
-- import Web.Blog.Util
-- import qualified Data.Foldable as Fo         (forM_)
-- import qualified Data.Text                   as T
-- import qualified Text.Blaze.Html5            as H
-- import qualified Text.Blaze.Html5.Attributes as A
-- import qualified Text.Blaze.Internal         as I
import Control.Applicative                      ((<$>))
import Control.Monad.Reader
import Control.Monad.State
import Data.List                                (find, sortBy)
import Data.Maybe                               (listToMaybe)
import Data.Ord                                 (comparing)
import Data.Time
import Web.Blog.Models
import Web.Blog.Models.EntryI
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.Entry
import qualified Data.Foldable                  as Fo
import qualified Data.Map                       as M
import qualified Data.Text.Lazy                 as L
import qualified Database.Persist.Postgresql    as D
import qualified Web.Scotty                     as S

routeEntrySlug :: RouteDatabase
routeEntrySlug = do
  eIdent <- S.param "entryIdent"
  now <- liftIO getCurrentTime
  return $ readerEntrySlug eIdent now

routeEntryId :: RouteDatabase
routeEntryId = do
  eIdent <- S.param "entryIdent"
  now <- liftIO getCurrentTime
  return $ readerEntryId (read eIdent) now


readerEntrySlug :: L.Text -> UTCTime -> RouteReader
readerEntrySlug sText now = do
  (db, _) <- ask

  let
    slugs = M.elems . siteDatabaseSlugs $ db
    s = find ((==) sText . L.fromStrict . slugSlug) slugs

  case s of
    Just s' -> do
      let
        eKey = slugEntryId s'
        e   = eKey `M.lookup` siteDatabaseEntries db

      case e of
        Just e' -> do
          currSlug <- getCurrentSlugI eKey

          case currSlug of
            Just currSlug' ->
              if slugSlug currSlug' == slugSlug s'
                then readerEntry (eKey, e') now
                else siteLeft $
                  L.append "/entry/" (L.fromStrict . slugSlug $ currSlug')

            Nothing ->
              readerEntry (eKey, e') now

        Nothing ->
          error404 "SlugHasNoEntry"

    Nothing ->
      error404 "SlugNotFound"

readerEntryId :: Int -> UTCTime -> RouteReader
readerEntryId i now = do
  (db, _) <- ask

  let
    eKey = D.Key . D.PersistInt64 $ fromIntegral i
    e   = eKey `M.lookup` siteDatabaseEntries db

  case e of
    Just e' -> do
      currSlug <- getCurrentSlugI eKey

      case currSlug of
        Just currSlug' ->
          siteLeft $
            L.append "/entry/" (L.fromStrict . slugSlug $ currSlug')

        Nothing ->
          readerEntry (eKey, e') now

    Nothing ->
      error404 "entryIdNotFound"

readerEntry :: (KeyMapKey Entry, Entry) -> UTCTime -> RouteReader
readerEntry (k, e) now = do
  rd@(_, blankPageData) <- ask
  tags          <- getTagsI k
  (prev, next)  <- prevNext now e

  let
    pdMap = execState $ do
      Fo.forM_ prev $ \(k',_) ->
        modify
          ( M.insert
            "prevUrl"
            (runRouteReaderMRight (getUrlPathI k') rd)
          )

      Fo.forM_ next $ \(k',_) ->
        modify
          ( M.insert
            "nextUrl"
            (runRouteReaderMRight (getUrlPathI k') rd)
          )

    view = viewEntry e tags (snd <$> prev) (snd <$> next)

    pageData = blankPageData { pageDataTitle   = Just $ entryTitle e
                             , pageDataType    = Just "article"
                             , pageDataDesc    = Just $ entryLedeStripped e
                             , pageDataImage   = entryImage e
                             , pageDataCss     = ["/css/page/entry.css"
                                                 ,"/css/pygments.css"]
                             , pageDataJs      = ["/js/disqus.js"
                                                 ,"/js/disqus_count.js"
                                                 ,"/js/social.js"
                                                 ,"/js/jquery/jquery.toc.js"
                                                 ,"/js/page/entry.js"]
                             , pageDataMap     = pdMap M.empty
                             }


  siteRight (view, pageData)

prevNext :: UTCTime -> Entry ->
            RouteReaderM
                (Maybe (KeyMapPair Entry), Maybe (KeyMapPair Entry))
prevNext now e = do
  (db, _) <- ask
  posteds <- M.elems <$> postedEntriesI now
  let
    eTime = entryPostedAt e
    postedsDesc = sortBy (flip (comparing entryPostedAt)) posteds
    postedsAsc = sortBy (comparing entryPostedAt) posteds
    befores = dropWhile ((>= eTime) . entryPostedAt) postedsDesc
    afters = dropWhile ((<= eTime) . entryPostedAt) postedsAsc
    prev = do
      eIdent <- entryIdentifier <$> listToMaybe befores
      listToMaybe . M.toList $
        M.filter ((== eIdent) . entryIdentifier) (siteDatabaseEntries db)
    next = do
      eIdent <- entryIdentifier <$> listToMaybe afters
      listToMaybe . M.toList $
        M.filter ((== eIdent) . entryIdentifier) (siteDatabaseEntries db)

  return (prev, next)

