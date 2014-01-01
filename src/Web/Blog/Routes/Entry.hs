{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.Entry (routeEntrySlug, routeEntryId) where

-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.Maybe
-- import Web.Blog.Database
-- import Web.Blog.Util
-- import qualified Data.Foldable as Fo         (forM_)
-- import qualified Text.Blaze.Html5            as H
-- import qualified Text.Blaze.Html5.Attributes as A
-- import qualified Text.Blaze.Internal         as I
import Control.Applicative                      ((<$>))
import Control.Monad.Reader
import Control.Monad.State
import Data.List                                (find, sortBy)
import Data.Maybe                               (fromJust, listToMaybe)
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
import qualified Data.Text                      as T
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
          let
            currSlug = getCurrentSlugI eKey db

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
      let
        currSlug = getCurrentSlugI eKey db

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
  (db, blankPageData) <- ask
  (prev, next) <- prevNext now

  let
    tags = getTagsI k db

    pdMap = execState $ do
      Fo.forM_ prev $ \(k',_) ->
        modify (M.insert ("prevUrl" :: T.Text) (getUrlPathI k' db))

      Fo.forM_ next $ \(k',_) ->
        modify (M.insert ("nextUrl" :: T.Text) (getUrlPathI k' db))



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

prevNext :: UTCTime ->
            RouteReaderM
                (Maybe (KeyMapPair Entry), Maybe (KeyMapPair Entry))
prevNext now = do
  (db, _) <- ask
  let
    posteds = M.elems $ postedEntriesI now db
    postedsDesc = sortBy (flip (comparing (fromJust . entryPostedAt))) posteds
    postedsAsc = sortBy (comparing (fromJust . entryPostedAt)) posteds
    befores = dropWhile ((> now) . fromJust . entryPostedAt) postedsDesc
    afters = dropWhile ((< now) . fromJust . entryPostedAt) postedsAsc
    prev = do
      eIdent <- entryIdentifier <$> listToMaybe befores
      listToMaybe . M.toList $
        M.filter ((== eIdent) . entryIdentifier) (siteDatabaseEntries db)
    next = do
      eIdent <- entryIdentifier <$> listToMaybe afters
      listToMaybe . M.toList $
        M.filter ((== eIdent) . entryIdentifier) (siteDatabaseEntries db)

  return (prev, next)

