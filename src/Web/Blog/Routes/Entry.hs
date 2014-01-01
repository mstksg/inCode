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
import Control.Applicative                      ((<$>),(<*>))
import Control.Monad.Reader
import Control.Monad.State
import Data.List                                (find)
import Data.Maybe                               (isJust, fromJust, mapMaybe)
import Web.Blog.Models
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.Entry
import qualified Data.Map                       as M
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as L
import qualified Database.Persist.Postgresql    as D
import qualified Web.Scotty                     as S

routeEntrySlug :: RouteDatabase
routeEntrySlug = do
  eIdent <- S.param "entryIdent"
  return $ readerEntrySlug eIdent

routeEntryId :: RouteDatabase
routeEntryId = do
  eIdent <- S.param "entryIdent"
  return $ readerEntryId (read eIdent)


readerEntrySlug :: L.Text -> RouteReader
readerEntrySlug sText = do
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
            matchingSlug = (==) eKey . slugEntryId
            currSlug = find ((&&) <$> slugIsCurrent <*> matchingSlug) slugs

          case currSlug of
            Just currSlug' ->
              if slugSlug currSlug' == slugSlug s'
                then readerEntry (eKey, e')
                else siteLeft $
                  L.append "/entry/" (L.fromStrict . slugSlug $ currSlug')

            Nothing ->
              readerEntry (eKey, e')

        Nothing ->
          lift . Left $ "SlugHasNoEntry"

    Nothing ->
      error404 "SlugNotFound"

readerEntryId :: Int -> RouteReader
readerEntryId i = do
  (db, _) <- ask

  let
    slugs = M.elems $ siteDatabaseSlugs db
    eKey = D.Key . D.PersistInt64 $ fromIntegral i
    e   = eKey `M.lookup` siteDatabaseEntries db

  case e of
    Just e' -> do
      let
        matchingSlug = (==) eKey . slugEntryId
        currSlug = find ((&&) <$> slugIsCurrent <*> matchingSlug) slugs

      case currSlug of
        Just currSlug' ->
          siteLeft $
            L.append "/entry/" (L.fromStrict . slugSlug $ currSlug')

        Nothing ->
          readerEntry (eKey, e')

    Nothing ->
      error404 "entryIdNotFound"

readerEntry :: (KeyMapKey Entry, Entry) -> RouteReader
readerEntry (k, e) = do
  (db, blankPageData) <- ask

  let
    matchingEntryTag = (==) k . entryTagEntryId
    entryTags = filter matchingEntryTag (M.elems . siteDatabaseEntryTags $ db)
    tagIds = map entryTagTagId entryTags
    tags = mapMaybe (`M.lookup` siteDatabaseTags db) tagIds
    prevData = Nothing
    nextData = Nothing

    pdMap = execState $ do
      when (isJust prevData) $ do
        let prevUrl = snd $ fromJust prevData
        modify (M.insert ("prevUrl" :: T.Text) prevUrl)

      when (isJust nextData) $ do
        let nextUrl = snd $ fromJust nextData
        modify (M.insert ("nextUrl" :: T.Text) nextUrl)

    view = viewEntry e tags (fst <$> prevData) (fst <$> nextData)

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



-- entryAux :: D.Key Entry -> Entry -> D.SqlPersistM ([Tag],Maybe (Entry, T.Text),Maybe (Entry, T.Text))
-- entryAux k e = do
--   tags <- getTagsByEntityKey k []

--   prevData <- runMaybeT $ do
--     prev <- MaybeT $ getPrevEntry e
--     prevUrl <- lift $ getUrlPath prev
--     lift $ return (D.entityVal prev, prevUrl)

--   nextData <- runMaybeT $ do
--     next <- MaybeT $ getNextEntry e
--     nextUrl <- lift $ getUrlPath next
--     lift $ return (D.entityVal next, nextUrl)

--   return (tags,prevData,nextData)

