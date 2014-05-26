module Web.Blog.Routes.Entry (
    routeEntrySlug
  , routeEntryId
  , markdownEntrySlug
  , markdownEntryId
  , texEntrySlug
  , texEntryId
  ) where

import "base" Prelude
import Control.Applicative                   ((<$>))
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.List                             (find, sortBy)
import Data.Maybe                            (listToMaybe)
import Data.Ord                              (comparing)
import Data.Time
import Web.Blog.Models
import Web.Blog.Models.EntryI
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.Entry
import qualified Data.Foldable               as Fo
import qualified Data.Map.Strict             as M
import qualified Data.Text.Lazy              as L
import qualified Database.Persist.Postgresql as D
import qualified Text.Pandoc.Templates       as P
import qualified Web.Scotty                  as S

routeEntrySlug :: RouteDatabase
routeEntrySlug = do
  eIdent <- S.param "entryIdent"
  now <- liftIO getCurrentTime
  let url = L.append "/entry/" eIdent
  return $
    readerEntry now url =<< entryBySlug eIdent False

routeEntryId :: RouteDatabase
routeEntryId = do
  eId <- S.param "eId"
  now <- liftIO getCurrentTime
  let url = L.append "/entry/id/" (L.pack (show eId))
  return $
    readerEntry now url =<< entryById eId False

markdownEntrySlug :: L.Text -> S.ActionM (RouteReaderM L.Text)
markdownEntrySlug ident = return $ L.fromStrict . entryMarkdownFull . snd <$> entryBySlug ident True

markdownEntryId :: Int -> S.ActionM (RouteReaderM L.Text)
markdownEntryId i = return $ L.fromStrict . entryMarkdownFull . snd <$> entryById i True

texEntrySlug :: L.Text -> S.ActionM (RouteReaderM L.Text)
texEntrySlug ident = do
    temp <- liftIO (try (readFile "copy/templates/default.latex") :: IO (Either SomeException String))
    let temp' = either (const Nothing) Just temp
    return $
      L.fromStrict . entryTexFull temp' . snd <$> entryBySlug ident True

texEntryId :: Int -> S.ActionM (RouteReaderM L.Text)
texEntryId i = do
    temp <- either (const Nothing) Just <$> liftIO (P.getDefaultTemplate Nothing "latex")
    return $
      L.fromStrict . entryTexFull temp . snd <$> entryById i True


entryBySlug :: L.Text -> Bool -> RouteReaderM (KeyMapKey Entry, Entry)
entryBySlug sText md = do
  (db, _) <- ask

  let slugs = M.elems . siteDatabaseSlugs $ db
      s = find ((==) sText . L.fromStrict . slugSlug) slugs

  case s of
    Just s' -> do
      let eKey = slugEntryId s'
          e    = eKey `M.lookup` siteDatabaseEntries db

      case e of
        Just e' -> do
          currSlug <- getCurrentSlugI eKey

          case currSlug of
            Just currSlug' ->
              if slugSlug currSlug' == slugSlug s'
                then return (eKey, e')
                else if md
                       then siteLeft . flip L.append ".md" . L.append "/entry/" . L.fromStrict . slugSlug $ currSlug'
                       else siteLeft . L.append "/entry/" . L.fromStrict . slugSlug $ currSlug'

            Nothing -> return (eKey, e')

        Nothing ->
          error404 "SlugHasNoEntry"

    Nothing ->
      error404 "SlugNotFound"

entryById :: Int -> Bool -> RouteReaderM (KeyMapKey Entry, Entry)
entryById i md = do
  (db, _) <- ask

  let eKey = D.Key . D.PersistInt64 $ fromIntegral i
      e   = eKey `M.lookup` siteDatabaseEntries db

  case e of
    Just e' -> do
      currSlug <- getCurrentSlugI eKey

      case currSlug of
        Just currSlug' | md        ->
                           siteLeft . flip L.append ".md" . L.append "/entry/" . L.fromStrict . slugSlug $ currSlug'
                       | otherwise ->
                           siteLeft . L.append "/entry/" . L.fromStrict . slugSlug $ currSlug'
        Nothing         -> return (eKey, e')

    Nothing ->
      error404 "entryIdNotFound"

readerEntry ::  UTCTime -> L.Text -> (KeyMapKey Entry, Entry) -> RouteReader
readerEntry now url (k,e) = do
  rd@(_, blankPageData) <- ask
  tags          <- getTagsI k
  (prev, next)  <- prevNext now e

  let pdMap = execState $ do
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

      view = viewEntry e (L.toStrict url) tags (snd <$> prev) (snd <$> next)

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
                                                   ,"/js/fay-runtime.min.js"
                                                   ,"/js/page/entry.js"
                                                   ,"/js/page/entry_toc.js"]
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

