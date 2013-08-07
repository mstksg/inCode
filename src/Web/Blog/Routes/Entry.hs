{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.Entry (routeEntrySlug, routeEntryId) where

import Control.Applicative ((<$>))
-- import Control.Monad.Reader
-- import Control.Monad.Trans
-- import qualified Text.Blaze.Html5 as H
import Control.Monad.IO.Class
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.Entry
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Database.Persist.Postgresql as D
import qualified Web.Scotty as S
-- import Control.Monad.Trans (lift)
import qualified Data.Map as M
-- import Control.Monad (join)
import Data.Maybe (isJust, fromJust)
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Web.Blog.Models.Util

routeEntrySlug :: RouteEither
routeEntrySlug = do
  eIdent <- S.param "entryIdent"

  eKey <- liftIO $ runDB $ do
    slug <- D.getBy $ UniqueSlug $ T.pack eIdent

    case slug of
      -- Found slug
      Just (D.Entity _ slug') -> 
        return $ Right $ slugEntryId slug'

      -- Slug not found
      Nothing ->
        return $ error404 "SlugNotFound"
        
  -- TODO: Wrap this all in an EitherT...that's what they were meant for,
  -- I think!
  case eKey of
    -- Yes there was a slug and entry found
    Right eKey' -> do
      e <- liftIO $ runDB $ D.get eKey'

      case e of
        -- Slug does indeed have a real entry
        Just e' ->
          routeEntry $ Right $ D.Entity eKey' e'

        -- Slug's entry does not exist.  How odd.
        Nothing -> 
          return $ error404 "SlugHasNoEntry"

    Left r ->
      return $ Left r

routeEntryId :: RouteEither
routeEntryId = do
  eIdent <- S.param "eId"

  let
    eKey = D.Key $ D.PersistInt64 (fromIntegral (read eIdent :: Int))

  e <- liftIO $ runDB $ do

    e' <- D.get eKey :: D.SqlPersistM (Maybe Entry)

    case e' of
      -- ID Found
      Just e'' -> do
        s' <- D.selectFirst [ SlugEntryId D.==. eKey ] []

        case s' of
          -- Found "a" slug.  It might not be "the" current slug,
          -- but for now we'll let redirection take care of it.
          Just (D.Entity _ s'') ->
            return $ Left $ L.fromStrict $ T.append "/entry/" (slugSlug s'')

          -- Did not find a slug...so it's an entry with no slug.
          -- Really shouldn't be happening but...just return the
          -- entry.
          -- TODO: maybe auto-generate new slug in this case?
          Nothing ->
            return $ Right $ D.Entity eKey e''

      -- ID not found
      Nothing ->
        return $ error404 "entryIdNotFound"

  routeEntry e


routeEntry :: Either L.Text (D.Entity Entry) -> RouteEither
routeEntry (Right (D.Entity eKey e')) = do
  (tags,prevData,nextData) <- liftIO $ runDB $ entryAux eKey e'

  let

    pdMap = execState $ do

      when (isJust prevData) $ do
        let prevUrl = snd $ fromJust prevData
        modify (M.insert ("prevUrl" :: T.Text) prevUrl)

      when (isJust nextData) $ do
        let nextUrl = snd $ fromJust nextData
        modify (M.insert ("nextUrl" :: T.Text) nextUrl)

      
    view = viewEntry e' tags (fst <$> prevData) (fst <$> nextData)
    pageData' = pageData { pageDataTitle = Just $ entryTitle e'
                         , pageDataMap   = pdMap M.empty
                         }
  
  return $ Right (view, pageData')
routeEntry (Left r) = return $ Left r

entryAux :: D.Key Entry -> Entry -> D.SqlPersistM ([Tag],Maybe (Entry, T.Text),Maybe (Entry, T.Text))
entryAux k e = do
  tags <- getTagsByEntityKey k

  prevData <- runMaybeT $ do
    prev <- MaybeT $ getPrevEntry e
    prevUrl <- lift $ getUrlPath prev
    lift $ return (D.entityVal prev, prevUrl)

  nextData <- runMaybeT $ do
    next <- MaybeT $ getNextEntry e
    nextUrl <- lift $ getUrlPath next
    lift $ return (D.entityVal next, nextUrl)

  return (tags,prevData,nextData)

