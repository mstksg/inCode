{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.Entry (routeEntry, routeEntryId) where

import Control.Applicative ((<$>))
-- import Control.Monad.Reader
-- import Control.Monad.Trans
-- import qualified Text.Blaze.Html5 as H
import Control.Monad.IO.Class
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Database.Persist.Postgresql as D
import qualified Web.Scotty as S
import Control.Monad.Trans (lift)
import Control.Monad (join)
import Data.Maybe
import Control.Monad.Trans.Maybe

routeEntry :: RouteEither
routeEntry = do
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
        Just e' -> do
          (tags,prevUrl,nextUrl) <- liftIO $ runDB $ entryAux eKey' e'

          let
            view = viewEntry e' (map tagLabel tags) prevUrl nextUrl
            pageData' = pageData {pageDataTitle = Just $ entryTitle e'}

          return $ Right (view, pageData')

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

    e' <- D.get eKey

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
            return $ Right e''

      -- ID not found
      Nothing ->
        return $ error404 "entryIdNotFound"

  case e of
    Right e' -> do

      (tags,prevUrl,nextUrl) <- liftIO $ runDB $ entryAux eKey e'

      let
        view = viewEntry e' (map tagLabel tags) prevUrl nextUrl
        pageData' = pageData {pageDataTitle = Just $ entryTitle e'}
      
      return $ Right (view, pageData')

    Left r ->
      return $ Left r

entryAux :: D.Key Entry -> Entry -> D.SqlPersistM ([Tag],Maybe T.Text,Maybe T.Text)
entryAux k e = do
  tags <- getTagsByEntityKey k
  prevUrl <- runMaybeT $ do
    prev <- MaybeT $ getPrevEntry e
    lift $ getUrlPath prev
  nextUrl <- runMaybeT $ do
    next <- MaybeT $ getNextEntry e
    lift $ getUrlPath next
  return (tags,prevUrl,nextUrl)

