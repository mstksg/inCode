{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.Entry (routeEntry) where

-- import Control.Monad.Reader
-- import Control.Monad.Trans
-- import qualified Text.Blaze.Html5 as H
import Control.Applicative ((<$>))
import Control.Monad.IO.Class
import Data.Char (isDigit)
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Database.Persist.Postgresql as D
import qualified Web.Scotty as S

routeEntry :: RouteEither
routeEntry = do
  eIdent <- S.param "entryIdent"

  e <- liftIO $ runDB $ do
    slug <- D.getBy $ UniqueSlug $ T.pack eIdent

    case slug of
      -- Found slug
      Just (D.Entity _ slug') -> do
        e' <- D.getJust $ slugEntryId slug'
        if slugIsCurrent slug'

          -- It's the current slug
          then
            return $ Right e'

          -- It's an out of date slug
          else do
            s' <- D.selectFirst [ SlugEntryId D.==. slugEntryId slug'
                                , SlugIsCurrent D.==. True ] []
            case s' of
              -- Found the current slug
              Just (D.Entity _ s'') ->
                return $ Left $ slugSlug s''

              -- No current slug...something is wrong.  Oh well, display
              -- entry anyway
              Nothing ->
                return $ Right e'

      -- Slug not found
      Nothing ->
        if all isDigit eIdent

          -- It's an ID
          then do
            let
              eKey = D.Key $ D.PersistInt64 (fromIntegral (read eIdent :: Int))
            s' <- D.selectFirst [ SlugEntryId D.==. eKey
                                , SlugIsCurrent D.==. True ] []

            case s' of
              -- ID Found
              Just (D.Entity _ s'') ->
                return $ Left $ T.append "/entry/" (slugSlug s'')
              -- ID not found
              Nothing ->
                return $ Left "/not-found"

          -- It's not an ID, it's just nothing
          else
            return $ Left "/not-found"
        
  case e of
    Right e' -> do

      tags <- liftIO $ runDB $ do
        slug <- D.getBy $ UniqueSlug $ T.pack eIdent
        case slug of
          Just (D.Entity _ slug') -> do
            tagAssociations <- D.selectList [EntryTagEntryId D.==. slugEntryId slug'] []
            let
              tagKeys = map (entryTagTagId . D.entityVal) tagAssociations
            map tagLabel <$> mapM D.getJust tagKeys
          Nothing -> 
            return []

      let
        view = viewEntry e' tags
        pageData' = pageData {pageDataTitle = Just $ entryTitle e'}
      
      return $ Right (view, pageData')

    Left r ->
      return $ Left $ L.fromStrict r
