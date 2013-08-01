{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes (routes) where

import Control.Applicative ((<$>))
-- import Control.Monad (when)
-- import Data.Maybe 
-- import Data.Monoid
-- import qualified Database.Persist as D
import Control.Monad.IO.Class
import Data.Char (isDigit)
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Render
import Web.Blog.SiteData (SiteData)
import Web.Blog.Views
import Web.Scotty
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Database.Persist.Postgresql as DP
import qualified Text.Blaze.Html5 as H
import qualified Data.Map as M

routes :: SiteData -> ScottyM ()
routes siteData = do
  let pageData' = pageData siteData
  
  get "/" $ 
    html "Hello World!"

  get "/entry/:entryIdent" $ do
    eIdent <- param "entryIdent"

    e <- liftIO $ runDB $ do
      slug <- DP.getBy $ UniqueSlug $ T.pack eIdent

      case slug of
        -- Found slug
        Just (DP.Entity _ slug') -> do
          e' <- DP.getJust $ slugEntryId slug'
          if slugIsCurrent slug'

            -- It's the current slug
            then
              return $ Right e'

            -- It's an out of date slug
            else do
              s' <- DP.selectFirst [ SlugEntryId DP.==. slugEntryId slug'
                                   , SlugIsCurrent DP.==. True ] []
              case s' of
                Just (DP.Entity _ s'') ->
                  return $ Left $ slugSlug s''
                Nothing ->
                  return $ Left "/404"

        -- Slug not found
        Nothing ->
          if all isDigit eIdent

            -- It's an ID
            then do
              let
                eKey = DP.Key $ DP.PersistInt64 (fromIntegral (read eIdent :: Int))
              s' <- DP.selectFirst [ SlugEntryId DP.==. eKey
                                   , SlugIsCurrent DP.==. True ] []

              case s' of
                -- ID Found
                Just (DP.Entity _ s'') ->
                  return $ Left $ T.append "/entry/" (slugSlug s'')
                -- ID not found
                Nothing ->
                  return $ Left "/404"

            -- It's a dud
            else
              return $ Left "/404"
          
    case e of
      Right e' -> do

        tags <- liftIO $ runDB $ do
          slug <- DP.getBy $ UniqueSlug $ T.pack eIdent
          case slug of
            Just (DP.Entity _ slug') -> do
              tagAssociations <- DP.selectList [EntryTagEntryId DP.==. slugEntryId slug'] []
              let
                tagKeys = map (entryTagTagId . DP.entityVal) tagAssociations
              map tagLabel <$> mapM DP.getJust tagKeys
            Nothing -> 
              return []

        let
          view = viewEntry e' tags

        siteRenderActionLayout view $ 
          pageData' { pageDataTitle = Just $ entryTitle e' }

      Left r ->
        redirect $ L.fromStrict r




siteRenderActionLayout :: SiteRender H.Html -> PageData -> ActionM ()
siteRenderActionLayout view = siteRenderAction (viewLayout view)
