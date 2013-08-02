{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.Home (routeHome) where

-- import Control.Monad.Reader
-- import Control.Monad.Trans
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
import qualified Text.Blaze.Html5 as H
import qualified Web.Scotty as S
import Control.Monad.Trans (lift)

routeHome :: RouteEither
routeHome = do
  es <- liftIO $ runDB $ postedEntries [ D.Desc EntryPostedAt
                                       , D.LimitTo 5            ]
  paths <- liftIO $ runDB $ mapM getUrlPath es

  tags  <- liftIO $ runDB $ mapM getTags es

  let
    eData = zip (map renderUrl' paths) tags
    eList = zip es eData

  let
    view = viewHome eList
    pageData' = pageData {pageDataTitle = Just "Home"}
      
  return $ Right (view, pageData')
