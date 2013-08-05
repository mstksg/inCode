{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.Home (routeHome) where

-- import Control.Monad.Reader
-- import Control.Monad.Trans
import Control.Applicative                   ((<$>))
import Control.Monad.IO.Class
import Control.Monad.Trans                   (lift)
import Data.Char                             (isDigit)
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.SiteData
import Web.Blog.Types
import Web.Blog.Views
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as L
import qualified Database.Persist.Postgresql as D
import qualified Text.Blaze.Html5            as H
import qualified Web.Scotty                  as S
import qualified Data.Map as M
import Control.Monad.State

routeHome :: Int -> RouteEither
routeHome page = do
  let
    m = siteDataHomeEntries siteData

  maxPage' <- liftIO $ runDB $ maxPage m

  if page < 1 || page > maxPage'
    then
      return $ Left "/"
    else do

      es <- liftIO $ runDB $ postedEntries [ D.Desc EntryPostedAt
                                          , D.LimitTo m 
                                          , D.OffsetBy $ (page - 1) * m ]

      paths <- liftIO $ runDB $ mapM getUrlPath es

      tags  <- liftIO $ runDB $ mapM getTags es


      



      let
        eData = zip (map renderUrl' paths) tags
        eList = zip es eData
        urlBase = renderUrl' "/home/"

      let
        pdMap = execState $ do
          when (page > 1) $ do
            let
              prevUrl = if page == 1
                then renderUrl' "/"
                else T.append urlBase $ T.pack $ show $ page - 1
            modify $
              M.insert "prevPage" prevUrl

          when (page < maxPage') $
            modify $
              M.insert "nextPage" (T.append urlBase $ T.pack $ show $ page + 1)

        view = viewHome eList
        pageData' = pageData { pageDataTitle = Just "Home"
                             , pageDataMap   = pdMap M.empty
                             }
          
      return $ Right (view, pageData')

maxPage :: Int -> D.SqlPersistM Int
maxPage perPage = do
  c <- postedEntryCount
  return $ (c + perPage - 1) `div` perPage
