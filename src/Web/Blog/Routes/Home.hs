{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.Home (routeHome) where

import Config.SiteData
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Time
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Models.EntryI
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.Home
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Database.Persist.Postgresql as D

routeHome :: Int -> RouteDatabase
routeHome page = do
  now <- liftIO getCurrentTime
  return $ readerHome now page

  -- posteds <-
  -- maxPage' <- liftIO $ runDB $ maxPage m

--   if page < 1 || page > maxPage'
--     then
--       return $ siteLeft "/"
--     else do
--       let
--         pageTitle =
--           if page == 1
--             then
--               Nothing
--             else
--               Just $ T.concat ["Home (Page ", T.pack $ show page,")"]

--         urlBase = renderUrl' "/home/"

--       eList <- liftIO $ runDB $
--         postedEntries [ D.Desc EntryPostedAt
--                       , D.LimitTo m
--                       , D.OffsetBy $ (page - 1) * m ]
--           >>= mapM wrapEntryData

--       blankPageData <- genPageData

--       let
--         pdMap = execState $ do
--           when (page > 1) $ do
--             let
--               prevUrl = if page == 1
--                 then renderUrl' "/"
--                 else T.append urlBase $ T.pack $ show $ page - 1
--             modify $
--               M.insert "prevPage" prevUrl
--             modify $
--               M.insert "pageNum" $ T.pack $ show page


--           when (page < maxPage') $
--             modify $
--               M.insert "nextPage" (T.append urlBase $ T.pack $ show $ page + 1)

--         view = viewHome eList page
--         pageData = blankPageData { pageDataTitle = pageTitle
--                                  , pageDataCss   = ["/css/page/home.css"
--                                                    ,"/css/pygments.css"]
--                                  , pageDataJs    = ["/js/disqus_count.js"]
--                                  , pageDataMap   = pdMap M.empty
--                                  }

--       return $ siteRight (view, pageData)

readerHome :: UTCTime -> Int -> RouteReader
readerHome now page = do
  (db, blankPageData) <- ask

  let
    perPage = appPrefsHomeEntries $ siteDataAppPrefs siteData
    mPage = maxPage now perPage db


  if page < 1 || page > mPage
    then
      siteLeft "/"
    else do
      let
        pageTitle =
          if page == 1
            then
              Nothing
            else
              Just $ T.concat ["Home (Page ", T.pack $ show page,")"]

        urlBase = renderUrl' "/home/"

        eList = undefined
      -- eList <- liftIO $ runDB $
      --   postedEntries [ D.Desc EntryPostedAt
      --                 , D.LimitTo m
      --                 , D.OffsetBy $ (page - 1) * m ]
      --     >>= mapM wrapEntryData

      -- blankPageData <- genPageData

      let
        pdMap = execState $ do
          when (page > 1) $ do
            let
              prevUrl = if page == 1
                then renderUrl' "/"
                else T.append urlBase $ T.pack $ show $ page - 1
            modify $
              M.insert "prevPage" prevUrl
            modify $
              M.insert "pageNum" $ T.pack $ show page

          when (page < mPage) $
            modify $
              M.insert "nextPage" (T.append urlBase $ T.pack $ show $ page + 1)

        view = viewHome eList page
        pageData = blankPageData { pageDataTitle = pageTitle
                                 , pageDataCss   = ["/css/page/home.css"
                                                   ,"/css/pygments.css"]
                                 , pageDataJs    = ["/js/disqus_count.js"]
                                 , pageDataMap   = pdMap M.empty
                                 }

      siteRight (view, pageData)

maxPage :: UTCTime -> Int -> SiteDatabase -> Int
maxPage now perPage db = (c + perPage - 1) `div` perPage
  where
    c = postedEntryCountI now db




-- maxPage :: Int -> D.SqlPersistM Int
-- maxPage perPage = do
--   c <- postedEntryCount
--   return $ (c + perPage - 1) `div` perPage
