module Web.Blog.Routes.Home (routeHome) where

import "base" Prelude
import Config.SiteData
import Control.Monad.IO.Class
import Control.Applicative ((<$>))
import Control.Monad.Reader
import Control.Monad.State
import Data.Time
import Web.Blog.Models.EntryI
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.Home
import qualified Data.Map                       as M
import qualified Data.Text                      as T

routeHome :: Int -> RouteDatabase
routeHome page = do
  now <- liftIO getCurrentTime
  return $ readerHome now page


readerHome :: UTCTime -> Int -> RouteReader
readerHome now page = do
  (_, blankPageData) <- ask
  posteds <- map fst . sortEntries <$> postedEntriesI now

  let
    perPage = appPrefsHomeEntries $ siteDataAppPrefs siteData
    mPage = (length posteds + perPage - 1) `div` perPage


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
        onPage = take perPage . drop (perPage * (page - 1)) $ posteds

      eList <- mapM wrapEntryDataI onPage

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
