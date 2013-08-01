{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Render (

    SiteRender
  , siteRenderAction
  , PageDataMap
  , PageData
  , pageDataTitle
  , pageDataHeaders
  , pageDataMap
  , pageSiteData
  , pageData
  , renderUrl

  ) where

import Control.Monad.Reader
import Web.Blog.SiteData
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
-- import qualified Text.Blaze.Html.Renderer.Text as B
import qualified Text.Blaze.Html.Renderer.Pretty as B
import qualified Text.Blaze.Html5 as H
import qualified Web.Scotty as S

type SiteRender a = ReaderT PageData S.ActionM a

type PageDataMap = M.Map T.Text String

data PageData = PageData
                { pageDataTitle   :: Maybe T.Text
                , pageDataHeaders :: [H.Html]
                , pageDataMap :: PageDataMap
                , pageSiteData :: SiteData
                }


pageData :: SiteData -> PageData
pageData = PageData Nothing [] M.empty 

siteRenderAction :: SiteRender H.Html -> PageData -> S.ActionM ()
siteRenderAction htmlRender pageData' = do
  ran <- runReaderT htmlRender pageData'
  S.html $ L.pack $ B.renderHtml ran

renderUrl :: T.Text -> SiteRender T.Text
renderUrl url = do
  let
    hasP = (length $ T.splitOn "://" url) > 1
  if hasP
    then return url
    else do
      host <- lift $ S.reqHeader "Host"
      return $ T.concat ["http://",L.toStrict host,url]
      
