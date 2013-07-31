{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Render (

    HtmlRender
  , htmlRenderAction
  , PageDataMap
  , PageData
  , pageDataTitle
  , pageDataHeaders
  , pageDataMap
  , pageSiteData
  , pageData

  ) where

import Control.Monad.Reader
import Web.Blog.SiteData
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Blaze.Html.Renderer.Text as B
import qualified Text.Blaze.Html5 as H
import qualified Web.Scotty as S

type HtmlRender = ReaderT PageData S.ActionM H.Html

type PageDataMap = M.Map T.Text String

data PageData = PageData
                { pageDataTitle   :: Maybe T.Text
                , pageDataHeaders :: [H.Html]
                , pageDataMap :: PageDataMap
                , pageSiteData :: SiteData
                }


pageData :: SiteData -> PageData
pageData = PageData Nothing [] M.empty 

htmlRenderAction :: HtmlRender -> PageData -> S.ActionM ()
htmlRenderAction htmlRender pageData' = do
  ran <- runReaderT htmlRender pageData'
  S.html $ B.renderHtml ran
