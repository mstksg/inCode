{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Render (

    HtmlRender
  , htmlRenderAction
  , PageDataMap
  , PageData
  , pageTitle
  , pageHeaders
  , pageDataMap
  , pageData

  ) where

import Control.Monad.Reader
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Blaze.Html.Renderer.Text as B
import qualified Text.Blaze.Html5 as H
import qualified Web.Scotty as S

import Web.Blog.SiteData

type HtmlRender = ReaderT PageData S.ActionM H.Html

type PageDataMap = M.Map T.Text String

data PageData = PageData
                { pageTitle   :: Maybe String
                , pageHeaders :: [H.Html]
                , pageDataMap :: PageDataMap
                }


pageData :: PageData
pageData = PageData Nothing [] M.empty

htmlRenderAction :: HtmlRender -> PageData -> S.ActionM ()
htmlRenderAction htmlRender pageData' = do
  ran <- runReaderT htmlRender pageData'
  S.html $ B.renderHtml ran
