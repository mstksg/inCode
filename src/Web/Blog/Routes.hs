{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes (routes) where

import Web.Scotty
import Web.Blog.Views
import Web.Blog.Render
import Web.Blog.SiteData (SiteData)
import qualified Text.Blaze.Html5 as H
-- import Data.Monoid

routes :: SiteData -> ScottyM ()
routes siteData = do
  let pageData' = pageData siteData
  
  get "/" $ 
    html "Hello World!"

  get "/entry/:entryId" $
    
    
    
    siteRenderActionLayout viewEntry $
      pageData' { pageDataTitle = Just "Entry" }


siteRenderActionLayout :: SiteRender H.Html -> PageData -> ActionM ()
siteRenderActionLayout view = siteRenderAction (viewLayout view)

