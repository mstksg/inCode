{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes (routes) where

import Web.Scotty
import Web.Blog.Views
-- import Text.Blaze.Html.Renderer.Text
import Web.Blog.Render
-- import Data.Monoid

routes :: ScottyM ()
routes = do
  get "/" $ 

    html "Hello World!"

  get "/entry/:entryId" $
    
    htmlRenderAction (viewLayout viewEntry) $
      pageData { pageTitle = Just "Entry" }


