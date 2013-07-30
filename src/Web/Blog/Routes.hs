{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes (routes) where

import Web.Scotty
import Web.Blog.Views
import Text.Blaze.Html.Renderer.Text
import Data.Monoid

routes :: ScottyM ()
routes = do
  get "/" $ 
    html "Hello World!"
  get "/entry/:entryId" $
    html $ renderHtml $ blogLayout Nothing [] mempty

