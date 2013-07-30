{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Web.Blog.Routes
import Web.Blog.Views
import Text.Blaze.Html.Renderer.Text
import Data.Monoid
import Data.Text.Lazy
-- import Web.Blog.Models

main :: IO ()
main = scotty 4268 $ do
    routes

