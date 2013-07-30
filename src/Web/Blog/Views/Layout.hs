{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Layout (blogLayout) where

import Data.Monoid
import Data.Maybe

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

blogLayout :: Maybe String -> [H.Html] -> H.Html -> H.Html
blogLayout title headers body = H.docTypeHtml $ do

  H.head $ do
    H.title (H.toHtml (fromMaybe "Blog" title))
    H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
    sequence_ headers

  H.body $ do

    H.div ! A.id "header_container" $
      H.div ! A.id "header_content" $
        mempty

    H.div ! A.id "main_container" $
      H.div ! A.id "main_content" $ do
        body
        H.div "hey"

    H.div ! A.id "footer_container" $
      H.div ! A.id "footer_content" $
        "© Justin Le 2013"

