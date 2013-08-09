{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.NotFound (viewNotFound) where

import Web.Blog.Types
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

viewNotFound :: SiteRender H.Html
viewNotFound = 
  return $ 
    H.section H.! A.class_ "tile" $ do
      H.header $
        H.h1 "Not Found"
      H.div $
        H.p "The page you were looking for was not found.  Sorry!"

