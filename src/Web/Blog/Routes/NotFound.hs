{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Routes.NotFound (routeNotFound) where

import Control.Applicative     ((<$>))
import Data.List               (find)
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.NotFound
import qualified Web.Scotty    as S

routeNotFound :: RouteEither
routeNotFound = do
  err <- find ((== "err") . fst) <$> S.params
  return $ case err of
    Just _  -> Left "/not-found"
    Nothing -> Right (viewNotFound, pageData)

