{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Web.Redirect where

import Hakyll
import Data.Monoid
import qualified Data.Text as T

redirectCompiler :: (Identifier -> T.Text) -> Compiler (Item String)
redirectCompiler f = do
    p <- getUnderlying
    let p' = f p
    makeItem . T.unpack $ redirectHTML p'

redirectHTML :: T.Text -> T.Text
redirectHTML p =
    T.unlines ["<!DOCTYPE html>"
              ,"<html>"
              ,"<head>"
              ,"<title>Redirecting...</title>"
              ,"<link rel=\"canonical\" href=\"" <> p <> "\"/>"
              ,"<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />"
              ,"<meta http-equiv=\"refresh\" content=\"0; url=" <> p <> "\" />"
              ,"</head>"
              ,"<body>"
              ,"  <p><strong>Redirecting...</strong></p>"
              ,"  <p><a href='" <> p <> "'>Click here if you are not redirected.</a></p>"
              ,"  <script>"
              ,"    document.location.href = \"" <> p <> "\";"
              ,"  </script>"
              ,"</body>"
              ,"</html>"
              ]
