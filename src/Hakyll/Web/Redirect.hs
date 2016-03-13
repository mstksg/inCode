{-# LANGUAGE QuasiQuotes #-}

module Hakyll.Web.Redirect where

import Hakyll
import NeatInterpolation
import qualified Data.Text as T

redirectCompiler :: (Identifier -> T.Text) -> Compiler (Item String)
redirectCompiler f = do
    p <- getUnderlying
    let p' = f p
    makeItem . T.unpack $ redirectHTML p'

redirectHTML :: T.Text -> T.Text
redirectHTML p =
    [text|
      <!DOCTYPE html>
      <html>
      <head>
      <title>Redirecting...</title>
      <link rel="canonical" href="${p}"/>
      <meta http-equiv="content-type" content="text/html; charset=utf-8" />
      <meta http-equiv="refresh" content="0; url=#{destination_path}" />
      </head>
      <body>
        <p><strong>Redirecting...</strong></p>
        <p><a href='${p}'>Click here if you are not redirected.</a></p>
        <script>
          document.location.href = "${p}";
        </script>
      </body>
      </html>
    |]
