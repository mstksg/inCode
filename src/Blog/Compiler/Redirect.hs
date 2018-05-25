{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog.Compiler.Redirect where

import           Blog.Types
import           Hakyll
import qualified Data.Text as T

redirectCompiler
    :: (?config :: Config)
    => (Identifier -> T.Text)
    -> Compiler (Item String)
redirectCompiler toDest = do
    ident <- toDest <$> getUnderlying
    makeItem $ T.unpack (redirectHTML ident)

redirectHTML
    :: (?config :: Config)
    => T.Text
    -> T.Text
redirectHTML p = T.unlines
    ["<!DOCTYPE html>"
    ,"<html>"
    ,"<head>"
    ,"<title>Redirecting...</title>"
    ,"<link rel=\"canonical\" href=\"" <> p <> "\"/>"
    ,"<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />"
    -- ,"<meta http-equiv=\"refresh\" content=\"0; url=" <> p <> "\" />"
    ,"<script>"
    ,"(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){"
    ,"(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),"
    ,"m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)"
    ,"})(window,document,'script','//www.google-analytics.com/analytics.js','ga');"
    ,"ga('create', { trackingId: '" <> aId <> "', cookieDomain: '" <> aDomain <> "', redirect: '" <> p <> "'});"
    ,"ga('send', { hitType: 'pageview', hitCallback: function() { document.location.href = '" <> p <> "'; } });"
    ,"</script>"
    ,"</head>"
    ,"<body>"
    ,"  <p><strong>Redirecting...</strong></p>"
    ,"  <p><a href='" <> p <> "'>Click here if you are not redirected.</a></p>"
    ,"  <script>"
    ,"    setTimeout(function() { document.location.href = '" <> p <> "'; }, 1000);"
    ,"  </script>"
    ,"</body>"
    ,"</html>"
    ]
  where
    (aId, aDomain) = devAnalytics (confDeveloperAPIs ?config)
