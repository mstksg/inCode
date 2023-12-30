{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Blog.Render where

import Blog.Types
import Blog.View
import Blog.View.Social
import Data.Foldable
import Data.Maybe
import Data.String
import qualified Data.Text as T
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

renderLayout ::
  (?config :: Config) =>
  PageData ->
  H.Html ->
  H.Html
renderLayout pd@PD {..} body =
  H.docTypeHtml $ do
    H.head $ do
      H.title $ H.toHtml title
      H.meta ! A.name "description" ! A.content (H.textValue confDesc)

      H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width,initial-scale=1.0"

      H.meta ! A.name "flattr:id" ! A.content (H.textValue (devFlattr confDeveloperAPIs))

      viewOpenGraphMetas pd

      H.link
        ! A.rel "author"
        ! A.href (H.textValue (authorRel confAuthorInfo))

      H.link
        ! A.rel "alternate"
        ! A.type_ "application/rss+xml"
        ! A.title (H.textValue (confTitle <> " (RSS Feed)"))
        ! A.href (H.textValue confFeed)

      forM_ pageDataCanonical $ \url ->
        H.link
          ! A.rel "canonical"
          ! A.href (fromString (renderUrl' url))

      H.link
        ! A.href (H.textValue (renderUrl "/favicon.ico"))
        ! A.rel "shortcut icon"

      forM_ allCss $ \u ->
        H.link ! A.href (H.textValue u) ! A.rel "stylesheet" ! A.type_ "text/css"

      H.script ! A.type_ "text/javascript" $
        H.toHtml $
          T.unlines
            [ "var page_data = {};",
              "var disqus_shortname='" <> devDisqus confDeveloperAPIs <> "';"
              -- , "var addthis_config = {'data_track_addressbar':true};"
            ]

      googleAnalyticsJs
      mathJax

      forM_ allJs $ \u ->
        H.script ! A.type_ "text/javascript" ! A.src (H.textValue u) $
          mempty

      sequence_ pageDataHeaders

    H.body $ do
      H.div ! A.id "fb-root" $
        facebookSdkJs

      H.div ! A.id "header-container" $ do
        H.div ! A.id "navbar-container" ! A.class_ "tile" $
          navBar
        H.div ! A.id "header-content" $
          mempty -- ??
      H.div ! A.id "body-container" ! A.class_ "container" $
        H.div ! A.id "main-container" ! A.class_ "grid" $
          body

      H.div ! A.id "footer-container" $
        H.div ! A.id "footer-content" $
          H.div ! A.class_ "tile" $ do
            H.div ! A.class_ "footer-copyright" $ do
              H.preEscapedToHtml $
                "&copy; " <> confCopyright
              " "
              H.span ! A.class_ "license-link" $ do
                "("
                H.a ! A.href (H.textValue confLicenseLink) ! A.class_ "license" $
                  H.toHtml confLicense
                ")"
            H.div ! A.class_ "footer-follow social-follows" $
              viewSocialFollow
  where
    Config {..} = ?config
    cssList =
      [ "/css/toast.css",
        "/css/font.css",
        "/css/main.css"
      ]
    jsList =
      [ "//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js",
        "//s7.addthis.com/js/300/addthis_widget.js#pubid=" <> devAddThis confDeveloperAPIs
      ]
    allCss = map renderUrl $ cssList ++ pageDataCss
    allJs = map renderUrl $ jsList ++ pageDataJs
    mathJax = do
      H.script ! A.src "https://polyfill.io/v3/polyfill.min.js?features=es6" $
        mempty
      H.script ! A.id "MathJax-script" ! A.async "" ! A.src "https://cdn.jsdelivr.net/npm/mathjax@3.0.1/es5/tex-mml-chtml.js" $
        mempty
    title = case pageDataTitle of
      Just t -> t <> " · " <> confTitle
      Nothing -> confTitle

navBar :: (?config :: Config) => H.Html
navBar = H.nav ! A.id "navbar-content" $ do
  H.div ! A.class_ "nav-info" $ do
    H.h1 ! A.class_ "site-title" $
      H.a ! A.href (H.textValue (renderUrl "/")) ! A.class_ "nav-title" $
        H.toHtml $
          confTitle ?config
    H.span ! A.class_ "nav-author" $
      H.toHtml $
        authorName (confAuthorInfo ?config)

  H.ul ! A.class_ "nav-links" $ do
    H.li $
      H.a ! A.href (H.textValue (renderUrl "/")) $
        "home"
    H.li $
      H.a ! A.href (H.textValue (renderUrl "/entries.html")) $
        "archives"
    H.li $
      H.a ! A.href (H.textValue (renderUrl "https://cv.jle.im")) $
        "cv"

    H.div ! A.class_ "clear" $
      mempty

googleAnalyticsJs :: (?config :: Config) => H.Html
googleAnalyticsJs =
  H.script $
    H.toHtml $
      T.unlines
        [ "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){",
          "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),",
          "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)",
          "})(window,document,'script','//www.google-analytics.com/analytics.js','ga');",
          "ga('create', '" <> aId <> "', '" <> aDomain <> "');",
          "ga('send', 'pageview');"
        ]
  where
    (aId, aDomain) = devAnalytics (confDeveloperAPIs ?config)

facebookSdkJs :: (?config :: Config) => H.Html
facebookSdkJs =
  H.script $
    H.toHtml $
      T.unlines
        [ "(function(d, s, id) {",
          " var js, fjs = d.getElementsByTagName(s)[0];",
          " if (d.getElementById(id)) return;",
          " js = d.createElement(s); js.id = id;",
          " js.src = \"//connect.facebook.net/en_US/all.js#xfbml=1&appId=" <> appId <> "\";",
          " fjs.parentNode.insertBefore(js, fjs);",
          "}(document, 'script', 'facebook-jssdk'));"
        ]
  where
    appId = devFacebook (confDeveloperAPIs ?config)

viewOpenGraphMetas :: (?config :: Config) => PageData -> H.Html
viewOpenGraphMetas PD {..} = do
  H.meta
    ! H.customAttribute "property" "og:site_name"
    ! A.content (H.textValue confTitle)
  H.meta
    ! H.customAttribute "property" "og:description"
    ! A.content (H.textValue descr)
  H.meta
    ! H.customAttribute "property" "og:type"
    ! A.content (H.textValue ogType)
  H.meta
    ! H.customAttribute "property" "og:title"
    ! A.content (H.textValue title)
  H.meta
    ! H.customAttribute "property" "og:image"
    ! A.content (H.textValue img)
  H.meta
    ! H.customAttribute "property" "og:locale"
    ! A.content "en_US"
  forM_ pageDataCanonical $ \url ->
    H.meta
      ! H.customAttribute "property" "og:url"
      ! A.content (H.textValue (renderUrl (T.pack url)))

  H.meta
    ! A.name "twitter:card"
    ! A.content "summary"
  H.meta
    ! A.name "twitter:creator:id"
    ! A.content (H.textValue (authorTwitter confAuthorInfo))
  where
    Config {..} = ?config
    title = fromMaybe confTitle pageDataTitle
    ogType = fromMaybe "website" pageDataType
    descr = fromMaybe confDesc pageDataDesc
    img = case pageDataImage of
      Just pdImage -> renderUrl (T.pack pdImage)
      Nothing -> renderUrl "/img/site_logo.jpg"
