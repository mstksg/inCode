{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Layout (viewLayout, viewLayoutEmpty) where

-- import Control.Applicative                ((<$>))
import Config.SiteData
import Control.Monad.Reader
import Data.Maybe                            (fromMaybe)
import Data.Monoid
import Text.Blaze.Html5                      ((!))
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Views.Social
import qualified Data.Foldable               as Fo
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as I

viewLayout :: SiteRender H.Html -> SiteRender H.Html
viewLayout body = do
  pageData' <- ask
  bodyHtml <- body
  navBarHtml <- navBar
  title <- createTitle
  socialFollowsHtml <- viewSocialFollow
  openGraphMetas <- viewOpenGraphMetas
  -- rssUrl <- renderUrl "/rss"

  let
    cssList = [ "/css/toast.css"
              , "/css/font.css"
              , "/css/main.css" ]
    jsList =  [ "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
              -- , "https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
              , T.append "//s7.addthis.com/js/300/addthis_widget.js#pubid=" $
                  developerAPIsAddThis $ siteDataDeveloperAPIs siteData
              , "/js/common.js"
              ]

  cssUrlList <- mapM renderUrl $ cssList ++ pageDataCss pageData'
  jsUrlList <- mapM renderUrl $ jsList ++ pageDataJs pageData'


  return $ H.docTypeHtml $ do

    H.head $ do

      H.title $
        H.toHtml title
      H.meta ! A.name "description" ! A.content (I.textValue $ siteDataDescription siteData)

      H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width,initial-scale=1.0"

      openGraphMetas

      H.link
        ! A.rel "author"
        ! A.href
          (I.textValue $ authorInfoRel $ siteDataAuthorInfo siteData)

      H.link
        ! A.rel "alternate"
        ! A.type_ "application/rss+xml"
        ! A.title
          (I.textValue $ T.append (siteDataTitle siteData) " (RSS Feed)")
        ! A.href "/rss"

      Fo.forM_ (pageDataUrl pageData') $ \url ->
        H.link
          ! A.rel "canonical"
          ! A.href (I.textValue url)

      H.link
        ! A.href "/favicon.ico"
        ! A.rel "shortcut icon"

      forM_ cssUrlList $ \u ->
        H.link ! A.href (I.textValue u) ! A.rel "stylesheet" ! A.type_ "text/css"


      H.script ! A.type_ "text/javascript" $ do
        H.toHtml $
          T.unlines
            [ "var page_data = {};"
            , T.concat
              [ "var disqus_shortname='"
              , developerAPIsDisqusShortname $ siteDataDeveloperAPIs siteData
              , "';" ]
            -- , "var addthis_config = {'data_track_addressbar':true};"
            ]

      forM_ jsUrlList $ \u ->
        H.script ! A.type_ "text/javascript" ! A.src (I.textValue u) $
          mempty

      sequence_ (pageDataHeaders pageData')


    H.body $ do

        googleAnalyticsJs
        H.div ! A.id "fb-root" $ mempty
        facebookSdkJs

        H.div ! A.id "header-container" $ do
          H.div! A.id "navbar-container" ! A.class_ "tile" $
            navBarHtml
          H.div ! A.id "header-content" $
            mempty

        H.div ! A.id "body-container" ! A.class_ "container" $
          H.div ! A.id "main-container" ! A.class_ "grid" $
            bodyHtml

            -- H.div ! A.id "sidebar-container" ! A.class_ "unit one-of-four" $
            --   sidebarHtml

            -- H.div ! A.id "main-container" ! A.class_ "unit three-of-four" ! I.customAttribute "role" "main" $
              -- bodyHtml

        H.div ! A.id "footer-container" $
          H.div ! A.id "footer-content" $
            H.div ! A.class_ "tile" $ do
              H.div ! A.class_ "footer-copyright" $
                H.preEscapedToHtml $
                  T.append "&copy; " $ siteDataCopyright siteData
              H.div ! A.class_ "footer-follow social-follows" $
                socialFollowsHtml

viewLayoutEmpty :: SiteRender H.Html
viewLayoutEmpty = viewLayout $ return mempty

createTitle :: SiteRender T.Text
createTitle = do
  pageData' <- ask
  let
    siteTitle = siteDataTitle siteData
    pageTitle = pageDataTitle pageData'
    combined   = case pageTitle of
      Just title -> T.concat [title," · ",siteTitle]
      Nothing    -> siteTitle
  return combined

navBar :: SiteRender H.Html
navBar = do
  homeUrl <- renderUrl "/"
  archiveUrl <- renderUrl "/entries"
  -- aboutUrl <- renderUrl "/about"

  return $
    H.nav ! A.id "navbar-content" $ do
      H.div ! A.class_ "nav-info" $ do
        H.h1 ! A.class_ "site-title" $
          H.a ! A.href (I.textValue homeUrl) ! A.class_ "nav-title" $
            H.toHtml $ siteDataTitle siteData
        H.span ! A.class_ "nav-author" $
          H.toHtml . authorInfoName $ siteDataAuthorInfo siteData

      H.ul ! A.class_ "nav-links" $ do
        H.li $
          H.a ! A.href (I.textValue homeUrl) $
            "home"
        H.li $
          H.a ! A.href (I.textValue archiveUrl) $
            "archives"
        -- H.li $
        --   H.a ! A.href (I.textValue aboutUrl) $
        --     "about"

        H.div ! A.class_ "clear" $
          mempty

googleAnalyticsJs :: H.Html
googleAnalyticsJs =
  H.script $
    H.toHtml $
      T.unlines
        [ "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){"
        , "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),"
        , "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)"
        , "})(window,document,'script','//www.google-analytics.com/analytics.js','ga');"
        , T.concat
          [ "ga('create', '"
          , fst $ developerAPIsAnalytics $ siteDataDeveloperAPIs siteData
          , "', '"
          , snd $ developerAPIsAnalytics $ siteDataDeveloperAPIs siteData
          , "');" ]
        , "ga('send', 'pageview');" ]

facebookSdkJs :: H.Html
facebookSdkJs =
  H.script $
    H.toHtml $
      T.unlines
        [ "(function(d, s, id) {"
        , "  var js, fjs = d.getElementsByTagName(s)[0];"
        , "  if (d.getElementById(id)) return;"
        , "  js = d.createElement(s); js.id = id;"
        , T.concat
          [ "  js.src = \"//connect.facebook.net/en_US/all.js#xfbml=1&appId="
          , developerAPIsFacebook $ siteDataDeveloperAPIs siteData
          , "\";" ]
        , "  fjs.parentNode.insertBefore(js, fjs);"
        , "}(document, 'script', 'facebook-jssdk'));"]

viewOpenGraphMetas :: SiteRender H.Html
viewOpenGraphMetas = do
  pageData' <- ask
  img <- case pageDataImage pageData' of
    Just pdImage -> renderUrl $ T.pack pdImage
    Nothing -> renderUrl "/img/site_logo.jpg"
  let
    title =
      fromMaybe (siteDataTitle siteData) $ pageDataTitle pageData'
    ogType =
      fromMaybe "website" $ pageDataType pageData'
    description =
      fromMaybe (siteDataDescription siteData) $ pageDataDesc pageData'
  return $ do
    H.meta
      ! I.customAttribute "property" "og:site_name"
      ! A.content (I.textValue . siteDataTitle $ siteData)
    H.meta
      ! I.customAttribute "property" "og:description"
      ! A.content (I.textValue description)
    H.meta
      ! I.customAttribute "property" "og:type"
      ! A.content (I.textValue ogType)
    H.meta
      ! I.customAttribute "property" "og:title"
      ! A.content (I.textValue title)
    H.meta
      ! I.customAttribute "property" "og:image"
      ! A.content (I.textValue img)
    H.meta
      ! I.customAttribute "property" "og:locale"
      ! A.content "en_US"
    Fo.forM_ (pageDataUrl pageData') $ \url ->
      H.meta
        ! I.customAttribute "property" "og:url"
        ! A.content (I.textValue url)

    H.meta
      ! A.name "twitter:card"
      ! A.content "summary"
    H.meta
      ! A.name "twitter:creator:id"
      ! A.content
        (I.textValue . authorInfoTwitterID $ siteDataAuthorInfo siteData)
