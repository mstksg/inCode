{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Layout (viewLayout, viewLayoutEmpty) where

import Control.Applicative                   ((<$>))
import Control.Monad.Reader
import Data.Monoid
import Text.Blaze.Html5                      ((!))
import Web.Blog.Render
import Web.Blog.SiteData
import Web.Blog.Types
import Web.Blog.Views.Social
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
  -- rssUrl <- renderUrl "/rss"

  let
    cssList = [ "/css/toast.css"
              , "/css/font.css"
              , "/css/main.css" ]
    jsList =  [ "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
              , T.append "//s7.addthis.com/js/300/addthis_widget.js#pubid=" $
                  developerAPIsAddThis $ siteDataDeveloperAPIs siteData
              , "/js/common.js"
              ]

  cssUrlList <- mapM renderUrl $ cssList ++ pageDataCss pageData'
  jsUrlList <- mapM renderUrl $ jsList ++ pageDataJs pageData'


  return $ H.docTypeHtml $ do

    H.head $ do

      H.title title
      H.meta ! A.name "description" ! A.content (I.textValue $ siteDataDescription siteData)

      H.meta ! A.httpEquiv "Content-Type" ! A.content "text/html;charset=utf-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width,initial-scale=1.0"

      forM_ cssUrlList $ \u ->
        H.link ! A.href (I.textValue u) ! A.rel "stylesheet" ! A.type_ "text/css"

      H.link
        ! A.rel "author"
        ! A.href
          (I.textValue $ authorInfoRel $ siteDataAuthorInfo $ pageSiteData pageData')

      H.link
        ! A.rel "alternate"
        ! A.type_ "application/rss+xml"
        ! A.title
          (I.textValue $ T.append (siteDataTitle siteData) " (RSS Feed)")
        ! A.href "/rss"
        -- ! A.href
        --   (I.textValue rssUrl)
        -- ! A.href
        --   (I.textValue $ T.append "http://feeds.feedburner.com/" $ developerAPIsFeedburner $ siteDataDeveloperAPIs siteData)

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

createTitle :: SiteRender H.Html
createTitle = do
  pageData' <- ask
  let
    siteTitle = siteDataTitle $ pageSiteData pageData'
    pageTitle = pageDataTitle pageData'
    combined   = case pageTitle of
      Just title -> T.concat [siteTitle, " — ", title]
      Nothing    -> siteTitle
  return $ H.toHtml combined

navBar :: SiteRender H.Html
navBar = do
  homeUrl <- renderUrl "/"
  archiveUrl <- renderUrl "/entries"
  -- aboutUrl <- renderUrl "/about"
  author <- (authorInfoName . siteDataAuthorInfo . pageSiteData) <$> ask
  siteTitle <- (siteDataTitle . pageSiteData) <$> ask

  return $
    H.nav ! A.id "navbar-content" $ do
      H.div ! A.class_ "nav-info" $ do
        H.h1 ! A.class_ "site-title" $
          H.a ! A.href (I.textValue homeUrl) ! A.class_ "nav-title" $
            H.toHtml siteTitle
        H.span ! A.class_ "nav-author" $
          H.toHtml author

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


-- renderFonts :: [(T.Text,[T.Text])] -> H.Html
-- renderFonts fs = H.link ! A.href l ! A.rel "stylesheet" ! A.type_ "text/css"
 --  where
 --    l = I.textValue $ T.concat $ map makeFont fs
 --    makeFont (n,ts) = T.append n $ T.intersperse ',' ts
