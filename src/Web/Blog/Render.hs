{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Render (
    siteRenderAction
  , emptyPageData
  , genPageData
  , renderUrl
  , renderUrl'
  , renderRawCopy
  , getCurrUrl
  , mainSection
  ) where

-- import System.Process
-- import qualified Text.Blaze.Html.Renderer.Pretty as B
-- import qualified Text.Blaze.Html5.Attributes     as A
-- import qualified Text.Pandoc.Builder             as P
import Config.SiteData
import Control.Applicative                          ((<$>))
import Control.Monad.Reader
import Data.Time
import Network.Wai
import System.Directory                             (doesFileExist)
import Web.Blog.Types
import qualified Data.Map                           as M
import qualified Data.Text                          as T
import qualified Data.Text.Lazy                     as L
import qualified Text.Blaze.Html.Renderer.Text      as B
import qualified Text.Blaze.Html5                   as H
import qualified Text.Blaze.Internal                as I
import qualified Text.Pandoc                        as P
import qualified Web.Scotty                         as S


emptyPageData :: PageData
emptyPageData =  PageData
            { pageDataTitle   = Nothing
            , pageDataDesc    = Nothing
            , pageDataImage   = Nothing
            , pageDataUrl     = Nothing
            , pageDataType    = Nothing
            , pageDataCss     = []
            , pageDataJs      = []
            , pageDataHeaders = []
            , pageDataMap     = M.empty
            , pageDataTimeZone = utc
            }

genPageData :: S.ActionM PageData
genPageData = do
    protocolHost <- renderProtocolHost
    pathText <- T.intercalate "/" . pathInfo <$> S.request
    tz <- liftIO getCurrentTimeZone

    return emptyPageData
           { pageDataUrl =
               Just $ T.intercalate "/"
                 [ protocolHost
                 , pathText
                 ]
           , pageDataTimeZone = tz
           }

siteRenderAction :: SiteRender H.Html -> PageData -> S.ActionM ()
siteRenderAction htmlRender pageData' = do
  ran <- runReaderT htmlRender pageData'
  S.html $ B.renderHtml ran
  -- S.html $ L.pack $ B.renderHtml ran

renderProtocolHost :: S.ActionM T.Text
renderProtocolHost = do
  host <- L.toStrict <$> S.reqHeader "Host"
  request <- S.request
  let
    protocol =
      if isSecure request
        then
          "https://"
        else
          "http://"
  return $ T.append protocol host

renderUrl :: T.Text -> SiteRender T.Text
renderUrl url = do
  let
    hasP = length (T.splitOn "//" url) > 1
  if hasP
    then return url
    else do
      protocolHost <- lift renderProtocolHost
      return $ T.concat [protocolHost, url]

renderUrl' :: T.Text -> T.Text
renderUrl' url =
  if hasP
    then url
    else T.concat
      [ "http://"
      , hostConfigHost $ siteDataHostConfig siteData
      , url
      ]
  where
    hasP = length (T.splitOn "://" url) > 1

-- renderScss :: FilePath -> Bool -> IO L.Text
-- renderScss fp minify = L.pack <$> readProcess "sass" ["--style",style,fp] []
--   where
--     style = if minify then "compressed" else "expanded"

renderRawCopy :: FilePath -> SiteRender H.Html
renderRawCopy fp = do
  exists <- liftIO $ doesFileExist fp
  if exists
    then do
      copyMarkdown <- liftIO $ readFile fp
      let
        copyPandoc = P.readMarkdown (P.def P.ReaderOptions) copyMarkdown
        fixedLinks = P.bottomUp fixLinks copyPandoc
          where
            fixLinks (P.Link label (url,title)) =
              P.Link label (T.unpack $ renderUrl' $ T.pack url, title)
            fixLinks other = other
        copyHtml = P.writeHtml (P.def P.WriterOptions) fixedLinks
      return copyHtml
    else
      return $
        H.p "Error: Copy not found!"

getCurrUrl :: S.ActionM T.Text
getCurrUrl = do
  path <- (T.intercalate "/" . pathInfo) <$> S.request
  host <- S.reqHeader "Host"
  return $ T.concat ["http://",L.toStrict host,"/",path]

mainSection :: I.Attribute
mainSection = I.customAttribute "role" "main"
