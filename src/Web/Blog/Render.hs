module Web.Blog.Render (
    siteRenderAction
  , emptyPageData
  , genPageData
  , renderProtocolHost
  , extractSiteRender
  , renderUrl
  , renderUrl'
  , pandocReaderOptions
  , pandocWriterOptions
  , renderRawCopy
  , getCurrUrl
  , mainSection
  , siteLeft
  , siteRight
  , error404
  , askDb
  , runRouteReaderMRight
  ) where

import "base" Prelude
import Config.SiteData
import Control.Applicative                          ((<$>))
import Control.Monad.Reader
import Network.Wai
import System.Directory                             (doesFileExist)
import Web.Blog.Types
import qualified Data.Map.Strict                    as M
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
            }

genPageData :: S.ActionM PageData
genPageData = do
    protocolHost <- renderProtocolHost
    pathText <- T.intercalate "/" . pathInfo <$> S.request

    return emptyPageData
           { pageDataUrl =
               Just $ T.intercalate "/"
                 [ protocolHost
                 , pathText
                 ]
           }

siteRenderAction :: SiteRender H.Html -> PageData -> S.ActionM ()
siteRenderAction htmlRender pageData' = do
  ran <- runReaderT htmlRender pageData'
  S.html $ B.renderHtml ran
  -- S.html $ L.pack $ B.renderHtml ran

extractSiteRender :: SiteRender a -> S.ActionM a
extractSiteRender toRender = runReaderT toRender emptyPageData

siteLeft :: L.Text -> RouteReaderM a
siteLeft = lift . Left

siteRight :: RenderData -> RouteReader
siteRight = lift . Right

error404 :: L.Text -> RouteReaderM a
error404 = siteLeft . L.append "/not-found?err="

askDb :: RouteReaderM SiteDatabase
askDb = fst <$> ask

runRouteReaderMRight :: RouteReaderM a -> (SiteDatabase, PageData) -> a
runRouteReaderMRight rr rd =
  case runReaderT rr rd of
    Right val -> val
    Left _ -> undefined


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
      return $ T.append protocolHost url

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

pandocReaderOptions :: P.ReaderOptions
pandocReaderOptions = (P.def P.ReaderOptions)
                      { P.readerSmart = True
                      }

pandocWriterOptions :: P.WriterOptions
pandocWriterOptions = (P.def P.WriterOptions)
                      { P.writerHtml5 = True
                      , P.writerHTMLMathMethod = P.WebTeX "http://chart.apis.google.com/chart?cht=tx&chf=bg,s,FFFFFF00&chl="
                      , P.writerHighlight = True
                      , P.writerVariables = [("geometry:margin","0.5in")
                                            ,("links-as-notes","")]
                      -- , P.writerHTMLMathMethod = P.WebTeX "http://chart.apis.google.com/chart?cht=tx&chl="
                      -- , P.writerHTMLMathMethod = P.WebTeX "http://www.mathtran.org/cgi-bin/mathtran?D=1&tex="
                      -- , P.writerHTMLMathMethod = P.WebTeX "http://webtex-2.sys.kth.se/api/webtex/v1/WebTex?tex="
                      }

renderRawCopy :: FilePath -> SiteRender H.Html
renderRawCopy fp = do
  exists <- liftIO $ doesFileExist fp
  if exists
    then do
      copyMarkdown <- liftIO $ readFile fp
      let
        copyPandoc = P.readMarkdown pandocReaderOptions copyMarkdown
        fixedLinks = P.bottomUp fixLinks copyPandoc
          where
            fixLinks (P.Link label (url,title)) =
              P.Link label (T.unpack $ renderUrl' $ T.pack url, title)
            fixLinks other = other
        copyHtml = P.writeHtml pandocWriterOptions fixedLinks
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
