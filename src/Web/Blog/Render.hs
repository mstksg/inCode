{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Render (
    siteRenderAction
  , pageData
  , renderUrl
  , renderUrl'
  , renderRawCopy
  , getCurrUrl
  , mainSection
  ) where

-- import Data.Time
-- import System.Process
-- import qualified Text.Blaze.Html.Renderer.Pretty as B
-- import qualified Text.Blaze.Html5.Attributes     as A
import Control.Applicative                          ((<$>))
import Control.Monad.Reader
import Network.Wai
import System.Directory                             (doesFileExist)
import Config.SiteData
import Web.Blog.Types
import qualified Data.Map                           as M
import qualified Data.Text                          as T
import qualified Data.Text.Lazy                     as L
import qualified Text.Blaze.Html.Renderer.Text      as B
import qualified Text.Blaze.Html5                   as H
import qualified Text.Blaze.Internal                as I
import qualified Text.Pandoc                        as P
import qualified Web.Scotty                         as S


pageData :: PageData
pageData =  PageData
            { pageDataTitle   = Nothing
            , pageDataCss     = []
            , pageDataJs      = []
            , pageDataHeaders = []
            , pageDataMap     = M.empty
            , pageSiteData    = siteData
            }

siteRenderAction :: SiteRender H.Html -> PageData -> S.ActionM ()
siteRenderAction htmlRender pageData' = do
  ran <- runReaderT htmlRender pageData'
  S.html $ B.renderHtml ran
  -- S.html $ L.pack $ B.renderHtml ran

renderUrl :: T.Text -> SiteRender T.Text
renderUrl url = do
  let
    hasP = length (T.splitOn "//" url) > 1
  if hasP
    then return url
    else do
      host <- lift $ S.reqHeader "Host"
      return $ T.concat ["http://",L.toStrict host,url]

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
        copyHtml = P.writeHtml (P.def P.WriterOptions) copyPandoc
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
