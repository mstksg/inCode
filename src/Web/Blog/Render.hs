{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Render (
    siteRenderAction
  , pageData
  , renderUrl
  , renderUrl'
  , renderScss
  , mainSection
  ) where

-- import Data.Time
-- import qualified Text.Blaze.Html.Renderer.Pretty as B
import Control.Applicative                          ((<$>))
import Control.Monad.Reader
import System.Process
import Web.Blog.SiteData
import Web.Blog.Types
import qualified Data.Map                           as M
import qualified Data.Text                          as T
import qualified Data.Text.Lazy                     as L
import qualified Text.Blaze.Html.Renderer.Text      as B
import qualified Text.Blaze.Html5                   as H
-- import qualified Text.Blaze.Html5.Attributes        as A
import qualified Text.Blaze.Internal                as I
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
    else T.concat ["http://",siteDataSiteHost siteData,url]
  where
    hasP = length (T.splitOn "://" url) > 1
      
renderScss :: FilePath -> Bool -> IO L.Text
renderScss fp minify = L.pack <$> readProcess "sass" ["--style",style,fp] []
-- renderScss fp minify = do
    -- t <- getCurrentTime
    -- putStrLn $ unwords ["Rendering scss file",fp]
    -- out <- L.pack <$> readProcess "sass" ["--style",style,fp] []
    -- t2 <- getCurrentTime
    -- let
    --   elapsed = diffUTCTime t2 t
    -- putStrLn $ unwords ["Rendered!  Total time:",show elapsed]
    -- return out
  where
    style = if minify then "compressed" else "expanded"

mainSection :: I.Attribute
mainSection = I.customAttribute "role" "main"
