module Web.Blog.Types (
    SiteData(..)
  , SiteRender
  , PageDataMap
  , PageData(..)
  , RouteEither
  , error404
  ) where

import Control.Monad.Reader
import qualified Data.Map         as M
import qualified Data.Text        as T
import qualified Data.Text.Lazy   as L
import qualified Text.Blaze.Html5 as H
import qualified Web.Scotty       as S

data SiteData = SiteData
                { siteDataTitle       :: T.Text
                , siteDataAuthor      :: T.Text
                , siteDataSiteHost    :: T.Text
                , siteDataAuthorRel   :: T.Text
                , siteDataSlugLength  :: Int
                , siteDataHomeEntries :: Int
                , siteDataLedeMax     :: Int
                }


type SiteRender a = ReaderT PageData S.ActionM a

type PageDataMap = M.Map T.Text T.Text

data PageData = PageData
                { pageDataTitle   :: Maybe T.Text
                , pageDataHeaders :: [H.Html]
                , pageDataMap     :: PageDataMap
                , pageSiteData    :: SiteData
                }

type RouteEither = S.ActionM (Either L.Text (SiteRender H.Html, PageData))

error404 :: L.Text -> Either L.Text a
error404 reason = Left $ L.append "/not-found?err=" reason
