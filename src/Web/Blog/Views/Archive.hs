{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Archive (
    viewArchive
  , ViewArchiveType(..)
  ) where

-- import Data.Monoid
-- import Web.Blog.Render
-- import Web.Blog.SiteData
-- import qualified Data.Map                 as M
import Control.Applicative                   ((<$>))
import Control.Monad.Reader
import Data.Maybe                            (fromMaybe)
import Text.Blaze.Html5                      ((!))
import Web.Blog.Models
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Util
import qualified Data.Text                   as T
import qualified Database.Persist.Postgresql as D
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as I

data ViewArchiveType = ViewArchiveAll
                     | ViewArchiveYear
                     | ViewArchiveMonth
                     | ViewArchiveTag
                     | ViewArchiveCategory
                     | ViewArchiveSeries
                     deriving (Show, Eq, Read)

viewArchive :: [[[(D.Entity Entry,(T.Text,[Tag]))]]] -> ViewArchiveType -> SiteRender H.Html
viewArchive eListYears viewType = do
  pageTitle <- pageDataTitle <$> ask
  byDateUrl <- renderUrl "/entries"
  byTagUrl <- renderUrl "/entries/tagged"
  byCatUrl <- renderUrl "/entries/category"
  bySerUrl <- renderUrl "/entries/series"

  return $ do

    H.header $ do
      H.h1 $ H.toHtml $ fromMaybe "Entries" pageTitle

      H.nav $
        H.ul $
          forM_ [("Date",byDateUrl,ViewArchiveAll)
                ,("Tag",byTagUrl,ViewArchiveTag)
                ,("Category",byCatUrl,ViewArchiveCategory)
                ,("Series",bySerUrl,ViewArchiveSeries)] $ \(t,u,_) -> 
            H.li $
              H.a ! A.href (I.textValue u) $
                t
              -- if vat /= viewType
              --   then
              --     H.a ! A.href (I.textValue u) $
              --       t
              --   else
              --     t

    if null eListYears
      then
        H.p $ H.toHtml $ case pageTitle of
          Just pt -> T.concat ["No entries found for ",pt,"."]
          Nothing -> "No entries found."
      else do
        let
          eListMonths = concat eListYears
          eList = concat eListMonths
        case viewType of
          ViewArchiveAll -> viewArchiveByYears eListYears
          ViewArchiveYear -> viewArchiveByMonths eListMonths
          ViewArchiveMonth -> viewArchiveFlat eList
          ViewArchiveTag -> viewArchiveFlat eList
          ViewArchiveCategory -> viewArchiveFlat eList
          ViewArchiveSeries -> viewArchiveFlat eList



viewArchiveFlat :: [(D.Entity Entry,(T.Text,[Tag]))] -> H.Html
viewArchiveFlat eList = 
  H.ul $
    forM_ eList $ \eData -> do
      let
        (D.Entity _ e,(u,ts)) = eData

      H.li $ do
        H.h4 $
          H.a ! A.href (I.textValue u) $
            H.toHtml $ entryTitle e
        H.time
          ! A.datetime (I.textValue $ T.pack $ renderDatetimeTime $ entryPostedAt e)
          ! A.pubdate "" 
          ! A.class_ "pubdate"
          $ H.toHtml $ renderFriendlyTime $ entryPostedAt e
        H.ul $
          forM_ ts $ \t ->
            tagLi t

viewArchiveByMonths :: [[(D.Entity Entry,(T.Text,[Tag]))]] -> H.Html
viewArchiveByMonths eListMonths = 
  H.ul $

    forM_ eListMonths $ \eList -> do
      let
        month = entryPostedAt $ D.entityVal $ fst $
            head eList

      H.li $ do
        H.h3 $
          H.a ! A.href (I.textValue $ renderUrl' $ T.pack $ renderMonthPath month) $
            H.toHtml $ renderMonthTime month

        viewArchiveFlat eList

viewArchiveByYears :: [[[(D.Entity Entry,(T.Text,[Tag]))]]] -> H.Html
viewArchiveByYears eListYears =
  H.ul $ 
    forM_ eListYears $ \eListMonths -> do
      let
        year = entryPostedAt $
          D.entityVal $ fst $ head $ head eListMonths

      H.li $ do
        H.h2 $
          H.a ! A.href (I.textValue $ renderUrl' $ T.pack $ renderYearPath year) $
            H.toHtml $ renderYearTime year

        viewArchiveByMonths eListMonths

