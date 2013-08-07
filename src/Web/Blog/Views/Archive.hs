{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Archive (
    viewArchive
  , ViewArchiveType(..)
  , viewArchiveNav
  , ViewArchiveIndex(..)
  ) where

-- import Data.Monoid
-- import Web.Blog.Render
-- import Web.Blog.SiteData
-- import qualified Data.Map                 as M
import Control.Applicative                   ((<$>))
import Control.Monad.Reader
import Data.Maybe                            (fromMaybe, isJust, fromJust)
import Text.Blaze.Html5                      ((!))
import Web.Blog.Models
import Web.Blog.Models.Util
import Web.Blog.Render
import Web.Blog.Types
import Web.Blog.Util
import qualified Data.Foldable as Fo         (forM_)
import qualified Data.Text                   as T
import qualified Data.Traversable  as Tr     (mapM)
import qualified Database.Persist.Postgresql as D
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as I

data ViewArchiveType = ViewArchiveAll
                     | ViewArchiveYear Int
                     | ViewArchiveMonth Int Int
                     | ViewArchiveTag Tag
                     | ViewArchiveCategory Tag
                     | ViewArchiveSeries Tag
                     deriving (Show)

viewArchive :: [[[(D.Entity Entry,(T.Text,[Tag]))]]] -> ViewArchiveType -> SiteRender H.Html
viewArchive eListYears viewType = do
  pageTitle <- pageDataTitle <$> ask
  nav <- viewArchiveNav (case viewType of
                          ViewArchiveAll -> Just ViewArchiveIndexDate
                          _ -> Nothing)

  upLink <- Tr.mapM renderUrl (upPath viewType)

  return $ do

    H.header $ do

      H.h1 $ H.toHtml $ fromMaybe "Entries" pageTitle
      when (isJust upLink) $
        H.a ! A.href (I.textValue $ fromJust upLink) $ "back"
      nav

    Fo.forM_ (desc viewType) $ \d ->
      H.p $
        H.toHtml d

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
          ViewArchiveAll        -> viewArchiveByYears eListYears
          ViewArchiveYear _     -> viewArchiveByMonths eListMonths
          ViewArchiveMonth _ _  -> viewArchiveFlat eList
          ViewArchiveTag _      -> viewArchiveFlat eList
          ViewArchiveCategory _ -> viewArchiveFlat eList
          ViewArchiveSeries _   -> viewArchiveFlat eList

upPath :: ViewArchiveType -> Maybe T.Text
upPath ViewArchiveAll          = Nothing
upPath (ViewArchiveYear _)     = Just "/entries"
upPath (ViewArchiveMonth y _)  = Just $ T.append "/entries/in/" $ T.pack $ show y
upPath (ViewArchiveTag _)      = Just "/tags"
upPath (ViewArchiveCategory _) = Just "/categories"
upPath (ViewArchiveSeries _)   = Just "/series"

desc :: ViewArchiveType -> Maybe T.Text
desc (ViewArchiveTag t)      = tagDescription t
desc (ViewArchiveCategory c) = tagDescription c
desc (ViewArchiveSeries s)   = tagDescription s
desc _                       = Nothing

data ViewArchiveIndex = ViewArchiveIndexDate
                      | ViewArchiveIndexTag
                      | ViewArchiveIndexCategory
                      | ViewArchiveIndexSeries
                      deriving (Show, Eq, Read)

viewArchiveNav :: Maybe ViewArchiveIndex -> SiteRender H.Html
viewArchiveNav isIndex = do
  byDateUrl <- renderUrl "/entries"
  byTagUrl  <- renderUrl "/tags"
  byCatUrl  <- renderUrl "/categories"
  bySerUrl  <- renderUrl "/series"
  return $
    H.nav $
      H.ul $
        forM_ [("History",byDateUrl,ViewArchiveIndexDate)
              ,("Tags",byTagUrl,ViewArchiveIndexTag)
              ,("Categories",byCatUrl,ViewArchiveIndexCategory)
              ,("Series",bySerUrl,ViewArchiveIndexSeries)] $ \(t,u,v) -> 
          H.li $
            if maybe True (/= v) isIndex
              then
                H.a ! A.href (I.textValue u) $
                  t
              else
                t


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

