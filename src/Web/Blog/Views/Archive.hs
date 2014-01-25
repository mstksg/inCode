module Web.Blog.Views.Archive (
    viewArchive
  , ViewArchiveType(..)
  , viewArchiveSidebar
  , ViewArchiveIndex(..)
  ) where

import "base" Prelude
import Control.Applicative                   ((<$>))
import Control.Monad.Reader
import Data.List                             (intersperse)
import Data.Maybe                            (fromMaybe, isJust, fromJust)
import Text.Blaze.Html5                      ((!))
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Models.Util
import Web.Blog.Render
import Data.Time
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
  let
    eListMonths = concat eListYears
    eList = concat eListMonths

  pageTitle <- pageDataTitle <$> ask
  sidebarHtml <- viewArchiveSidebar $ case viewType of
                          ViewArchiveAll -> Just ViewArchiveIndexDate
                          _ -> Nothing

  upLink <- Tr.mapM renderUrl (upPath viewType)

  archiveListHtml <-
    case viewType of
      ViewArchiveAll        -> viewArchiveByYears eListYears viewType
      ViewArchiveYear _     -> viewArchiveByMonths eListMonths viewType
      ViewArchiveMonth _ _  -> viewArchiveFlat eList viewType
      ViewArchiveTag _      -> viewArchiveFlat eList viewType
      ViewArchiveCategory _ -> viewArchiveFlat eList viewType
      ViewArchiveSeries _   -> viewArchiveFlat eList viewType


  return $ do
    H.div ! A.class_ "archive-sidebar unit one-of-four" $
      sidebarHtml

    H.section ! A.class_ "archive-section unit three-of-four" ! mainSection $ do

      H.header ! A.class_ "tile" $ do

        when (isJust upLink) $
          H.nav $
            H.a ! A.href (I.textValue $ fromJust upLink) ! A.class_ "back-link" $
              "back"

        H.h1 $ H.toHtml $ fromMaybe "Entries" pageTitle

        Fo.forM_ (desc viewType) $ \d ->
          H.p $
            H.toHtml d

      if null eListYears
        then
          H.div ! A.class_ "tile no-entries" $
            H.p $ H.toHtml $ case pageTitle of
              Just pt -> T.concat ["No entries found for ",pt,"."]
              Nothing -> "No entries found."
        else
          archiveListHtml

upPath :: ViewArchiveType -> Maybe T.Text
upPath ViewArchiveAll          = Nothing
upPath (ViewArchiveYear _)     = Just "/entries"
upPath (ViewArchiveMonth y _)  = Just $ T.append "/entries/in/" $ T.pack $ show y
upPath (ViewArchiveTag _)      = Just "/tags"
upPath (ViewArchiveCategory _) = Just "/categories"
upPath (ViewArchiveSeries _)   = Just "/series"

desc :: ViewArchiveType -> Maybe H.Html
desc (ViewArchiveTag t)      = tagDescHtml t
desc (ViewArchiveCategory c) = tagDescHtml c
desc (ViewArchiveSeries s)   = tagDescHtml s
desc _                       = Nothing

data ViewArchiveIndex = ViewArchiveIndexDate
                      | ViewArchiveIndexTag
                      | ViewArchiveIndexCategory
                      | ViewArchiveIndexSeries
                      deriving (Show, Eq, Read)

-- TODO: One day this can be a "top entries"
viewArchiveSidebar :: Maybe ViewArchiveIndex -> SiteRender H.Html
viewArchiveSidebar isIndex = do
  byDateUrl <- renderUrl "/entries"
  byTagUrl  <- renderUrl "/tags"
  byCatUrl  <- renderUrl "/categories"
  bySerUrl  <- renderUrl "/series"

  entries <- liftIO $ runDB $
    postedEntriesFilter [] [ D.Desc EntryPostedAt, D.LimitTo 5 ]
  eList <- liftIO $ runDB $ mapM wrapEntryData entries

  return $ do
    H.nav ! A.class_ "archive-nav tile" $ do
      H.h2
        "Entries"
      H.ul $
          forM_ [("History",byDateUrl,ViewArchiveIndexDate)
                ,("Tags",byTagUrl,ViewArchiveIndexTag)
                ,("Categories",byCatUrl,ViewArchiveIndexCategory)
                ,("Series",bySerUrl,ViewArchiveIndexSeries)] $ \(t,u,v) ->
            if maybe True (/= v) isIndex
              then
                H.li $
                  H.a ! A.href (I.textValue u) $
                    t
              else
                H.li ! A.class_ "curr-index" $
                  t
    H.div ! A.class_ "archive-recents tile" $ do
      H.h2 "Recent"
      H.ul $
        forM_ eList $ \(D.Entity _ e,(u,_)) ->
          H.li $
            H.a ! A.href (I.textValue $ renderUrl' u) $
              H.toHtml $ entryTitle e



viewArchiveFlat :: [(D.Entity Entry,(T.Text,[Tag]))] -> ViewArchiveType -> SiteRender H.Html
viewArchiveFlat eList viewType = do
  tz <- liftIO getCurrentTimeZone

  return $
    H.ul ! A.class_ ulClass $
      forM_ eList $ \(D.Entity _ e,(u,ts)) -> do
        let
          commentUrl = T.append u "#disqus_thread"

        H.li ! A.class_ "entry-item" $ do
          H.div ! A.class_ "entry-info" $ do
            Fo.forM_ (entryPostedAt e) $ \t -> do
              H.time
                ! A.datetime (I.textValue $ T.pack $ renderDatetimeTime t)
                ! A.pubdate ""
                ! A.class_ "pubdate"
                $ H.toHtml $ renderFriendlyTime tz t
              H.preEscapedToHtml
                (" &mdash; " :: T.Text)
            H.a ! A.href (I.textValue commentUrl) ! A.class_ "entry-comments" $
              "Comments"

          H.a ! A.href (I.textValue u) ! A.class_ "entry-link" $
            H.toHtml $ entryTitle e
          let
            tagList = filter tagFilter ts
          unless (null tagList) $
            H.p ! A.class_ "inline-tag-list" $ do
              "in " :: H.Html
              inlineTagList tagList
  where
    ulClass =
      case viewType of
        ViewArchiveAll -> "entry-list"
        ViewArchiveYear _ -> "entry-list"
        _ -> "tile entry-list"
    tagFilter =
      case viewType of
        ViewArchiveTag t      -> (/=) t
        ViewArchiveCategory t -> (/=) t
        ViewArchiveSeries t   -> (/=) t
        _                     -> const True


viewArchiveByMonths :: [[(D.Entity Entry,(T.Text,[Tag]))]] -> ViewArchiveType -> SiteRender H.Html
viewArchiveByMonths eListMonths viewType = do
  eListHtml <- forM eListMonths $ \eList -> do
    let
      month = fromJust $ entryPostedAt $ D.entityVal $ fst $
          head eList

    archiveFlatHtml <- viewArchiveFlat eList viewType

    return $
      H.li $ do
        H.h3 $
          H.a ! A.href (I.textValue $ renderUrl' $ T.pack $ renderMonthPath month) $
            H.toHtml $ renderMonthTime month

        archiveFlatHtml

  return $
    H.ul ! A.class_ ulClass $
      sequence_ eListHtml
  where
    ulClass =
      case viewType of
        ViewArchiveAll -> "entry-list"
        _ -> "tile entry-list"

viewArchiveByYears :: [[[(D.Entity Entry,(T.Text,[Tag]))]]] -> ViewArchiveType -> SiteRender H.Html
viewArchiveByYears eListYears viewType = do
  eListHtml <- forM eListYears $ \eListMonths -> do
    let
      year = fromJust $ entryPostedAt $
        D.entityVal $ fst $ head $ head eListMonths

    archiveMonthsHtml <- viewArchiveByMonths eListMonths viewType

    return $
      H.li ! A.class_ "tile" $ do
        H.h2 $
          H.a ! A.href (I.textValue $ renderUrl' $ T.pack $ renderYearPath year) $
            H.toHtml $ renderYearTime year

        archiveMonthsHtml

  return $
    H.ul ! A.class_ "entry-list" $
      sequence_ eListHtml

inlineTagList :: [Tag] -> H.Html
inlineTagList ts = sequence_ hinter
  where
    hlist = map catLink ts
    hinter = intersperse ", " hlist
    catLink t =
      H.a
      ! A.href (I.textValue $ renderUrl' $ tagPath t)
      ! A.class_ (tagLiClass t) $
        H.toHtml $ tagLabel'' t
