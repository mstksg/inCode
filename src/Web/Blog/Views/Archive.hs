{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Archive (
    viewArchive
  , ViewArchiveType(..)
  , viewArchiveNav
  , ViewArchiveIndex(..)
  ) where

import Control.Applicative                   ((<$>))
import Control.Monad.Reader
import Data.List                             (intersperse)
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
  navHtml <- viewArchiveNav (case viewType of
                          ViewArchiveAll -> Just ViewArchiveIndexDate
                          _ -> Nothing)

  upLink <- Tr.mapM renderUrl (upPath viewType)

  return $ do
    H.nav ! A.class_ "archive-nav tile unit one-of-four" $
      navHtml

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
        else do
          let
            eListMonths = concat eListYears
            eList = concat eListMonths
          case viewType of
            ViewArchiveAll        -> viewArchiveByYears eListYears
            ViewArchiveYear _     -> viewArchiveByMonths eListMonths True
            ViewArchiveMonth _ _  -> viewArchiveFlat eList True
            ViewArchiveTag _      -> viewArchiveFlat eList True
            ViewArchiveCategory _ -> viewArchiveFlat eList True
            ViewArchiveSeries _   -> viewArchiveFlat eList True

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
  return $ do
    H.h2
      "Archives"
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


viewArchiveFlat :: [(D.Entity Entry,(T.Text,[Tag]))] -> Bool -> H.Html
viewArchiveFlat eList tile =
  H.ul ! A.class_ (if tile then "tile entry-list" else "entry-list") $
    forM_ eList $ \eData -> do
      let
        (D.Entity _ e,(u,ts)) = eData
        commentUrl = T.append u "#disqus_thread"

      H.li ! A.class_ "entry-item" $ do
        H.div ! A.class_ "entry-info" $ do
          H.time
            ! A.datetime (I.textValue $ T.pack $ renderDatetimeTime $ entryPostedAt e)
            ! A.pubdate ""
            ! A.class_ "pubdate"
            $ H.toHtml $ renderFriendlyTime $ entryPostedAt e
          H.preEscapedToHtml
            (" &mdash; " :: T.Text)
          H.a ! A.href (I.textValue commentUrl) ! A.class_ "entry-comments" $
            "Comments"

        H.a ! A.href (I.textValue u) ! A.class_ "entry-link" $
          H.toHtml $ entryTitle e
        H.p ! A.class_ "inline-tag-list" $ do
          "in " :: H.Html
          inlineTagList ts

viewArchiveByMonths :: [[(D.Entity Entry,(T.Text,[Tag]))]] -> Bool -> H.Html
viewArchiveByMonths eListMonths tile =
  H.ul ! A.class_ (if tile then "tile entry-list" else "entry-list") $

    forM_ eListMonths $ \eList -> do
      let
        month = entryPostedAt $ D.entityVal $ fst $
            head eList

      H.li $ do
        H.h3 $
          H.a ! A.href (I.textValue $ renderUrl' $ T.pack $ renderMonthPath month) $
            H.toHtml $ renderMonthTime month

        viewArchiveFlat eList False

viewArchiveByYears :: [[[(D.Entity Entry,(T.Text,[Tag]))]]] -> H.Html
viewArchiveByYears eListYears =
  H.ul ! A.class_ "entry-list" $
    forM_ eListYears $ \eListMonths -> do
      let
        year = entryPostedAt $
          D.entityVal $ fst $ head $ head eListMonths

      H.li ! A.class_ "tile" $ do
        H.h2 $
          H.a ! A.href (I.textValue $ renderUrl' $ T.pack $ renderYearPath year) $
            H.toHtml $ renderYearTime year

        viewArchiveByMonths eListMonths False

inlineTagList :: [Tag] -> H.Html
inlineTagList ts = sequence_ hinter
  where
    hlist = map catLink ts
    hinter = intersperse ", " hlist
    catLink t =
      H.a H.! A.href (I.textValue $ renderUrl' $ tagPath t) $
        H.toHtml $ tagLabel'' t
