{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blog.View.Archive where

import           Blog.Compiler.Entry
import           Blog.Types
import           Blog.Util
import           Blog.Util.Tag
import           Blog.View
import           Control.Monad
import           Data.List
import           Data.Monoid
import           Data.String
import           System.FilePath
import           Text.Blaze.Html5            ((!))
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

data ArchiveInfo = AI
    { aiData    :: !(ArchiveData TaggedEntry)
    , aiRecents :: ![Entry]
    }
  deriving (Show)

viewArchive
    :: (?config :: Config)
    => ArchiveInfo
    -> H.Html
viewArchive AI{..} = do
    H.div ! A.class_ "archive-sidebar unit one-of-four" $
      viewArchiveSidebar aiRecents $ case aiData of
                                       ADAll _ -> Just AIndHistory
                                       _       -> Nothing

    H.section ! A.class_ "archive-section unit three-of-four" ! mainSection $ do

      H.header ! A.class_ "tile" $ do

        forM_ upPath $ \p ->
          H.nav $
            H.a ! A.href (H.textValue (renderUrl p)) ! A.class_ "back-link" $
              "back"

        H.h1 $ H.toHtml archTitle

        sequence_ descr

      if null aiData
        then
          H.div ! A.class_ "tile no-entries" $
            H.p $ H.toHtml ("No entries found for " <> archTitle <> ".")
        else
          archiveList
  where
    archiveList = case aiData of
                    ADAll        es -> viewArchiveByYears es
                    ADYear   y   es -> viewArchiveByMonths True y es
                    ADMonth  _ _ es -> viewArchiveFlat True es
                    ADTagged t   es -> viewArchiveFlat True . flip map es $ \case
                                         TE e ts -> TE e . flip filter ts $ \t' ->
                                           not ( tagLabel t == tagLabel t'
                                              && tagType  t == tagType  t'
                                               )
    archTitle = archiveTitle aiData
    upPath :: Maybe T.Text
    upPath = case aiData of
               ADAll _                -> Nothing
               ADYear _ _             -> Just "/entries"
               ADMonth y _ _          -> Just . T.pack $ "/entries/in/" ++ show y
               ADTagged t _           ->
                 case tagType t of
                   GeneralTag  -> Just "/tags"
                   CategoryTag -> Just "/categories"
                   SeriesTag   -> Just "/series"
    descr :: Maybe H.Html
    descr = case aiData of
              ADAll    _     -> Nothing
              ADYear   _ _   -> Nothing
              ADMonth  _ _ _ -> Nothing
              ADTagged t _   -> htmlDescription t

archiveTitle :: ArchiveData a -> String
archiveTitle = \case
                 ADAll    _     -> "History"
                 ADYear   y _   -> show y
                 ADMonth  y m _ -> showMonth m ++ " " ++ show y
                 ADTagged t _   -> tagPrettyLabel t

data ArchiveIndex = AIndHistory
                  | AIndTagged TagType
  deriving (Show, Eq, Read)

-- TODO: One day this can be a "top entries"
viewArchiveSidebar
    :: (?config :: Config)
    => [Entry]
    -> Maybe ArchiveIndex
    -> H.Html
viewArchiveSidebar recents isIndex = do
    H.nav ! A.class_ "archive-nav tile" $ do
      H.h2
        "Entries"
      H.ul $
        forM_ indexList $ \(t,u,i) ->
          if maybe True (/= i) isIndex
            then H.li $
                   H.a ! A.href (H.textValue (renderUrl u)) $ t
            else H.li ! A.class_ "curr-index" $ t
    H.div ! A.class_ "archive-recents tile" $ do
      H.h2 "Recent"
      H.ul $
        forM_ recents $ \Entry{..} ->
          H.li $
            H.a ! A.href (fromString (renderUrl' entryCanonical)) $
              H.toHtml entryTitle
  where
    indexList =  [("History"   , "/entries"   , AIndHistory           )
                 ,("Tags"      , "/tags"      , AIndTagged GeneralTag )
                 ,("Categories", "/categories", AIndTagged CategoryTag)
                 ,("Series"    , "/series"    , AIndTagged SeriesTag  )
                 ]

viewArchiveFlat
    :: (?config :: Config)
    => Bool
    -> [TaggedEntry]
    -> H.Html
viewArchiveFlat tile entries =
    H.ul ! A.class_ ulClass $
      forM_ (sortTaggedEntries entries) $ \TE{..} -> do
        let entryUrl   = T.pack $ renderUrl' (entryCanonical teEntry)
            commentUrl = entryUrl <> "#disqus_thread"

        H.li ! A.class_ "entry-item" $ do
          H.div ! A.class_ "entry-info" $ do
            forM_ (entryPostTime teEntry) $ \t -> do
              H.time
                ! A.datetime (H.textValue $ T.pack (renderDatetimeTime t))
                ! A.pubdate ""
                ! A.class_ "pubdate"
                $ H.toHtml (renderFriendlyTime t)
              H.preEscapedToHtml
                (" &mdash; " :: T.Text)
            H.a ! A.href (H.textValue commentUrl) ! A.class_ "entry-comments" $
              "Comments"

          H.a ! A.href (H.textValue entryUrl) ! A.class_ "entry-link" $
            H.toHtml (entryTitle teEntry)
          unless (null teTags) $
            H.p ! A.class_ "inline-tag-list" $ do
              "in " :: H.Html
              sequence_ . intersperse ", "
                $ map (tagLink tagPrettyLabelLower) teTags
  where
    ulClass | tile      = "tile entry-list"
            | otherwise = "entry-list"


viewArchiveByMonths
    :: (?config :: Config)
    => Bool
    -> Year
    -> M.Map Month [TaggedEntry]
    -> H.Html
viewArchiveByMonths tile y entries =
    H.ul ! A.class_ ulClass $
      forM_ (reverse (M.toList entries)) $ \(m, tes) -> do
        let monthPath = renderUrl' $ "/entries/in" </> show y </> show (mInt m)
        H.li $ do
          H.h3 $
            H.a ! A.href (fromString monthPath)
              $ H.toHtml (showMonth m)

        viewArchiveFlat False tes
  where
    ulClass | tile      = "tile entry-list"
            | otherwise = "entry-list"

viewArchiveByYears
    :: (?config :: Config)
    => M.Map Year (M.Map Month [TaggedEntry])
    -> H.Html
viewArchiveByYears entries =
    H.ul ! A.class_ "entry-list" $
      forM_ (reverse (M.toList entries)) $ \(y, tes) -> do
        let yearPath = renderUrl' $ "/entries/in" </> show y
        H.li ! A.class_ "tile" $ do
          H.h2 $
            H.a ! A.href (fromString yearPath)
                $ H.toHtml (show y)

          viewArchiveByMonths False y tes
