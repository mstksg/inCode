{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Models.Tag  where

import Control.Applicative                   ((<$>))
import Control.Monad.IO.Class                (liftIO)
import Control.Monad.Trans                   (lift)
import Control.Monad.Trans.Maybe
import Data.Char                             (toLower, toUpper)
import Data.Time
import Web.Blog.Models
import Web.Blog.Models.Entry
import Web.Blog.Models.Types
import Web.Blog.Render
import Web.Blog.Util
import qualified Data.Text                   as T
import qualified Database.Persist.Postgresql as D
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as I

data PreTag = PreTag T.Text TagType (Maybe T.Text)

insertTag :: PreTag -> D.SqlPersistM (Maybe (D.Key Tag))
insertTag ptag = D.insertUnique $ fillTag ptag

insertTag_ :: PreTag -> D.SqlPersistM ()
insertTag_ ptag = D.insert_ $ fillTag ptag

fillTag :: PreTag -> Tag
fillTag ptag = tag
  where
    PreTag l t d = ptag
    slug = genSlug (maxBound :: Int) l
    tag = case t of
      GeneralTag  -> Tag (T.map toLower l) t d slug
      CategoryTag -> Tag (T.map toUpper l) t d slug
      SeriesTag   -> Tag l t d slug

tagLabel' :: Tag -> T.Text
tagLabel' t = T.append (tagTypePrefix $ tagType_ t) $ tagLabel t

tagPath :: Tag -> T.Text
tagPath t = T.append prefix $ tagSlug t
  where
    prefix = T.append "/entries/"
      (case tagType_ t of
        GeneralTag  -> "tagged/"
        CategoryTag -> "category/@"
        SeriesTag   -> "series/+")

tagLi :: Tag -> H.Html
tagLi t = H.li $
  H.a H.! A.href (I.textValue $ renderUrl' $ tagPath t) $
    H.toHtml $ tagLabel' t

isGeneralTag :: Tag -> Bool
isGeneralTag t = case tagType_ t of
  GeneralTag -> True
  _          -> False

isCategoryTag :: Tag -> Bool
isCategoryTag t = case tagType_ t of
  CategoryTag -> True
  _           -> False

isSeriesTag :: Tag -> Bool
isSeriesTag t = case tagType_ t of
  SeriesTag -> True
  _         -> False

data TagInfo = TagInfo
               { tagInfoTag       :: Tag
               , tagInfoCount     :: Int
               , tagInfoRecent    :: Maybe (Entry, T.Text) }

getTagInfoList :: TagType -> D.SqlPersistM [TagInfo]
getTagInfoList tt = getTagInfoListRecent tt True

getTagInfoListRecent :: TagType -> Bool -> D.SqlPersistM [TagInfo]
getTagInfoListRecent tt recent = do
  allTags <- D.selectList [ TagType_ D.==. tt ] [ D.Asc TagLabel ]
  now <- liftIO getCurrentTime
  let
    tagInfo (D.Entity tKey t) = do
      c <- D.count [ EntryTagTagId D.==. tKey ]
      eKeys <- map (entryTagEntryId . D.entityVal) <$> D.selectList [ EntryTagTagId D.==. tKey ] []

      r <- if recent
        then
          runMaybeT $ do
            re <- MaybeT $ D.selectFirst (postedFilter now ++ [ EntryId D.<-. eKeys ]) [ D.Desc EntryPostedAt ]
            ru <- lift $ getUrlPath re
            return (D.entityVal re,ru)
        else 
          return Nothing

      return $ TagInfo t c r

  tagInfos <- mapM tagInfo allTags

  return $ filter ((> 0) . tagInfoCount) tagInfos
