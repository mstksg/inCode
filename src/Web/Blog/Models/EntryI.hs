module Web.Blog.Models.EntryI where

import Control.Applicative
import Data.List                             (find)
import Data.Maybe                            (mapMaybe, fromJust)
import Data.Time
import Web.Blog.Models.Models
import Web.Blog.Models.Types
import Web.Blog.Types
import qualified Data.Foldable               as F
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Database.Persist.Postgresql as D

postedEntriesI :: UTCTime -> SiteDatabase -> KeyMap Entry
postedEntriesI now db =
    M.filter (F.any (< now) . entryPostedAt) $ siteDatabaseEntries db

postedEntryCountI :: UTCTime -> SiteDatabase -> Int
postedEntryCountI = (M.size .) . postedEntriesI


getEntryDataI :: KeyMapKey Entry -> SiteDatabase -> (T.Text,[Tag])
getEntryDataI k db = (urlPath, tags)
  where
    urlPath = getUrlPathI k db
    tags = getTagsI k db

wrapEntryDataI :: KeyMapKey Entry -> SiteDatabase -> (KeyMapPair Entry, (T.Text, [Tag]))
wrapEntryDataI k db = ((k, e), getEntryDataI k db)
  where
    e = fromJust $ k `M.lookup` siteDatabaseEntries db

getTagsI :: KeyMapKey Entry -> SiteDatabase -> [Tag]
getTagsI k db = sortedTags
  where
    entryTags = M.elems . siteDatabaseEntryTags $ db
    filtered = filter ((== k) . entryTagEntryId) entryTags
    tagKeys = map entryTagTagId filtered
    tags = mapMaybe (`M.lookup` siteDatabaseTags db) tagKeys
    tagsOf tt = filter ((== tt) . tagType_) tags
    sortedTags = concatMap tagsOf [GeneralTag ..]

getUrlPathI :: KeyMapKey Entry -> SiteDatabase -> T.Text
getUrlPathI k db =
  case getCurrentSlugI k db of
    Just slug -> T.append "/entry/" (slugSlug slug)
    Nothing -> T.append "/entry/id/" (T.pack . show . intFromKey $ k)

intFromKey :: KeyMapKey a -> Int
intFromKey (D.Key (D.PersistInt64 i)) = fromIntegral i
intFromKey _ = undefined

getCurrentSlugI :: KeyMapKey Entry -> SiteDatabase -> Maybe Slug
getCurrentSlugI k db = find ((&&) <$> slugIsCurrent <*> matchingSlug) slugs
  where
    slugs = M.elems $ siteDatabaseSlugs db
    matchingSlug = (== k) . slugEntryId

