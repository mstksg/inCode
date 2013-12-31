module Web.Blog.Models.Load (loadDatabase) where

import Web.Blog.Types
import Web.Blog.Models
import qualified Data.IntMap                 as IM
import qualified Database.Persist.Postgresql as D
import Data.List (foldl')
import Control.Applicative


loadDatabase :: D.SqlPersistM SiteDatabase
loadDatabase =
  SiteDatabase <$> loadEntries <*> loadTags <*> loadEntryTags <*> loadSlugs

-- loadTable :: D.SqlPersistM (IM.IntMap a)
-- loadTable = loadEntities <$> D.selectList [] []

loadEntries :: D.SqlPersistM (IM.IntMap Entry)
loadEntries = loadEntities <$> D.selectList [] []

loadTags :: D.SqlPersistM (IM.IntMap Tag)
loadTags = loadEntities <$> D.selectList [] []

loadEntryTags :: D.SqlPersistM (IM.IntMap EntryTag)
loadEntryTags = loadEntities <$> D.selectList [] []

loadSlugs :: D.SqlPersistM (IM.IntMap Slug)
loadSlugs = loadEntities <$> D.selectList [] []

loadEntities :: [D.Entity a] -> IM.IntMap a
loadEntities = foldl' addEntity IM.empty
  where
    addEntity m (D.Entity k e) =
      case k of
        D.Key (D.PersistInt64 i)    -> IM.insert (fromIntegral i) e m
        _                           -> m


