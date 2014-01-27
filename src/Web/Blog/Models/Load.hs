module Web.Blog.Models.Load (loadDatabase) where

import Control.Applicative
import Data.List                             (foldl')
import Web.Blog.Models
import Web.Blog.Types
import qualified Data.Map                    as M
import qualified Database.Persist.Postgresql as D


loadDatabase :: D.SqlPersistM SiteDatabase
loadDatabase =
  SiteDatabase <$> loadEntries <*> loadTags <*> loadEntryTags <*> loadSlugs

-- loadTable :: D.PersistEntity a => D.SqlPersistM (KeyMap a)
-- loadTable = loadEntities <$> D.selectList [] []

loadEntries :: D.SqlPersistM (KeyMap Entry)
loadEntries = loadEntities <$> D.selectList [] []

loadTags :: D.SqlPersistM (KeyMap Tag)
loadTags = loadEntities <$> D.selectList [] []

loadEntryTags :: D.SqlPersistM (KeyMap EntryTag)
loadEntryTags = loadEntities <$> D.selectList [] []

loadSlugs :: D.SqlPersistM (KeyMap Slug)
loadSlugs = loadEntities <$> D.selectList [] []

loadEntities :: [D.Entity a] -> KeyMap a
loadEntities = foldl' addEntity M.empty
  where
    addEntity m (D.Entity k e) = M.insert k e m

