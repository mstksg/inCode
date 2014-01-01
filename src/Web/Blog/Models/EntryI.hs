module Web.Blog.Models.EntryI where

import Data.Time
import Web.Blog.Models.Models
import Web.Blog.Types
-- import Control.Applicative
import qualified Data.Foldable as F
import qualified Data.Map      as M

postedEntriesI :: UTCTime -> SiteDatabase -> KeyMap Entry
postedEntriesI now db =
    M.filter (F.any (< now) . entryPostedAt) $ siteDatabaseEntries db

postedEntryCountI :: UTCTime -> SiteDatabase -> Int
postedEntryCountI = (M.size .) . postedEntriesI
