module Web.Blog.Models.Persist where

import "base" Prelude
import Database.Persist.Quasi
import Database.Persist.TH

sqlSettings' :: PersistSettings
sqlSettings' = sqlSettings { psStrictFields = True }

