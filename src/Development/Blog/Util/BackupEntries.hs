
module Development.Blog.Util.BackupEntries (backupEntries) where

import Control.Applicative       ((<$>))
import Data.Aeson
import Data.ByteString.Lazy.UTF8
import Web.Blog.Database
import Web.Blog.Models.Util
import qualified Data.Text       as T

backupEntries :: IO T.Text
backupEntries = (T.pack . toString . encode) <$> runDB entryJSONs


