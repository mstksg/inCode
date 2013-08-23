{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE TypeSynonymInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# LANGUAGE OverloadedStrings   #-}

module Web.Blog.Database (runDB, blogMigrate, blogClear) where

import Database.Persist.Postgresql
import Web.Blog.Models
import Web.Blog.SiteData
import Web.Blog.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

connStr :: ConnectionString
connStr = TE.encodeUtf8 $ T.intercalate " " $ map dbConfigString
    [ ("host",databaseConfigHost)
    , ("dbname",databaseConfigName)
    , ("user",databaseConfigUser)
    , ("password",databaseConfigPassword)
    , ("port",T.pack . show . databaseConfigPort)
    ]
  where
    dbConfigString (key,valField) = T.concat
      [ key
      , "="
      , valField $ siteDataDatabaseConfig siteData
      ]


runDB :: SqlPersistM a -> IO a
runDB commands = withPostgresqlPool connStr 10 $ \pool ->
  runSqlPersistMPool commands pool


blogMigrate :: SqlPersistM ()
blogMigrate = runMigration migrateAll

blogClear :: SqlPersistM ()
blogClear = do
  deleteWhere ([] :: [Filter Slug])
  deleteWhere ([] :: [Filter EntryTag])
  deleteWhere ([] :: [Filter Tag])
  deleteWhere ([] :: [Filter Entry])
