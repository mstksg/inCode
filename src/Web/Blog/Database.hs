{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE TypeSynonymInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}

module Web.Blog.Database (runDB, blogMigrate, blogClear) where

import Database.Persist.Postgresql
import Web.Blog.Models

connStr :: ConnectionString
connStr =
    "host=localhost dbname=test_blog user=blog-test password=blog-testblog-test port=4432"

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
