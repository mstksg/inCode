{-# LANGUAGE FlexibleContexts             #-} 
{-# LANGUAGE GADTs                        #-} 
{-# LANGUAGE TypeFamilies                 #-} 
{-# LANGUAGE TypeSynonymInstances         #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving   #-} 

module Web.Blog.Database (blogMigrate) where

import Database.Persist.Postgresql
import Web.Blog.Models

connStr :: ConnectionString
connStr =
    "host=localhost dbname=test_blog user=blog-test password=blog-testblog-test port=4432"

blogMigrate :: IO ()
blogMigrate = withPostgresqlPool connStr 10 $ \pool ->
  flip runSqlPersistMPool pool $
    runMigration migrateAll
