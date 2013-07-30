{-# LANGUAGE FlexibleContexts             #-} 
{-# LANGUAGE GADTs                        #-} 
{-# LANGUAGE TypeFamilies                 #-} 
{-# LANGUAGE TypeSynonymInstances         #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving   #-} 

import Database.Persist.Sqlite
import Web.Blog.Models

main :: IO ()
main = runSqlite ":memory:" $ do
  runMigration migrateAll
