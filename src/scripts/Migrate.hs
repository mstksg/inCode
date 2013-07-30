{-# LANGUAGE FlexibleContexts             #-} 
{-# LANGUAGE GADTs                        #-} 
{-# LANGUAGE TypeFamilies                 #-} 
{-# LANGUAGE TypeSynonymInstances         #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving   #-} 

import Database.Persist.Postgresql
import Web.Blog.Database

-- main :: IO ()
-- main = runSqlite ":memory:" $ do
--   runMigration migrateAll

main :: IO ()
main = blogMigrate
