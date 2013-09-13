{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE TypeSynonymInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# LANGUAGE OverloadedStrings   #-}

module Web.Blog.Database (runDB, blogMigrate, blogClear) where

import Database.Persist.Postgresql
import Web.Blog.Models
import Config.SiteData
import Web.Blog.Types
import Web.Heroku (dbConnParams)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as TE

connStr :: IO ConnectionString
connStr = do
  dbConfigs <- dbConfigsGen

  return $
    TE.encodeUtf8 $ T.intercalate " " $ map dbConfigString dbConfigs
  where
    dbConfigString (key,valField) = T.concat
      [ key
      , "="
      , valField
      ]
    dbConfigsGen =
      case siteDataDatabaseConfig siteData of
        Just dbc -> return
          [ ("host",databaseConfigHost dbc)
          , ("dbname",databaseConfigName dbc)
          , ("user",databaseConfigUser dbc)
          , ("password",databaseConfigPassword dbc)
          , ("port",T.pack $ show $ databaseConfigPort dbc)
          ]
        Nothing -> dbConnParams


runDB :: SqlPersistM a -> IO a
runDB commands = do
  connstr <- connStr
  withPostgresqlPool connstr 10 $ \pool ->
    runSqlPersistMPool commands pool


blogMigrate :: SqlPersistM ()
blogMigrate = runMigration migrateAll

blogClear :: SqlPersistM ()
blogClear = do
  deleteWhere ([] :: [Filter Slug])
  deleteWhere ([] :: [Filter EntryTag])
  deleteWhere ([] :: [Filter Tag])
  deleteWhere ([] :: [Filter Entry])
