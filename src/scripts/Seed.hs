{-# LANGUAGE FlexibleContexts             #-} 
{-# LANGUAGE GADTs                        #-} 
{-# LANGUAGE TypeFamilies                 #-} 
{-# LANGUAGE TypeSynonymInstances         #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving   #-} 

import Database.Persist
import Web.Blog.Database
import Web.Blog.Models
import Data.Time.Clock
import Control.Monad.IO.Class
import Network.HTTP
import Text.Pandoc
import Control.Applicative
import qualified Data.Text as T
import Data.List.Split
import System.Random
import Control.Monad
import Data.Bits
import System.IO

main :: IO ()
main = runDB $ do
  blogMigrate
  blogClear

  tagId1 <- insert $ Tag "Random"
  tagId2 <- insert $ Tag "Cool Stuff"
  tagId3 <- insert $ Tag "Okay Stuff"

  replicateM_ 25 $ do
    (entry,tags) <- liftIO genEntry
    entryid <- insert entry

    when (tags .&. (1 :: Int) > 0) $ do
      insert $ EntryTag entryid tagId1
      return ()
    when (tags .&. (2 :: Int) > 0) $ do
      insert $ EntryTag entryid tagId2
      return ()
    when (tags .&. (4 :: Int) > 0) $ do
      insert $ EntryTag entryid tagId3
      return ()

    liftIO $ do
      putStrLn "Created new entry:"
      putStrLn (entryTitle entry)
      hFlush stdout


  liftIO $ putStrLn "Seed complete!"

  return ()

genEntry :: IO (Entry,Int)
genEntry = do
  now <- getCurrentTime

  gen <- newStdGen

  let
    createDiff :: Integer
    postDiff :: Integer
    (createDiff, gen') = randomR (-31536000,-604800) gen
    (postDiff, gen'') = randomR (100,604800) gen'
    createTime = addUTCTime (fromIntegral createDiff) now
    postTime = addUTCTime (fromIntegral postDiff) createTime
    (tags, _) = randomR (0,7) gen''

  -- let
  --   dayNow = utctDay now
  --   dayStart = addDays -100 dayNow
  --   dayEnd = addDays -2 dayNow
  --   (creationTime,gen') = genUTCTime gen dayStart dayEnd
    -- (postTime, _)       = genUTCTime gen 2


  title <- (last . splitOn ". " . unwords . lines)
      <$> genLoripsum "http://loripsum.net/api/1/short"


  desc <- (unwords . tail . splitOn ". " . unwords . lines)
      <$> genLoripsum "http://loripsum.net/api/1/short"

  body <- genLoripsum "http://loripsum.net/api/7/code/bq/ul/ol/dl/link/long/decorate/headers"

  return (
    ( Entry
        title
        desc
        (T.pack body)
        createTime
        postTime)
    , tags)


genLoripsum :: String -> IO String
genLoripsum apiUrl = do
  rsp <- simpleHTTP (getRequest apiUrl)
  body <- getResponseBody rsp

  return $ writeMarkdown (def WriterOptions) $
    readHtml (def ReaderOptions) body

