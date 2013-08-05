{-# LANGUAGE FlexibleContexts             #-} 
{-# LANGUAGE GADTs                        #-} 
{-# LANGUAGE TypeFamilies                 #-} 
{-# LANGUAGE TypeSynonymInstances         #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving   #-} 

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Data.Bits
import Data.Char
import Data.List.Split
import Data.Maybe
import Data.Time.Clock
import Database.Persist
import Network.HTTP
import System.IO
import Text.Pandoc
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Models.Types
import Web.Blog.Models.Util
import qualified Data.Text as T

main :: IO ()
main = runDB $ do
  blogMigrate
  blogClear

  tags <- replicateM 6 $ do
    t <- liftIO $ (map toLower . unwords . reverse . take 2 . reverse . words . filter (not . isPunctuation) . unwords . lines)
      <$> genLoripsum "http://loripsum.net/api/1/short"
    insertTag $ PreTag (T.pack t) GeneralTag

  categories <- replicateM 4 $ do
    c <- liftIO $ (capitalizeFirst . unwords . reverse . take 2 . reverse . words . filter (not . isPunctuation) . unwords . lines)
      <$> genLoripsum "http://loripsum.net/api/1/short"
    insertTag $ PreTag (T.pack c) CategoryTag

  serieses <- replicateM 3 $ do
    s <- liftIO $ (capitalizeFirst . unwords . reverse . take 5 . reverse . words . filter (not . isPunctuation) . unwords . lines)
      <$> genLoripsum "http://loripsum.net/api/1/short"
    insertTag $ PreTag (T.pack s) SeriesTag


  replicateM_ 25 $ do
    (entry,tags',category,series) <- liftIO genEntry
    entryid <- insertEntry entry

    let
      powers = map (\x -> (x,2^x)) ([0..5] :: [Int])

    forM_ powers $ \(tagNum,b) ->
      when ((tags' .&. b) > 0) $ 
        insert_ $ EntryTag entryid $ tags !! tagNum

    insert_ $ EntryTag entryid $ categories !! category

    when (series < 3) $ 
      insert_ $ EntryTag entryid $ serieses !! series
    
    tagAssociations <- selectList [EntryTagEntryId ==. entryid] []

    let
      tagKeys = map (entryTagTagId . entityVal) tagAssociations

    tags'' <- map (tagLabel . fromJust) <$> mapM get tagKeys

    liftIO $ do
      putStrLn "Created new entry:"
      print $ entryTitle entry
      print tags''
      hFlush stdout


  liftIO $ putStrLn "Seed complete!"

  return ()

genEntry :: IO (Entry,Int,Int,Int)
genEntry = do
  now <- getCurrentTime

  gen <- newStdGen

  let
    (createTime, postTime, tags, category, series) = (evalRand $ do
      cD <- getRandomR (-31536000,-604800)
      pD <- getRandomR (100,604800) 
      ts <- getRandomR (1,63)
      c  <- getRandomR (0,3)
      s  <- getRandomR (0,11)
      return
        ( addUTCTime (fromIntegral (cD :: Int)) now
        , addUTCTime (fromIntegral (pD :: Int)) createTime
        , ts
        , c
        , s
        )
      ) gen
      

  fullEntry <- genLoripsum "http://loripsum.net/api/7/code/bq/ul/ol/dl/link/long/decorate/headers"

  let
    title = init    $ head   $ lines fullEntry
    body  = unlines $ drop 3 $ lines fullEntry

  let
    e = Entry
      (T.pack title)
      (T.pack body)
      createTime
      postTime

  return (e, tags, category, series)


genLoripsum :: String -> IO String
genLoripsum apiUrl = do
  rsp <- simpleHTTP (getRequest apiUrl)
  body <- getResponseBody rsp

  return $ writeMarkdown (def WriterOptions) $
    readHtml (def ReaderOptions) body

capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = toUpper x:map toLower xs
capitalizeFirst x = map toUpper x
