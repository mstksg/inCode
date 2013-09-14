{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE TypeSynonymInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.Random
import Data.Char
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
import qualified Data.Text    as T

main :: IO ()
main = runDB $ do
  blogMigrate
  blogClear

  tags <- replicateM 10 $
    untilJust $ do
      t <- liftIO $ (T.pack . map toLower . unwords . reverse . take 2 . reverse . words . filter (not . isPunctuation) . unwords . lines)
        <$> genLoripsum "http://loripsum.net/api/1/short"
      desc <- liftIO $ genDesc 4
      insertTag $ PreTag t GeneralTag desc

  categories' <- replicateM 4 $
    untilJust $ do
      c <- liftIO $ (T.pack . capitalizeFirst . unwords . reverse . take 2 . reverse . words . filter (not . isPunctuation) . unwords . lines)
        <$> genLoripsum "http://loripsum.net/api/1/short"
      desc <- liftIO $ (Just . T.pack) <$> genLoripsum "http://loripsum.net/api/1/short"
      insertTag $ PreTag c CategoryTag desc

  extracat <- untilJust $ do
    c <- liftIO $ (T.pack . capitalizeFirst . unwords . reverse . take 2 . reverse . words . filter (not . isPunctuation) . unwords . lines)
      <$> genLoripsum "http://loripsum.net/api/1/short"
    insertTag $ PreTag c CategoryTag Nothing

  serieses' <- replicateM 3 $
    untilJust $ do
      s <- liftIO $ (T.pack . capitalizeFirst . unwords . reverse . take 5 . reverse . words . filter (not . isPunctuation) . unwords . lines)
        <$> genLoripsum "http://loripsum.net/api/1/short"
      desc <- liftIO $ (Just . T.pack) <$> genLoripsum "http://loripsum.net/api/1/short"
      insertTag $ PreTag s SeriesTag desc

  extraser <- untilJust $ do
    s <- liftIO $ (T.pack . capitalizeFirst . unwords . reverse . take 5 . reverse . words . filter (not . isPunctuation) . unwords . lines)
      <$> genLoripsum "http://loripsum.net/api/1/short"
    insertTag $ PreTag s SeriesTag Nothing

  let
    categories = extracat:categories'
    serieses = extraser:serieses'


  replicateM_ 40 $ do
    ((entry,tags',category,series),entryid) <-
      untilJust $ do
        d@(e,_,_,_) <- liftIO genEntry
        eid <- insertEntry e
        case eid of
          Just eid' ->
            return $ Just (d,eid')
          Nothing ->
            return Nothing

    forM_ (zip tags' [0..]) $ \(odds,tnum) ->
      when (odds < 1) $
        insert_ $ EntryTag entryid $ tags !! tnum

    insert_ $ EntryTag entryid $ categories !! category

    when (series < 4) $
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

genEntry :: IO (Entry,[Double],Int,Int)
genEntry = do
  now <- getCurrentTime

  gen <- newStdGen

  let
    (createTime, postTime, tags, category, series) = (evalRand $ do
      cD <- getRandomR (-31536000,8035200)
      pD <- getRandomR (100,604800)
      ts <- forM [1..10] $ \i -> getRandomR (0,i*i/4+1)
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


  fullEntry <- genLoripsum "http://loripsum.net/api/12/code/bq/ul/ol/dl/link/long/decorate/headers"

  let
    title = init    $ head   $ lines fullEntry
    body  = unlines $ drop 3 $ lines fullEntry

  let
    e = Entry
      (T.pack title)
      (T.pack body)
      Nothing
      (Just createTime)
      (Just postTime)
      Nothing
      Nothing

  return (e, tags, category, series)


genLoripsum :: String -> IO String
genLoripsum apiUrl = do
  rsp <- simpleHTTP (getRequest apiUrl)
  body <- getResponseBody rsp

  return $ writeMarkdown (def WriterOptions) $
    readHtml (def ReaderOptions) body

genDesc :: Int -> IO (Maybe T.Text)
genDesc o = do
  possible <- T.pack <$> genLoripsum "http://loripsum.net/api/1/short"
  return $
    if T.length possible `mod` o == 0
      then
        Just possible
      else
        Nothing

capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = toUpper x:map toLower xs
capitalizeFirst x = map toUpper x
