#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver lts-10.3 --package free -- -Wall

{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Alternative.Free.Final
import           Control.Applicative
import           Data.Bifunctor
import           Data.Foldable
import           Form
import           Text.Printf
import           Text.Read

newtype FormIO a = FormIO { runFormIO :: IO (Either [String] a) }
  deriving Functor

instance Applicative FormIO where
    pure = FormIO . return . Right
    ff <*> fx = FormIO $ do
      rf <- runFormIO ff
      rx <- runFormIO fx
      return $ case (rf, rx) of
        (Left ef, Left ex) -> Left (ef ++ ex)
        (Left ef, Right _) -> Left ef
        (Right _, Left ex) -> Left ex
        (Right f, Right x) -> Right (f x)

instance Alternative FormIO where
    empty = FormIO $ return (Left [])
    fx <|> fy = FormIO $ do
      rx <- runFormIO fx
      case rx of
        Right x -> return (Right x)
        Left ex -> do
          ry <- runFormIO fy
          return $ case ry of
            Right y -> Right y
            Left ey -> Left (ex ++ ey)

formIO :: Form a -> IO (Either [String] a)
formIO = runFormIO . runAlt go
  where
    go :: FormElem x -> FormIO x
    go FE{..} = FormIO . (fmap . first) (:[]) $ do
      printf "%s?\n" feDesc
      case feElem of
        EText   -> feParse <$> getLine
        ENumber -> do
          res <- readMaybe <$> getLine
          return $ case res of
            Nothing -> Left $ "Could not parse " ++ feDesc
            Just n  -> feParse n
        ESelect opts -> do
          forM_ (zip [0..] opts) $ \(i :: Int,o) -> do
            printf "[%d] %s\n" i o
          feParse . readMaybe <$> getLine
        ECheck x y -> do
          printf "[n] %s / (y) %s\n" x y
          feParse . (== "y") <$> getLine

main :: IO ()
main = print =<< formIO accountForm
