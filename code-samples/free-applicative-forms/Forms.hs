#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver lts-10.3 --package free -- -Wall

{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeInType         #-}

import           Control.Alternative.Free.Final
import           Control.Applicative
import           Data.Bool
import           Data.Kind
import           Data.Scientific
import           Text.Read

data Elem :: Type -> Type where
    EText   :: String                -> Elem String
    ENumber :: Maybe Scientific      -> Elem Scientific
    ESelect :: Maybe Int -> [String] -> Elem (Maybe Int)
    ECheck  :: String    -> String   -> Elem Bool

data FormElem :: Type -> Type where
    FE :: { feElem  :: Elem a
          , feParse :: a -> Either String b
          , feDesc  :: String
          , feIdent :: String
          }
        -> FormElem b

deriving instance Functor FormElem

type Form = Alt FormElem

stringInput
    :: String           -- ^ description
    -> String           -- ^ identifier
    -> String           -- ^ initial
    -> Form String
stringInput desc ident initial = liftAlt $
    FE (EText initial) Right desc ident

readInput
    :: (Read a, Show a)
    => String           -- ^ description
    -> String           -- ^ identifier
    -> Maybe a          -- ^ initial
    -> Form a
readInput desc ident initial = liftAlt $
    FE (EText (maybe "" show initial)) p desc ident
  where
    p = maybe (Left ("Could not parse " ++ ident)) Right
      . readMaybe

intInput
    :: Integral a
    => String     -- ^ description
    -> String     -- ^ identifier
    -> Maybe a    -- ^ initial
    -> Form a
intInput desc ident initial = liftAlt $
    FE (ENumber (fromIntegral <$> initial)) p desc ident
  where
    p = either (\_ -> Left (ident ++ " should be integer")) Right
      . floatingOrInteger @Double

floatInput
    :: String           -- ^ description
    -> String           -- ^ identifier
    -> Maybe Double     -- ^ initial
    -> Form Double
floatInput desc ident initial = liftAlt $
    FE (ENumber (realToFrac <$> initial)) p desc ident
  where
    p = either Right (\_ -> Left (ident ++ " should be float"))
      . floatingOrInteger @Double @Integer

selectInput
    :: Show a
    => String           -- ^ description
    -> String           -- ^ identifier
    -> Maybe Int        -- ^ initial
    -> [a]              -- ^ options
    -> Form a
selectInput desc ident initial opts = liftAlt $
    FE (ESelect initial (show <$> opts)) p desc ident
  where
    p = maybe (Left ("No selection for " ++ ident)) (Right . (opts !!))

checkInput
    :: Show a
    => String           -- ^ description
    -> String           -- ^ identifier
    -> a                -- ^ unchecked option
    -> a                -- ^ checked option
    -> Form a
checkInput desc ident x y = liftAlt $
    FE (ECheck (show x) (show y)) p desc ident
  where
    p = Right . bool x y

data AccountType = Normal | Premium
    deriving Show
data Color = Red | Blue | Orange | Yellow
    deriving Show

data Account = Acc { accName     :: String
                   , accCountry  :: Maybe String
                   , accAge      :: Int
                   , accFavColor :: Either Color String
                   , accPremium  :: AccountType
                   }
    deriving Show

accountForm :: Form Account
accountForm =
    Acc <$> stringInput "Name" "name" ""
        <*> optional (stringInput "Country" "country" "USA")
        <*> intInput "Age" "age" Nothing
        <*> ((Left <$> favColor) <|> (Right <$> customColor))
        <*> checkInput "Premium Account" "premium" Normal Premium 
  where
    favColor    = selectInput "Favorite Color" "fav-color" Nothing
                    [Red, Blue, Orange, Yellow]
    customColor = stringInput "Custom Color" "custum-color" ""

main :: IO ()
main = putStrLn "hi"
