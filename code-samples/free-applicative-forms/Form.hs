#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver lts-10.3 --package free -- -Wall

{-# LANGUAGE ApplicativeDo      #-}
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
    -- | Text field, with initial contents
    EText   :: String                -> Elem String
    -- | Numberic field, with initial number
    ENumber :: Maybe Scientific      -> Elem Scientific
    -- | Select box, with initial selection and list of options to display
    ESelect :: Maybe Int -> [String] -> Elem (Maybe Int)
    -- | Check box, with the labels to attach to on/off states
    ECheck  :: String    -> String   -> Elem Bool

data FormElem :: Type -> Type where
    FE :: { feElem  :: Elem b
          , feParse :: b -> Either String a
          , feDesc  :: String
          , feIdent :: String
          }
        -> FormElem a

deriving instance Functor FormElem

type Form = Alt FormElem

stringInput
    :: String           -- ^ description
    -> String           -- ^ identifier
    -> String           -- ^ initial
    -> Form String
stringInput desc ident initial = liftAlt $
    FE { feElem  = EText initial
       , feParse = Right
       , feDesc  = desc
       , feIdent = ident
       }

readInput
    :: (Read a, Show a)
    => String           -- ^ description
    -> String           -- ^ identifier
    -> Maybe a          -- ^ initial
    -> Form a
readInput desc ident initial = liftAlt $
    FE { feElem  = EText (maybe "" show initial)
       , feParse = maybe (Left ("Could not parse " ++ ident)) Right
                 . readMaybe
       , feDesc  = desc
       , feIdent = ident
       }

intInput
    :: Integral a
    => String     -- ^ description
    -> String     -- ^ identifier
    -> Maybe a    -- ^ initial
    -> Form a
intInput desc ident initial = liftAlt $
    FE { feElem  = ENumber (fromIntegral <$> initial)
       , feParse = either (\_ -> Left (ident ++ " should be integer")) Right
                 . floatingOrInteger @Double
       , feDesc  = desc
       , feIdent = ident
       }

floatInput
    :: RealFloat a
    => String           -- ^ description
    -> String           -- ^ identifier
    -> Maybe a          -- ^ initial
    -> Form a
floatInput desc ident initial = liftAlt $
    FE { feElem  = ENumber (realToFrac <$> initial)
       , feParse = Right . realToFrac
       , feDesc  = desc
       , feIdent = ident
       }
  where

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

boolInput :: String -> String -> Form Bool
boolInput desc ident = checkInput desc ident False True

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
        <*> (Left <$> favColor <|> Right <$> customColor)
        <*> checkInput "Premium Account" "premium" Normal Premium 
  where
    favColor    = selectInput "Favorite Color" "fav-color" Nothing
                    [Red, Blue, Orange, Yellow]
    customColor = stringInput "Custom Color" "custum-color" ""

accountFormAdo :: Form Account
accountFormAdo = do
    nam <- stringInput "Name" "name" ""
    con <- optional $ stringInput "Country" "country" "USA"
    age <- intInput "Age" "age" Nothing
    col <- Left  <$> selectInput "Favorite Color" "fav-color" Nothing
                       [Red, Blue, Orange, Yellow]
       <|> Right <$> stringInput "Custom Color" "custum-color" ""
    typ <- checkInput "Premium Account" "premium" Normal Premium 
    pure (Acc nam con age col typ)

main :: IO ()
main = putStrLn "hi"
