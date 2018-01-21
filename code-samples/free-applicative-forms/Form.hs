{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeInType         #-}

module Form where

import           Control.Alternative.Free.Final
import           Control.Applicative
import           Control.Monad
import           Data.Bool
import           Data.Kind
import           Data.Scientific
import           Text.Read

data Elem :: Type -> Type where
    -- | Text field
    EText   :: Elem String
    -- | Numberic field
    ENumber :: Elem Scientific
    -- | Select box, with list of options to display
    ESelect :: [String] -> Elem (Maybe Int)
    -- | Check box, with the labels to attach to on/off states
    ECheck  :: String -> String -> Elem Bool

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
    -> Form String
stringInput desc ident = liftAlt $
    FE { feElem  = EText
       , feParse = mfilter (not . null) . Right
       , feDesc  = desc
       , feIdent = ident
       }

readInput
    :: (Read a, Show a)
    => String           -- ^ description
    -> String           -- ^ identifier
    -> Form a
readInput desc ident = liftAlt $
    FE { feElem  = EText
       , feParse = maybe (Left ("Could not parse " ++ ident)) Right
                 . readMaybe
       , feDesc  = desc
       , feIdent = ident
       }

intInput
    :: Integral a
    => String     -- ^ description
    -> String     -- ^ identifier
    -> Form a
intInput desc ident = liftAlt $
    FE { feElem  = ENumber
       , feParse = either (\_ -> Left (ident ++ " should be integer")) Right
                 . floatingOrInteger @Double
       , feDesc  = desc
       , feIdent = ident
       }

floatInput
    :: RealFloat a
    => String           -- ^ description
    -> String           -- ^ identifier
    -> Form a
floatInput desc ident = liftAlt $
    FE { feElem  = ENumber
       , feParse = Right . realToFrac
       , feDesc  = desc
       , feIdent = ident
       }

selectInput
    :: Show a
    => String           -- ^ description
    -> String           -- ^ identifier
    -> [a]              -- ^ options
    -> Form a
selectInput desc ident opts = liftAlt $
    FE (ESelect (show <$> opts)) p desc ident
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
                   , accAge      :: Maybe Int
                   , accFavColor :: Either Color String
                   , accPremium  :: AccountType
                   }
    deriving Show

accountForm :: Form Account
accountForm =
    Acc <$> stringInput "Name" "name"
        <*> optional (intInput "Age" "age")
        <*> (Left <$> favColor <|> Right <$> customColor)
        <*> checkInput "Premium Account" "premium" Normal Premium 
  where
    favColor    = selectInput "Favorite Color" "fav-color"
                    [Red, Blue, Orange, Yellow]
    customColor = stringInput "Custom Color" "custum-color"

accountFormAdo :: Form Account
accountFormAdo = do
    nam <- stringInput "Name" "name"
    age <- optional (intInput "Age" "age")
    col <- Left  <$> selectInput "Favorite Color" "fav-color"
                       [Red, Blue, Orange, Yellow]
       <|> Right <$> stringInput "Custom Color" "custom-color"
    typ <- checkInput "Premium Account" "premium" Normal Premium 
    pure (Acc nam age col typ)
