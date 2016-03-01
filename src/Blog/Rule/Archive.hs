{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Blog.Rule.Archive where

import           Blog.Util
import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Data.Traversable
import           Hakyll
import qualified Data.Map.Strict     as M
import qualified Data.Set            as S

type Year = Integer
data Month = JAN | FEB | MAR | APR | MAY | JUN
           | JUL | AUG | SEP | OCT | NOV | DEC
  deriving (Show, Eq, Ord, Enum)


data History = History
    { historyMap        :: M.Map Year (M.Map Month [Identifier])
    , historyMakeId     :: Year -> Maybe Month -> Identifier
    , historyDependency :: Dependency
    }

buildHistoryWith
    :: MonadMetadata m
    => (Identifier -> m (Maybe (Year, Month)))
    -> Pattern
    -> (Year -> Maybe Month -> Identifier)
    -> m History
buildHistoryWith f p r = do
    ids <- getMatches p
    idDates <- fmap catMaybes . forM ids $ \i -> fmap (i,) <$> f i
    let idsSet = S.fromList ids
        hMap   = M.fromList
               . map (\xs@((_,(y,_)):_) ->
                       (y, M.fromListWith (++) (map (\(i,(_,m)) -> (m, [i])) xs))
                     )
               . groupBy ((==) `on` fst)
               $ sort idDates
    return History
        { historyMap        = hMap
        , historyMakeId     = r
        , historyDependency = PatternDependency p idsSet
        }

historyRules
    :: History
    -> (Year -> Maybe Month -> Pattern -> Rules ())
    -> Rules ()
historyRules (History{..}) r = do
    forM_ (M.toList historyMap) $ \(y, ms) -> do
      rulesExtraDependencies [historyDependency] $
        create [historyMakeId y Nothing] $
          r y Nothing (fromList $ concat (M.elems ms))

      forM_ (M.toList ms) $ \(m, ids) -> do
        rulesExtraDependencies [historyDependency] $
          create [historyMakeId y (Just m)] $
            r y (Just m) (fromList ids)

ymByField
    :: MonadMetadata m
    => Identifier
    -> m (Maybe (Year, Month))
ymByField i = (f =<<) <$> getMetadataField i "date"
  where
    f :: String -> Maybe (Year, Month)
    f d = flip fmap (parseETime d) $ \t ->
      let (y, m, _) = toGregorian $ localDay t
      in  (y, toEnum (m - 1))


