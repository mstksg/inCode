{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Blog.Rule.Archive where

import           Blog.Types
import           Blog.Util
import           Control.Monad
import           Data.Maybe
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Hakyll
import qualified Data.Map.Strict     as M
import qualified Data.Set            as S

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
        hMap   = M.unionsWith (M.unionWith (++))
               . map (\(i, (y, m)) -> M.singleton y (M.singleton m [i]))
               $ idDates
    return History
        { historyMap        = hMap
        , historyMakeId     = r
        , historyDependency = PatternDependency p idsSet
        }

historyRules
    :: History
    -> (Either (Year, M.Map Month Pattern) ((Year, Month), Pattern) -> Rules ())
    -> Rules ()
historyRules h f = historyRules' h $ \case
                                       Left  (y , mp) -> f $ Left (y, fromList <$> mp)
                                       Right (ym, is) -> f $ Right (ym, fromList is)

historyRules'
    :: History
    -> (Either (Year, M.Map Month [Identifier]) ((Year, Month), [Identifier]) -> Rules ())
    -> Rules ()
historyRules' (History{..}) r = do
    void . flip M.traverseWithKey historyMap $ \y ms -> do
      rulesExtraDependencies [historyDependency] $
        create [historyMakeId y Nothing] $
          r (Left (y, ms))

      void . flip M.traverseWithKey ms $ \m ids -> do
        rulesExtraDependencies [historyDependency] $
          create [historyMakeId y (Just m)] $
            r (Right ((y, m), ids))

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


