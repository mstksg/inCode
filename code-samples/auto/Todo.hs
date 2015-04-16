{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Arrows        #-}
{-# LANGUAGE TupleSections #-}

module Todo where

import Control.Auto
import Control.Auto.Collection
import Control.Monad.Fix
import Data.IntMap             (IntMap)
import Data.Serialize
import GHC.Generics
import Prelude hiding          ((.), id)
import qualified Data.IntMap   as IM

data TodoInp = IAdd  String
             | ITask TaskID TaskCmd
             | IAll TaskCmd
             deriving Show

data TaskCmd = CDelete
             | CPrune
             | CComplete Bool
             | CModify String
             | CNop
             deriving Show

type TaskMap = IntMap Task
type TaskID  = Int

data Task = Task { taskDescr     :: String
                 , taskCompleted :: Bool
                 } deriving (Show, Generic)

instance Serialize Task -- from Data.Serialize, from the cereal library

taskCollection :: Monad m
               => Auto m (IntMap TaskCmd, Blip [String]) (IntMap Task)
taskCollection = dynMapF initTask CNop

initTask :: Monad m => String -> Interval m TaskCmd Task
initTask descr = accum f (Just (Task descr False))
  where
    f (Just t) tc = case tc of
                      CDelete                  -> Nothing
                      CPrune | taskCompleted t -> Nothing
                             | otherwise       -> Just t
                      CComplete s              -> Just t { taskCompleted = s }
                      CModify descr            -> Just t { taskDescr = descr }
                      CNop                     -> Just t
    f Nothing _   = Nothing

todoApp :: MonadFix m => Auto m TodoInp (IntMap Task)
todoApp = proc inpEvt -> do

    rec allIds <- arrD IM.keys [] -< taskMap

        -- "forking" `inpEvt` into three blip streams:
        newTaskB  <- emitJusts getAddEvts  -< inpEvt
        modTaskB  <- emitJusts getModEvts  -< inpEvt
        massTaskB <- emitJusts getMassEvts -< (allIds, inpEvt)

        -- merge the two streams together to get "all" inputs, single and
        -- mass.
        let allInpB = modTaskB <> massTaskB

        -- from a blip stream to an `IntMap` stream that is empty when the
        -- stream doesn't emit
        taskCommands <- fromBlips IM.empty -< allInpB

        -- feed the commands and the new tasks to `taskMap`...the result is
        -- the `IntMap` of tasks.
        taskMap <- taskCollection -< (taskCommands, newTaskB)

    id -< taskMap

getAddEvts :: TodoInp -> Maybe [String]
getAddEvts (IAdd descr) = Just [descr]
getAddEvts _            = Nothing

getModEvts :: TodoInp -> Maybe (IntMap TaskCmd)
getModEvts (ITask n te) = Just $ IM.singleton n te
getModEvts _            = Nothing

getMassEvts :: ([TaskID], TodoInp) -> Maybe (IntMap TaskCmd)
getMassEvts (allIds, IAll te) = Just $ IM.fromList (map (,te) allIds)
getMassEvts _                 = Nothing
