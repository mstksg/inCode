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

data TodoInp = IAdd  String           -- new task with description
             | ITask TaskID TaskCmd   -- send command to task by ID
             | IAll TaskCmd           -- send command to all tasks
             deriving Show

data TaskCmd = CDelete          -- delete
             | CPrune           -- delete if completed
             | CComplete Bool   -- set completed status
             | CModify String   -- modify description
             | CNop             -- do nothing
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

    rec -- all id's currently alive
        allIds <- arrD IM.keys [] -< taskMap

        -- "forking" `inpEvt` into three blip streams:
        -- newTaskB :: Blip [String]
        newTaskB  <- emitJusts getAddEvts  -< inpEvt
        -- modTaskB :: Blip (IntMap TaskCmd)
        modTaskB  <- emitJusts getModEvts  -< inpEvt
        -- massTaskB :: Blip (IntMap TaskCmd)
        massTaskB <- emitJusts getMassEvts -< (allIds, inpEvt)

        -- merge the two streams together to get "all" inputs, single and
        -- mass.
        let allInpB :: Blip (IntMap TaskCmd)
            allInpB = modTaskB <> massTaskB

        -- from a blip stream to an `IntMap` stream that is empty when the
        -- stream doesn't emit
        -- taskCommands :: IntMap TaskCmd
        taskCommands <- fromBlips IM.empty -< allInpB

        -- feed the commands and the new tasks to `taskMap`...the result is
        -- the `IntMap` of tasks.
        -- taskMap :: IntMap Task
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
