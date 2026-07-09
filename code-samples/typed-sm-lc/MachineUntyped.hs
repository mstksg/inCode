{-# LANGUAGE OverloadedStrings #-}

module MachineUntyped where

import Data.Map (Map)
import qualified Data.Map as M

data UntypedTransition = UntypedTransition
  { utFrom    :: String
  , utTrigger :: String
  , utTo      :: String
  , utGuard   :: Map String Int -> Bool
  , utBuild   :: Map String Int -> Map String Int
  }

type UntypedMachine = [UntypedTransition]

badTransition1 :: UntypedTransition
badTransition1 = UntypedTransition
  { utFrom    = "Scanning"
  , utTrigger = "Peak"
  , utTo      = "Lockde"          -- typo: "Lockde" instead of "Locked"
  , utGuard   = \env -> (env M.! "rssi") > 4
  , utBuild   = \env -> M.singleton "lockRssi" (env M.! "rssi")
  }

badTransition2 :: UntypedTransition
badTransition2 = UntypedTransition
  { utFrom    = "Scanning"
  , utTrigger = "Peak"
  , utTo      = "Locked"
  , utGuard   = \env -> (env M.! "lockRssi") > 4  -- lockRssi doesn't exist while Scanning!
  , utBuild   = \env -> M.singleton "freq" (env M.! "rssi")
                        -- missing "lockRssi" and "dwell" that Locked needs
  }

scanMachine :: UntypedMachine
scanMachine =
  [ UntypedTransition "Idle" "Start" "Scanning"
      (const True)
      (\_ -> M.fromList [("freq", 2400), ("dwell", 0)])
  , UntypedTransition "Scanning" "Peak" "Locked"
      (\env -> (env M.! "rssi") >= (env M.! "noise") + 4)
      (\env -> M.fromList
        [ ("freq", (env M.! "freq") + (env M.! "peakOffset"))
        , ("lockRssi", env M.! "rssi")
        , ("dwell", (env M.! "dwell") + 1)
        ])
  , UntypedTransition "Scanning" "Timeout" "CoolingDown"
      (\env -> (env M.! "dwell") >= 3)
      (\env -> M.singleton "lastFreq" (env M.! "freq"))
  , UntypedTransition "Locked" "Drift" "Scanning"
      (const True)
      (\env -> M.fromList
        [ ("freq", (env M.! "freq") + (env M.! "peakOffset"))
        , ("dwell", env M.! "dwell")
        ])
  , UntypedTransition "CoolingDown" "Reset" "Idle"
      (const True)
      (const M.empty)
  ]

main :: IO ()
main = putStrLn $ "Machine has " ++ show (length scanMachine) ++ " transitions"
