{-# OPTIONS_GHC -Wall -Werror=incomplete-patterns #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module MachineStage5 where

import Data.Kind (Type)
import Data.List (intercalate)
import ExprStage4 hiding (NameField, main)
import GHC.TypeLits (Symbol)
import System.Environment (getArgs)

type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type data ScanState
  = Idle
  | Scanning
  | Locked
  | CoolingDown

type data Trigger
  = Start
  | Peak
  | Drift
  | Timeout
  | Reset

type family NodeVars (s :: ScanState) where
  NodeVars Idle = '[]
  NodeVars Scanning = '["freq" ::: TInt, "dwell" ::: TInt]
  NodeVars Locked = '["freq" ::: TInt, "lockRssi" ::: TInt, "dwell" ::: TInt]
  NodeVars CoolingDown = '["lastFreq" ::: TInt]

type Env =
  '[ "rssi" ::: TInt,
     "noise" ::: TInt,
     "peakOffset" ::: TInt
   ]

type Scope s = Env ++ NodeVars s

data SState :: ScanState -> Type where
  SIdle :: SState Idle
  SScanning :: SState Scanning
  SLocked :: SState Locked
  SCoolingDown :: SState CoolingDown

data STrigger :: Trigger -> Type where
  SStart :: STrigger Start
  SPeak :: STrigger Peak
  SDrift :: STrigger Drift
  STimeout :: STrigger Timeout
  SReset :: STrigger Reset

stateName :: SState s -> String
stateName = \case
  SIdle -> "Idle"
  SScanning -> "Scanning"
  SLocked -> "Locked"
  SCoolingDown -> "CoolingDown"

triggerName :: STrigger t -> String
triggerName = \case
  SStart -> "Start"
  SPeak -> "Peak"
  SDrift -> "Drift"
  STimeout -> "Timeout"
  SReset -> "Reset"

data NameField :: (Symbol, Ty) -> Type where
  NameField :: String -> NameField field

envNames :: Rec NameField Env
envNames =
  NameField "rssi"
    :& NameField "noise"
    :& NameField "peakOffset"
    :& RNil

idleNames :: Rec NameField (Scope Idle)
idleNames = envNames

scanningNames :: Rec NameField (Scope Scanning)
scanningNames =
  NameField "rssi"
    :& NameField "noise"
    :& NameField "peakOffset"
    :& NameField "freq"
    :& NameField "dwell"
    :& RNil

lockedNames :: Rec NameField (Scope Locked)
lockedNames =
  NameField "rssi"
    :& NameField "noise"
    :& NameField "peakOffset"
    :& NameField "freq"
    :& NameField "lockRssi"
    :& NameField "dwell"
    :& RNil

coolingDownNames :: Rec NameField (Scope CoolingDown)
coolingDownNames =
  NameField "rssi"
    :& NameField "noise"
    :& NameField "peakOffset"
    :& NameField "lastFreq"
    :& RNil

stateNames :: SState s -> Rec NameField (Scope s)
stateNames = \case
  SIdle -> idleNames
  SScanning -> scanningNames
  SLocked -> lockedNames
  SCoolingDown -> coolingDownNames

data BuildField scope :: (Symbol, Ty) -> Type where
  BuildField :: String -> Expr scope t -> BuildField scope (name ::: t)

data Build scope to = Build
  { buildName :: String,
    buildFields :: Rec (BuildField scope) (NodeVars to)
  }

data Transition env from trigger to where
  Transition ::
    SState from ->
    STrigger trigger ->
    SState to ->
    Expr (env ++ NodeVars from) TBool ->
    Build (env ++ NodeVars from) to ->
    Transition env from trigger to

data SomeTransition env where
  SomeTransition :: Transition env from trigger to -> SomeTransition env

newtype Machine env = Machine [SomeTransition env]

true :: Expr scope TBool
true = EPrim (PBool True)

rssiScanning :: Expr (Scope Scanning) TInt
rssiScanning = EVar IZ

noiseScanning :: Expr (Scope Scanning) TInt
noiseScanning = EVar (IS IZ)

peakOffsetScanning :: Expr (Scope Scanning) TInt
peakOffsetScanning = EVar (IS (IS IZ))

rssiLocked :: Expr (Scope Locked) TInt
rssiLocked = EVar IZ

noiseLocked :: Expr (Scope Locked) TInt
noiseLocked = EVar (IS IZ)

peakOffsetLocked :: Expr (Scope Locked) TInt
peakOffsetLocked = EVar (IS (IS IZ))

scanFreq :: Expr (Scope Scanning) TInt
scanFreq = EVar (IS (IS (IS IZ)))

scanDwell :: Expr (Scope Scanning) TInt
scanDwell = EVar (IS (IS (IS (IS IZ))))

lockFreq :: Expr (Scope Locked) TInt
lockFreq = EVar (IS (IS (IS IZ)))

lockRssi :: Expr (Scope Locked) TInt
lockRssi = EVar (IS (IS (IS (IS IZ))))

lockDwell :: Expr (Scope Locked) TInt
lockDwell = EVar (IS (IS (IS (IS (IS IZ)))))

threshold :: Expr scope TInt
threshold = EPrim (PInt 4)

maxDwell :: Expr scope TInt
maxDwell = EPrim (PInt 3)

zero :: Expr scope TInt
zero = EPrim (PInt 0)

baseFreq :: Expr scope TInt
baseFreq = EPrim (PInt 2400)

one :: Expr scope TInt
one = EPrim (PInt 1)

strongEnoughScanning :: Expr (Scope Scanning) TBool
strongEnoughScanning = EOp OLte (EOp OPlus noiseScanning threshold) rssiScanning

strongEnoughLocked :: Expr (Scope Locked) TBool
strongEnoughLocked = EOp OLte (EOp OPlus noiseLocked threshold) rssiLocked

scanTooLong :: Expr (Scope Scanning) TBool
scanTooLong = EOp OLte maxDwell scanDwell

lockedTooLong :: Expr (Scope Locked) TBool
lockedTooLong = EOp OLte maxDwell lockDwell

scanNextFreq :: Expr (Scope Scanning) TInt
scanNextFreq = EOp OPlus scanFreq peakOffsetScanning

lockNextFreq :: Expr (Scope Locked) TInt
lockNextFreq = EOp OPlus lockFreq peakOffsetLocked

scanNextDwell :: Expr (Scope Scanning) TInt
scanNextDwell = EOp OPlus scanDwell one

lockNextDwell :: Expr (Scope Locked) TInt
lockNextDwell = EOp OPlus lockDwell one

startScan :: Build (Scope Idle) Scanning
startScan =
  Build
    "StartScan"
    ( BuildField "freq" baseFreq
        :& BuildField "dwell" zero
        :& RNil
    )

lockOnPeak :: Build (Scope Scanning) Locked
lockOnPeak =
  Build
    "LockOnPeak"
    ( BuildField "freq" scanNextFreq
        :& BuildField "lockRssi" rssiScanning
        :& BuildField "dwell" scanNextDwell
        :& RNil
    )

keepScanning :: Build (Scope Locked) Scanning
keepScanning =
  Build
    "TrackDrift"
    ( BuildField "freq" lockNextFreq
        :& BuildField "dwell" lockDwell
        :& RNil
    )

coolFromScanning :: Build (Scope Scanning) CoolingDown
coolFromScanning =
  Build
    "CoolScanner"
    (BuildField "lastFreq" scanFreq :& RNil)

coolFromLock :: Build (Scope Locked) CoolingDown
coolFromLock =
  Build
    "CoolLock"
    (BuildField "lastFreq" lockFreq :& RNil)

idle :: Build (Scope CoolingDown) Idle
idle = Build "ResetToIdle" RNil

quickScanMachine :: Machine Env
quickScanMachine =
  Machine
    [ SomeTransition $
        Transition
          SIdle
          SStart
          SScanning
          true
          startScan,
      SomeTransition $
        Transition
          SScanning
          SPeak
          SLocked
          strongEnoughScanning
          lockOnPeak,
      SomeTransition $
        Transition
          SScanning
          STimeout
          SCoolingDown
          scanTooLong
          coolFromScanning,
      SomeTransition $
        Transition
          SLocked
          SDrift
          SScanning
          true
          keepScanning,
      SomeTransition $
        Transition
          SCoolingDown
          SReset
          SIdle
          true
          idle
    ]

dwellScanMachine :: Machine Env
dwellScanMachine =
  Machine
    [ SomeTransition $
        Transition
          SIdle
          SStart
          SScanning
          true
          startScan,
      SomeTransition $
        Transition
          SScanning
          SPeak
          SLocked
          strongEnoughScanning
          lockOnPeak,
      SomeTransition $
        Transition
          SScanning
          STimeout
          SCoolingDown
          scanTooLong
          coolFromScanning,
      SomeTransition $
        Transition
          SLocked
          SPeak
          SLocked
          strongEnoughLocked
          ( Build
              "RefreshLock"
              ( BuildField "freq" lockFreq
                  :& BuildField "lockRssi" rssiLocked
                  :& BuildField "dwell" lockNextDwell
                  :& RNil
              )
          ),
      SomeTransition $
        Transition
          SLocked
          SDrift
          SScanning
          true
          keepScanning,
      SomeTransition $
        Transition
          SLocked
          STimeout
          SCoolingDown
          lockedTooLong
          coolFromLock,
      SomeTransition $
        Transition
          SCoolingDown
          SReset
          SIdle
          true
          idle
    ]

machines :: [(String, Machine Env)]
machines =
  [ ("Quick scan", quickScanMachine),
    ("Dwell scan", dwellScanMachine)
  ]

compileDot :: [(String, Machine Env)] -> String
compileDot xs =
  block
    [ "digraph TypedSmLc {",
      "  rankdir=LR;",
      "  node [shape=box];",
      indent 2 (intercalate "\n" (map (uncurry renderMachineDot) xs)),
      "}"
    ]

renderMachineDot :: String -> Machine Env -> String
renderMachineDot name (Machine transitions) =
  block
    [ "subgraph cluster_" ++ dotId name ++ " {",
      "  label=" ++ jsString name ++ ";",
      indent 2 (intercalate "\n" (map (renderSomeTransitionDot name) transitions)),
      "}"
    ]

renderSomeTransitionDot :: String -> SomeTransition Env -> String
renderSomeTransitionDot name (SomeTransition t) = renderTransitionDot name t

renderTransitionDot :: String -> Transition Env from trigger to -> String
renderTransitionDot name (Transition from trigger to guard build) =
  dotNode name (stateName from)
    ++ " -> "
    ++ dotNode name (stateName to)
    ++ " [label="
    ++ jsString
      ( triggerName trigger
          ++ " ["
          ++ renderExprDot (stateNames from) guard
          ++ "] / "
          ++ buildName build
      )
    ++ "];"

renderExprDot :: Rec NameField vs -> Expr vs t -> String
renderExprDot names = \case
  EPrim (PInt n) -> show n
  EPrim (PBool b) -> if b then "true" else "false"
  EPrim (PString s) -> show s
  EVar i -> case indexRec i names of
    NameField n -> n
  EOp o x y ->
    parens $
      renderExprDot names x
        ++ " "
        ++ renderOp o
        ++ " "
        ++ renderExprDot names y
  ERecord {} -> "<record>"
  EAccess {} -> "<field>"
  EChoice {} -> "<choice>"
  ECase {} -> "<case>"
  ELambda {} -> "<function>"
  EApply {} -> "<apply>"

dotNode :: String -> String -> String
dotNode machine state = dotId machine ++ "_" ++ dotId state

dotId :: String -> String
dotId = map go
  where
    go c
      | c >= 'a' && c <= 'z' = c
      | c >= 'A' && c <= 'Z' = c
      | c >= '0' && c <= '9' = c
      | otherwise = '_'

renderExpr :: Rec NameField vs -> Expr vs t -> String
renderExpr names = \case
  EPrim (PInt n) -> show n
  EPrim (PBool b) -> if b then "true" else "false"
  EPrim (PString s) -> jsString s
  EVar i -> case indexRec i names of
    NameField n -> "scope." ++ n
  EOp o x y ->
    parens $
      renderExpr names x
        ++ " "
        ++ renderOp o
        ++ " "
        ++ renderExpr names y
  ERecord {} -> error "records are not part of this JavaScript demo backend"
  EAccess {} -> error "records are not part of this JavaScript demo backend"
  EChoice {} -> error "sums are not part of this JavaScript demo backend"
  ECase {} -> error "sums are not part of this JavaScript demo backend"
  ELambda {} -> error "functions are not part of this JavaScript demo backend"
  EApply {} -> error "functions are not part of this JavaScript demo backend"

renderOp :: Op a b c -> String
renderOp = \case
  OPlus -> "+"
  OTimes -> "*"
  OLte -> "<="
  OAnd -> "&&"

compileJs :: [(String, Machine Env)] -> String
compileJs xs =
  unlines
    [ "window.typedSmLcMachines = [",
      indent 2 (intercalate ",\n" (map (uncurry renderMachine) xs)),
      "];"
    ]

renderMachine :: String -> Machine Env -> String
renderMachine name (Machine transitions) =
  block
    [ "{",
      "  name: " ++ jsString name ++ ",",
      "  initialState: \"Idle\",",
      "  initialEnv: { rssi: 9, noise: 4, peakOffset: 2 },",
      "  initialPayload: {},",
      "  triggers: [\"Start\", \"Peak\", \"Drift\", \"Timeout\", \"Reset\"],",
      "  transitions: [",
      indent 4 (intercalate ",\n" (map renderSomeTransition transitions)),
      "  ]",
      "}"
    ]

renderSomeTransition :: SomeTransition Env -> String
renderSomeTransition (SomeTransition t) = renderTransition t

renderTransition :: Transition Env from trigger to -> String
renderTransition (Transition from trigger to guard build) =
  block
    [ "{",
      "  from: " ++ jsString (stateName from) ++ ",",
      "  trigger: " ++ jsString (triggerName trigger) ++ ",",
      "  to: " ++ jsString (stateName to) ++ ",",
      "  event: " ++ jsString (buildName build) ++ ",",
      "  guard: function (env, payload) {",
      "    var scope = Object.assign({}, env, payload);",
      "    return " ++ renderExpr (stateNames from) guard ++ ";",
      "  },",
      "  build: function (env, payload) {",
      "    var scope = Object.assign({}, env, payload);",
      "    return " ++ renderBuild (stateNames from) build ++ ";",
      "  }",
      "}"
    ]

renderBuild :: Rec NameField scope -> Build scope to -> String
renderBuild names (Build _ fields) =
  "{" ++ intercalate ", " (renderBuildFields names fields) ++ "}"

renderBuildFields :: Rec NameField scope -> Rec (BuildField scope) fields -> [String]
renderBuildFields _ RNil = []
renderBuildFields names (BuildField field expr :& rest) =
  (field ++ ": " ++ renderExpr names expr) : renderBuildFields names rest

indent :: Int -> String -> String
indent n = block . map (replicate n ' ' ++) . lines

block :: [String] -> String
block = intercalate "\n"

parens :: String -> String
parens x = "(" ++ x ++ ")"

jsString :: String -> String
jsString x = show x

main :: IO ()
main = do
  args <- getArgs
  putStr $
    case args of
      ["--dot"] -> compileDot machines
      _ -> compileJs machines
