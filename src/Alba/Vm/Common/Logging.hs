-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.Logging
  ( LogDisplayOpts (..),
    FunctionTable,
    FunctionTableEntry (..),
    defaultDisplayOpts,
    logOp,
    logStart,
    logFunctionExit,
    logFailure,
  )
where

import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.StackElement (Labels)
import Alba.Vm.Common.VmState
  ( LogEntry (..),
    Operation (..),
    VmState (..),
  )
import Data.Map qualified as M
import Data.Sequence qualified as S
import Data.Text (Text)
import Prelude hiding (log)

data LogDisplayOpts = LogDisplayOpts
  { labels :: Maybe Labels,
    functionTable :: Maybe FunctionTable,
    showMetrics :: Bool,
    showUnexecuted :: Bool
  }

type FunctionTable = M.Map Int FunctionTableEntry

data FunctionTableEntry = FunctionTableEntry
  { functionName :: Text,
    functionLongName :: Text,
    slot :: Int,
    callSites :: Maybe Int
  }
  deriving (Show)

defaultDisplayOpts :: LogDisplayOpts
defaultDisplayOpts =
  LogDisplayOpts
    { labels = Nothing,
      functionTable = Nothing,
      showMetrics = False,
      showUnexecuted = False
    }

logStart :: VmState -> VmState
logStart state@VmState {logData = Nothing} = state
logStart state@VmState {s, alt, metrics, logData = Just logData} =
  let entry = Completed {op = Start, exec = True, stack = s, altStack = alt, ..}
   in state {logData = Just $ logData S.|> entry}

logOp :: OpcodeL2 -> Bool -> VmState -> VmState
logOp _op _exec state@VmState {logData = Nothing} = state
logOp op exec state@VmState {s, alt, metrics, logData = Just logData} =
  let entry = Completed {op = Op op, stack = s, altStack = alt, ..}
   in state {logData = Just $ logData S.|> entry}

logFunctionExit :: VmState -> VmState
logFunctionExit state@VmState {logData = Nothing} = state
logFunctionExit state@VmState {s, alt, metrics, logData = Just logData} =
  let entry =
        Completed
          { op = FunctionExit,
            exec = True,
            stack = s,
            altStack = alt,
            ..
          }
   in state {logData = Just $ logData S.|> entry}

logFailure :: OpcodeL2 -> VmState -> VmState
logFailure _op state@VmState {logData = Nothing} = state
logFailure op state@VmState {logData = Just logData} =
  let entry = Failed {opcode = op}
   in state {logData = Just $ logData S.|> entry}
