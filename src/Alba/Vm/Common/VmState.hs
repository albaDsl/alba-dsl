-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.VmState
  ( VmState (..),
    VmMetrics (..),
    VmParams (..),
    VmLogs,
    LogEntry (..),
    VerifyScriptResult (..),
    CodeL1,
    zeroedMetrics,
    maxedMetrics,
  )
where

import Alba.Vm.Common.OpcodeL1 (CodeL1)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.VmParams (VmParams (..))
import Alba.Vm.Common.VmStack (CondStack, VmStack)
import Data.Sequence qualified as S
import Data.Map qualified as M

data VmState = VmState
  { code :: !CodeL1,
    signedCode :: !CodeL1,
    s :: !VmStack,
    alt :: !VmStack,
    exec :: !CondStack,
    pushOnly :: !Bool,
    functions :: !(M.Map Int CodeL1),
    metrics :: !VmMetrics,
    limits :: !VmMetrics,
    params :: !VmParams,
    logData :: !(Maybe VmLogs)
  }
  deriving (Eq, Show)

type VmLogs = S.Seq LogEntry

data VmMetrics = VmMetrics
  { instructions :: !Int,
    pushedBytes :: !Int,
    arithmeticBytes :: !Int,
    hashIterations :: !Int,
    sigChecks :: !Int,
    cost :: !Int
  }
  deriving (Eq, Show)

data LogEntry = LogEntry
  { op :: Maybe OpcodeL2,
    exec :: Bool,
    stack :: !VmStack,
    altStack :: !VmStack,
    metrics :: !VmMetrics
  }
  deriving (Eq, Show)

data VerifyScriptResult = VerifyScriptResult
  { scriptSigResult :: Maybe VmState,
    scriptPubKeyResult :: Maybe VmState,
    scriptRedeemResult :: Maybe VmState
  }
  deriving (Eq, Show)

instance Ord VmMetrics where
  (<=) m1 m2 =
    m1.hashIterations <= m2.hashIterations
      && m1.cost <= m2.cost
      && m1.sigChecks <= m2.sigChecks

zeroedMetrics :: VmMetrics
zeroedMetrics =
  VmMetrics
    { instructions = 0,
      pushedBytes = 0,
      arithmeticBytes = 0,
      hashIterations = 0,
      sigChecks = 0,
      cost = 0
    }

maxedMetrics :: VmMetrics
maxedMetrics =
  VmMetrics
    { instructions = maxBound,
      pushedBytes = maxBound,
      arithmeticBytes = maxBound,
      hashIterations = maxBound,
      sigChecks = maxBound,
      cost = maxBound
    }
