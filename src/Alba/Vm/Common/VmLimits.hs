-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.VmLimits
  ( addCost,
    addOperationCost,
    addBytesPushed,
    addArithmeticBytes,
    addHashIterations,
    addSigCheck,
    setLimits,
    verifyCondStack,
    verifyMetrics,
    dumpMetrics,
    dumpLimits,
  )
where

import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.VmParams (SigLimit (..), VmParams (..))
import Alba.Vm.Common.VmState (CodeL1, VmMetrics (..), VmState (..))
import Data.ByteString qualified as B
import Data.Sequence qualified as S
import Text.Printf (printf)

addCost :: Int -> VmState -> VmState
addCost amount st@VmState {metrics = m@VmMetrics {cost}} =
  st {metrics = m {cost = cost + amount}}

addOperationCost :: VmState -> VmState
addOperationCost st@VmState {metrics = m} =
  st
    { metrics =
        m
          { instructions = succ m.instructions,
            cost = m.cost + st.params.opCodeCost
          }
    }

addBytesPushed :: Int -> VmState -> VmState
addBytesPushed numBytes st@VmState {metrics = m} =
  st
    { metrics =
        m
          { pushedBytes = m.pushedBytes + numBytes,
            cost = m.cost + numBytes
          }
    }

addArithmeticBytes :: Int -> VmState -> VmState
addArithmeticBytes numBytes st@VmState {metrics = m} =
  st
    { metrics =
        m
          { arithmeticBytes = m.arithmeticBytes + numBytes,
            cost = m.cost + numBytes
          }
    }

addHashIterations :: Int -> Bool -> VmState -> VmState
addHashIterations imgSize isTwoRounds st@VmState {metrics = m} =
  st
    { metrics =
        m
          { hashIterations = m.hashIterations + its,
            cost = m.cost + its * st.params.hashDigestIterationCost
          }
    }
  where
    its =
      if not isTwoRounds
        then 1 + ((imgSize + 8) `div` hashBlockSize)
        else 2 + ((imgSize + 8) `div` hashBlockSize)
    hashBlockSize = 64

addSigCheck :: Int -> VmState -> VmState
addSigCheck count st@VmState {metrics = m} =
  st
    { metrics =
        m
          { sigChecks = m.sigChecks + count,
            cost = m.cost + count * st.params.sigCheckCost
          }
    }

-- See the Ord instance for VmMetrics for what fields are used when enforcing
-- the limits.
setLimits :: CodeL1 -> VmState -> VmState
setLimits code st =
  st
    { limits =
        VmMetrics
          { instructions = 0,
            pushedBytes = 0,
            arithmeticBytes = 0,
            hashIterations = hashLimit,
            sigChecks = sigLimit,
            cost = costLimit
          }
    }
  where
    costLimit =
      (st.params.fixedCredit + B.length code)
        * st.params.costBudgetPerInputByte

    hashLimit =
      ( (st.params.fixedCredit + B.length code)
          * st.params.hashItersLimitNumerator
      )
        `div` st.params.hashItersLimitDenominator

    sigLimit = case st.params.sigLimit of
      DclBased {..} -> (fixedCredit + B.length code) `div` denominator
      MaxLimit {..} -> limit

verifyCondStack :: VmState -> Either (ScriptError, VmState) ()
verifyCondStack st@VmState {exec}
  | S.length exec <= st.params.maxExecStackSize = Right ()
verifyCondStack st = Left (SeOpVmLimit, st)

verifyMetrics :: VmState -> Either (ScriptError, VmState) ()
verifyMetrics VmState {metrics, limits} | metrics <= limits = Right ()
verifyMetrics st = Left (SeOpVmLimit, st)

dumpMetrics :: VmState -> IO ()
dumpMetrics VmState {..} = print metrics

dumpLimits :: VmState -> IO ()
dumpLimits VmState {limits} =
  printf
    "Cost limit = %d. Hash iters limit = %d. Sig limit = %d.\n"
    limits.cost
    limits.hashIterations
    limits.sigChecks
