-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleVmState
  ( initState,
    getOp,
    getOpAndCondStack,
    getOpBytes,
    getCondStack,
    putCondStack,
    toggleCondStack,
    dropCondStack,
    isEndOfProgram,
  )
where

import Alba.Dsl.V1.Bch2026
  ( FN,
    FNA,
    StackEntry,
    TBool,
    TBytes,
    TNat,
    begin,
    bytes,
    cast,
    ifZero,
    nat,
    op2Drop,
    opCat,
    opDrop,
    opDup,
    opEqual,
    opFromAltStack,
    opGreaterThanOrEqual,
    opIf,
    opLessThanOrEqual,
    opNip,
    opNumEqual,
    opRot,
    opSize,
    opSplit,
    opSwap,
    opToAltStack,
    opTuck,
    (#),
    type (>),
  )
import DslDemo.TurtleVm.Bch2026.TurtleVmUtils (vmError)
import DslDemo.TurtleVm.Common.Maybe (TMaybe, just, nothing)
import DslDemo.TurtleVm.Common.Tuple (TTuple, tuple, untuple)

data TurtleVmState

instance StackEntry TurtleVmState

initState :: FNA (s > TBytes) alt s (alt > TurtleVmState)
initState = bytes [] # tuple # putState

getOp ::
  FNA s (alt > TurtleVmState) (s > TMaybe TBytes) (alt > TurtleVmState)
getOp =
  begin
    # (getState # untuple # opSwap # opSize)
    # ifZero
      (opSwap # tuple # putState # nothing)
      (nat 1 # opSplit # opRot # tuple # putState # just)

getOpBytes ::
  FNA (s > TNat) (alt > TurtleVmState) (s > TBytes) (alt > TurtleVmState)
getOpBytes =
  getState # untuple # opSwap # opRot # opSplit # opRot # tuple # putState

getOpAndCondStack ::
  FNA s (alt > TurtleVmState) (s > TMaybe TBytes > TBytes) (alt > TurtleVmState)
getOpAndCondStack =
  begin
    # (getState # untuple # opSwap # opSize)
    # ifZero
      (opSwap # opTuck # tuple # putState # nothing # opSwap)
      ( begin
          # (nat 1 # opSplit # opRot # opTuck # tuple # putState)
          # (opSwap # just # opSwap)
      )

getCondStack :: FNA s (alt > TurtleVmState) (s > TBytes) (alt > TurtleVmState)
getCondStack = getState # opDup # untuple # opNip # opSwap # putState

putCondStack ::
  Int -> FNA (s > TBool) (alt > TurtleVmState) s (alt > TurtleVmState)
putCondStack maxCsDepth =
  begin
    # (getState # untuple # opSize # maxCsDepth' # opLessThanOrEqual)
    # opIf
      ( begin
          # (opRot # opIf (bytes [1]) (bytes [0]))
          # (opSwap # opCat # tuple # putState)
      )
      (op2Drop # opDrop # vmError "E2") -- CondStack overflow
  where
    maxCsDepth' = nat (fromIntegral maxCsDepth)

toggleCondStack :: FNA s (alt > TurtleVmState) s (alt > TurtleVmState)
toggleCondStack =
  begin
    # (getState # untuple # opSize # nat 1 # opGreaterThanOrEqual)
    # opIf
      ( begin
          # (nat 1 # opSplit # opSwap # bytes [1] # opEqual)
          # opIf (bytes [0]) (bytes [1])
          # (opSwap # opCat)
          # (tuple # putState)
      )
      (vmError "E3") -- CondStack underflow

dropCondStack :: FNA s (alt > TurtleVmState) s (alt > TurtleVmState)
dropCondStack =
  begin
    # (getState # untuple # opSize # nat 1 # opGreaterThanOrEqual)
    # opIf
      (nat 1 # opSplit # opNip # tuple # putState)
      (vmError "E4") -- CondStack underflow

getState :: FNA s (alt > TurtleVmState) (s > TTuple) alt
getState = opFromAltStack # toTuple
  where
    toTuple :: FN (s > TurtleVmState) (s > TTuple)
    toTuple = cast

putState :: FNA (s > TTuple) alt s (alt > TurtleVmState)
putState = fromTuple # opToAltStack
  where
    fromTuple :: FN (s > TTuple) (s > TurtleVmState)
    fromTuple = cast

isEndOfProgram :: FNA s (alt > TurtleVmState) (s > TBool) (alt > TurtleVmState)
isEndOfProgram =
  begin
    # (getState # opDup # putState)
    # (untuple # opDrop # opSize # opNip # nat 0 # opNumEqual)
