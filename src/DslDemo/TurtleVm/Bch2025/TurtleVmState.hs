-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2025.TurtleVmState
  ( initState,
    getOp,
    getOpAndCondStack,
    getCondStack,
    putCondStack,
    toggleCondStack,
    dropCondStack,
  )
where

import Alba.Dsl.V1.Bch2025
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
    name2,
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
    opRot,
    opSize,
    opSplit,
    opSwap,
    opToAltStack,
    opTuck,
    pick,
    roll,
    (#),
    type (>),
  )
import DslDemo.TurtleVm.Bch2025.TurtleVmUtils (isOpDataOp, vmError)
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
      (getOpBytes # opRot # tuple # putState # just)

getOpBytes :: FN (s > TBytes) (s > TBytes > TBytes)
getOpBytes =
  begin
    # name2 @"op" @"rest" (nat 1 # opSplit)
    # (pick @"op" # isOpDataOp)
    # opIf
      ( begin
          # (roll @"rest" # pick @"op" # bytesToNat # opSplit) -- oprest rest
          # (roll @"op" # opRot # opCat # opSwap)
      )
      (roll @"op" # roll @"rest")
  where
    bytesToNat :: FN (s > TBytes) (s > TNat)
    bytesToNat = cast

getOpAndCondStack ::
  FNA s (alt > TurtleVmState) (s > TMaybe TBytes > TBytes) (alt > TurtleVmState)
getOpAndCondStack =
  begin
    # (getState # untuple # opSwap # opSize)
    # ifZero
      (opSwap # opTuck # tuple # putState # nothing # opSwap)
      ( begin
          # (getOpBytes # opRot # opTuck # tuple # putState)
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
