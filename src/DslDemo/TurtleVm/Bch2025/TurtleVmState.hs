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

import Alba.Dsl.V1.Bch2026
  ( Fn,
    FnA,
    Stack (..),
    StackEntry,
    TBool,
    TBytes,
    TNat,
    begin,
    bytes,
    cast,
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
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude (ifZero)
import DslDemo.TurtleVm.Bch2025.Maybe (TMaybe, just, nothing)
import DslDemo.TurtleVm.Bch2025.Tuple (TTuple, tuple, untuple)
import DslDemo.TurtleVm.Bch2025.TurtleVmUtils (isOpDataOp, vmError)
import Prelude (Int, fromIntegral)

data TurtleVmState

instance StackEntry TurtleVmState

initState :: FnA (s :> TBytes) alt s (alt :> TurtleVmState)
initState = bytes [] . tuple . putState

getOp ::
  FnA s (alt :> TurtleVmState) (s :> TMaybe TBytes) (alt :> TurtleVmState)
getOp =
  begin
    . (getState . untuple . opSwap . opSize)
    . ifZero
      (opSwap . tuple . putState . nothing)
      (getOpBytes . opRot . tuple . putState . just)

getOpBytes :: Fn (s :> TBytes) (s :> TBytes :> TBytes)
getOpBytes =
  begin
    . name2 #op #rest (nat 1 . opSplit)
    . (pick #op . isOpDataOp)
    . opIf
      ( begin
          . (roll #rest . pick #op . bytesToNat . opSplit) -- oprest rest
          . (roll #op . opRot . opCat . opSwap)
      )
      (roll #op . roll #rest)
  where
    bytesToNat :: Fn (s :> TBytes) (s :> TNat)
    bytesToNat = cast

getOpAndCondStack ::
  FnA
    s
    (alt :> TurtleVmState)
    (s :> TMaybe TBytes :> TBytes)
    (alt :> TurtleVmState)
getOpAndCondStack =
  begin
    . (getState . untuple . opSwap . opSize)
    . ifZero
      (opSwap . opTuck . tuple . putState . nothing . opSwap)
      ( begin
          . (getOpBytes . opRot . opTuck . tuple . putState)
          . (opSwap . just . opSwap)
      )

getCondStack ::
  FnA s (alt :> TurtleVmState) (s :> TBytes) (alt :> TurtleVmState)
getCondStack = getState . opDup . untuple . opNip . opSwap . putState

putCondStack ::
  Int -> FnA (s :> TBool) (alt :> TurtleVmState) s (alt :> TurtleVmState)
putCondStack maxCsDepth =
  begin
    . (getState . untuple . opSize . maxCsDepth' . opLessThanOrEqual)
    . opIf
      ( begin
          . (opRot . opIf (bytes [1]) (bytes [0]))
          . (opSwap . opCat . tuple . putState)
      )
      (op2Drop . opDrop . vmError "E2") -- CondStack overflow
  where
    maxCsDepth' = nat (fromIntegral maxCsDepth)

toggleCondStack :: FnA s (alt :> TurtleVmState) s (alt :> TurtleVmState)
toggleCondStack =
  begin
    . (getState . untuple . opSize . nat 1 . opGreaterThanOrEqual)
    . opIf
      ( begin
          . (nat 1 . opSplit . opSwap . bytes [1] . opEqual)
          . opIf (bytes [0]) (bytes [1])
          . (opSwap . opCat)
          . (tuple . putState)
      )
      (vmError "E3") -- CondStack underflow

dropCondStack :: FnA s (alt :> TurtleVmState) s (alt :> TurtleVmState)
dropCondStack =
  begin
    . (getState . untuple . opSize . nat 1 . opGreaterThanOrEqual)
    . opIf
      (nat 1 . opSplit . opNip . tuple . putState)
      (vmError "E4") -- CondStack underflow

getState :: FnA s (alt :> TurtleVmState) (s :> TTuple) alt
getState = opFromAltStack . toTuple
  where
    toTuple :: Fn (s :> TurtleVmState) (s :> TTuple)
    toTuple = cast

putState :: FnA (s :> TTuple) alt s (alt :> TurtleVmState)
putState = fromTuple . opToAltStack
  where
    fromTuple :: Fn (s :> TTuple) (s :> TurtleVmState)
    fromTuple = cast
