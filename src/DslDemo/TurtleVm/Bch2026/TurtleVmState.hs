-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleVmState
  ( initState,
    getOp,
    getOpAndCondStack,
    getCondStack,
    putCondStack,
    toggleCondStack,
    dropCondStack,
    getState,
    putState,
    isEndOfProgram,
  )
where

import Alba.Dsl.V1.Bch2026
  ( FN,
    FNA,
    TBool,
    TBytes,
    TNat,
    begin,
    bytes,
    cast,
    function,
    ifZero,
    name2,
    nat,
    op2Drop,
    opCat,
    opEqual,
    opFromAltStack,
    opGreaterThanOrEqual,
    opIf,
    opLessThanOrEqual,
    opNumEqual,
    opSize,
    opSplit,
    opToAltStack,
    pick,
    roll,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Maybe (TMaybe, just, nothing)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, dup, nip, rot, swap, tuck)
import Alba.Dsl.V1.Bch2026.Contract.Tuple (TTuple, tuple, untuple)
import DslDemo.TurtleVm.Bch2026.TurtleVmUtils (isOpDataOp, vmError)
import Prelude hiding (drop)

type TurtleVmState = TTuple TBytes TBytes

initState :: FNA (s > TBytes) alt s (alt > TurtleVmState)
initState = bytes [] # tuple # putState

getOp ::
  FNA s (alt > TurtleVmState) (s > TMaybe TBytes) (alt > TurtleVmState)
getOp =
  begin
    # (getState # untuple # swap # opSize)
    # ifZero
      (swap # tuple # putState # nothing)
      (getOpBytes # rot # tuple # putState # just)

getOpBytes :: FN (s > TBytes) (s > TBytes > TBytes)
getOpBytes =
  begin
    # name2 "op" "rest" (nat 1 # opSplit)
    # (pick "op" # isOpDataOp)
    # opIf
      ( begin
          # (roll "rest" # pick "op" # bytesToNat # opSplit) -- oprest rest
          # (roll "op" # rot # opCat # swap)
      )
      (roll "op" # roll "rest")
  where
    bytesToNat :: FN (s > TBytes) (s > TNat)
    bytesToNat = cast

getOpAndCondStack ::
  FNA s (alt > TurtleVmState) (s > TMaybe TBytes > TBytes) (alt > TurtleVmState)
getOpAndCondStack =
  begin
    # (getState # untuple # swap # opSize)
    # ifZero
      (swap # tuck # tuple # putState # nothing # swap)
      ( begin
          # (getOpBytes # rot # tuck # tuple # putState)
          # (swap # just # swap)
      )

getCondStack :: FNA s (alt > TurtleVmState) (s > TBytes) (alt > TurtleVmState)
getCondStack = getState # dup # untuple # nip # swap # putState

putCondStack ::
  Int -> FNA (s > TBool) (alt > TurtleVmState) s (alt > TurtleVmState)
putCondStack maxCsDepth =
  function
    ( begin
        # (getStateUnpackedWithSize # maxCsDepth' # opLessThanOrEqual)
        # opIf
          ( begin
              # (rot # opIf (bytes [1]) (bytes [0]))
              # (swap # opCat # tuple # putState)
          )
          (op2Drop # drop # vmError "E2") -- CondStack overflow
    )
  where
    maxCsDepth' = nat (fromIntegral maxCsDepth)

getStateUnpackedWithSize ::
  FNA s (alt > TurtleVmState) (s > TBytes > TBytes > TNat) alt
getStateUnpackedWithSize = function (getState # untuple # opSize)

toggleCondStack :: FNA s (alt > TurtleVmState) s (alt > TurtleVmState)
toggleCondStack =
  begin
    # (getStateUnpackedWithSize # nat 1 # opGreaterThanOrEqual)
    # opIf
      ( begin
          # (nat 1 # opSplit # swap # bytes [1] # opEqual)
          # opIf (bytes [0]) (bytes [1])
          # (swap # opCat # tuple # putState)
      )
      (vmError "E3") -- CondStack underflow

dropCondStack :: FNA s (alt > TurtleVmState) s (alt > TurtleVmState)
dropCondStack =
  begin
    # (getStateUnpackedWithSize # nat 1 # opGreaterThanOrEqual)
    # opIf
      (nat 1 # opSplit # nip # tuple # putState)
      (vmError "E4") -- CondStack underflow

getState :: FNA s (alt > TurtleVmState) (s > TurtleVmState) alt
getState = opFromAltStack

putState :: FNA (s > TurtleVmState) alt s (alt > TurtleVmState)
putState = opToAltStack

isEndOfProgram :: FNA s (alt > TurtleVmState) (s > TBool) (alt > TurtleVmState)
isEndOfProgram =
  begin
    # (getState # dup # putState)
    # (untuple # drop # opSize # nip # nat 0 # opNumEqual)
