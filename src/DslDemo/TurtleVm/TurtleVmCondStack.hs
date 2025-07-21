-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.TurtleVmCondStack (executeP, condStackExecuteP) where

import Alba.Dsl.V1.Bch2025
  ( FN,
    N,
    TBool,
    TBytes,
    TInt,
    begin,
    bytes,
    drop,
    int,
    nat,
    opBoolOr,
    opDup,
    opEqual,
    opFalse,
    opGreaterThanOrEqual,
    opIf,
    opSize,
    opSplit,
    opSwap,
    opTrue,
    opWithin,
    roll,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2025.LangUntyped (repeatProg)
import Alba.Dsl.V1.Bch2025.OpsUntyped qualified as UT
import Alba.Dsl.V1.Common.StackUntyped (fromTyped, toTyped)
import DslDemo.TurtleVm.Maybe (TMaybe, ifJust)
import DslDemo.TurtleVm.TurtleVmUtils (toSigned)
import Prelude hiding (and, drop)

executeP ::
  Int ->
  FN
    (s > N "op" (TMaybe TBytes) > N "condStack" TBytes)
    (s > TInt > TBool)
executeP maxCsDepth =
  begin
    # (roll @"op")
    # ifJust
      ( begin
          # toSigned
          # (opDup # isConditionalOp)
          # (roll @"condStack" # condStackExecuteP maxCsDepth)
          # opBoolOr
      )
      (drop @"condStack" # int 0 # opFalse)

isConditionalOp :: FN (s > TInt) (s > TBool)
isConditionalOp = int 0x63 # int 0x69 # opWithin

condStackExecuteP :: Int -> FN (s > TBytes) (s > TBool)
condStackExecuteP maxCsDepth =
  toTyped
    ( begin
        # repeatProg maxCsDepth (fromTyped check)
        # UT.opDrop
        # repeatProg (pred maxCsDepth) UT.opBoolAnd
    )
  where
    check :: FN (s > TBytes) (s > TBool > TBytes)
    check =
      begin
        # (opSize # nat 1 # opGreaterThanOrEqual)
        # opIf
          (nat 1 # opSplit # opSwap # bytes [1] # opEqual # opSwap)
          (opTrue # opSwap)
