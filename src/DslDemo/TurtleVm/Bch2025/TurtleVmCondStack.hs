-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2025.TurtleVmCondStack
  ( executeP,
    condStackExecuteP,
  )
where

import Alba.Dsl.V1.Bch2025
  ( Fn,
    N,
    TBool,
    TBytes,
    begin,
    bytes,
    del,
    name,
    name2,
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
    roll,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2025.LangUntyped (repeatProg)
import Alba.Dsl.V1.Bch2025.OpsUntyped qualified as UT
import Alba.Dsl.V1.Common.StackUntyped (fromTyped, toTyped)
import DslDemo.TurtleVm.Bch2025.Maybe (TMaybe, ifJust)
import DslDemo.TurtleVm.Bch2025.TurtleVmUtils (isConditionalOp, isSingleByteOp)
import Prelude hiding (and)

executeP ::
  Int ->
  Fn
    (s > N "op" (TMaybe TBytes) > N "condStack" TBytes)
    (s > TBytes > TBool)
executeP maxCsDepth =
  begin
    # (roll "op")
    # ifJust
      ( begin
          # name2 "op'" "singleByte" isSingleByteOp
          # name "exec?" (roll "condStack" # condStackExecuteP maxCsDepth)
          # roll "singleByte"
          # opIf
            (roll "op'" # opDup # isConditionalOp # (roll "exec?") # opBoolOr)
            (roll "op'" # roll "exec?")
      )
      (del "condStack" # bytes [] # opFalse)

condStackExecuteP :: Int -> Fn (s > TBytes) (s > TBool)
condStackExecuteP maxCsDepth =
  toTyped
    ( begin
        # repeatProg maxCsDepth (fromTyped check)
        # UT.opDrop
        # repeatProg (pred maxCsDepth) UT.opBoolAnd
    )
  where
    check :: Fn (s > TBytes) (s > TBool > TBytes)
    check =
      begin
        # (opSize # nat 1 # opGreaterThanOrEqual)
        # opIf
          (nat 1 # opSplit # opSwap # bytes [1] # opEqual # opSwap)
          (opTrue # opSwap)
