-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleOpIf (ifOp) where

import Alba.Dsl.V1.Bch2025.OpsUntyped (opFalse, opIf)
import Alba.Dsl.V1.Common.Lang (begin, (#))
import Alba.Dsl.V1.Common.StackUntyped (FNU, fromTyped)
import DslDemo.TurtleVm.Bch2026.TurtleVmCondStack (condStackExecuteP)
import DslDemo.TurtleVm.Bch2026.TurtleVmState (getCondStack, putCondStack)

ifOp :: Int -> FNU -> FNU
ifOp maxCsDepth op =
  begin
    # ft getCondStack
    # ft condStackExecuteP
    # opIf
      (op # ft (putCondStack maxCsDepth))
      (opFalse # ft (putCondStack maxCsDepth))
  where
    ft = fromTyped
