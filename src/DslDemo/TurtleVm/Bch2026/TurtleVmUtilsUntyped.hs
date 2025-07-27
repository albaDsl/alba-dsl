-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleVmUtilsUntyped
  ( condOp,
    inRange,
    is,
    unsupportedOp,
  )
where

import Alba.Dsl.V1.Bch2025.LangUntyped (int)
import Alba.Dsl.V1.Bch2025.OpsUntyped (opDup, opIf, opNumEqual)
import Alba.Dsl.V1.Common.Lang ((#))
import Alba.Dsl.V1.Common.StackUntyped (FNU, SU, fromTyped)
import DslDemo.TurtleVm.Bch2026.TurtleVmUtils qualified as UT

condOp :: [(SU -> SU, SU -> SU)] -> FNU
condOp [] st = unsupportedOp st
condOp ((test, result) : rest) st =
  (opDup # test # opIf result (condOp rest)) st

unsupportedOp :: FNU
unsupportedOp = fromTyped UT.unsupportedOp

inRange :: Integer -> Integer -> FNU
inRange x y = fromTyped (UT.inRange x y)

is :: Integer -> FNU
is x = int x # opNumEqual
