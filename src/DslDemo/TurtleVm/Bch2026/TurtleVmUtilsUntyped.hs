-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleVmUtilsUntyped
  ( condOp,
    inRange,
    is,
    unsupportedOp,
  )
where

import Alba.Dsl.V1.Bch2026.LangUntyped (int)
import Alba.Dsl.V1.Bch2026.OpsUntyped (opDup, opIf, opNumEqual)
import Alba.Dsl.V1.Common.StackUntyped (FnU, SU, fromTyped, (.))
import DslDemo.TurtleVm.Bch2026.TurtleVmUtils qualified as UT
import Prelude (Integer)

condOp :: [(SU -> SU, SU -> SU)] -> FnU
condOp [] st = unsupportedOp st
condOp ((test, result) : rest) st =
  (opDup . test . opIf result (condOp rest)) st

unsupportedOp :: FnU
unsupportedOp = fromTyped UT.unsupportedOp

inRange :: Integer -> Integer -> FnU
inRange x y = fromTyped (UT.inRange x y)

is :: Integer -> FnU
is x = int x . opNumEqual
