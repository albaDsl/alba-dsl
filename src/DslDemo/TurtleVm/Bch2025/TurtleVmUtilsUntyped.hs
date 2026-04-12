-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2025.TurtleVmUtilsUntyped
  ( condOp,
    condOpLeaf,
    inRange,
    is,
    unsupportedOp,
  )
where

import Alba.Dsl.V1.Bch2025.LangUntyped (int)
import Alba.Dsl.V1.Bch2025.OpsUntyped (opDrop, opDup, opIf, opNumEqual)
import Alba.Dsl.V1.Common.StackUntyped (FnU, SU, fromTyped, (.))
import DslDemo.TurtleVm.Bch2025.TurtleVmUtils qualified as UT
import Prelude (Integer)

condOp :: [(SU -> SU, SU -> SU)] -> FnU
condOp [] st = unsupportedOp st
condOp ((test, result) : rest) st =
  (opDup . test . opIf result (condOp rest)) st

condOpLeaf :: [(SU -> SU, SU -> SU)] -> FnU
condOpLeaf [] st = unsupportedOp st
condOpLeaf ((test, result) : rest) st =
  (opDup . test . opIf (opDrop . result) (condOpLeaf rest)) st

unsupportedOp :: FnU
unsupportedOp = fromTyped UT.unsupportedOp

inRange :: Integer -> Integer -> FnU
inRange x y = fromTyped (UT.inRange x y)

is :: Integer -> FnU
is x = int x . opNumEqual
