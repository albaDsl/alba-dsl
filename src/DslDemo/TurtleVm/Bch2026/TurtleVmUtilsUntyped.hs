-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleVmUtilsUntyped
  ( condOp,
    condOpLeaf,
    inRange,
    is,
  )
where

import Alba.Dsl.V1.Bch2025.LangUntyped (int)
import Alba.Dsl.V1.Bch2025.OpsUntyped
import Alba.Dsl.V1.Common.Lang ((#))
import Alba.Dsl.V1.Common.StackUntyped (FNU, SU, fromTyped)
import DslDemo.TurtleVm.Bch2026.TurtleVmUtils (vmError)

condOp :: [(SU -> SU, SU -> SU)] -> FNU
condOp [] st = unsupportedOp st
condOp ((test, result) : rest) st =
  (opDup # test # opIf result (condOp rest)) st

condOpLeaf :: [(SU -> SU, SU -> SU)] -> FNU
condOpLeaf [] st = unsupportedOp st
condOpLeaf ((test, result) : rest) st =
  (opDup # test # opIf (opDrop # result) (condOpLeaf rest)) st

unsupportedOp :: FNU
unsupportedOp = fromTyped (vmError "E1")

inRange :: Integer -> Integer -> FNU
inRange x y = int x # int y # opWithin

is :: Integer -> FNU
is x = int x # opNumEqual
