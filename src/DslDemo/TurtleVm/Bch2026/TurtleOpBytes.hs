-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleOpBytes (turtleOpBytes) where

import Alba.Dsl.V1.Bch2025.OpsUntyped
  ( opBin2Num,
    opCat,
    opNum2Bin,
    opSize,
    opSplit,
  )
import Alba.Dsl.V1.Common.StackUntyped (FNU)
import DslDemo.TurtleVm.Bch2026.TurtleVmUtilsUntyped (condOpLeaf, is)

turtleOpBytes :: FNU
turtleOpBytes =
  condOpLeaf
    [ (is 0x7e, opCat),
      (is 0x7f, opSplit),
      (is 0x80, opNum2Bin),
      (is 0x81, opBin2Num),
      (is 0x82, opSize)
    ]
