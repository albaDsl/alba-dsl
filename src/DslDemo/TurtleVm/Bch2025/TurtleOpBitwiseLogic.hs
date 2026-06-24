-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2025.TurtleOpBitwiseLogic
  ( turtleOpBitwiseLogic,
  )
where

import Alba.Dsl.V1.Bch2026.OpsUntyped
  ( opAnd,
    opEqual,
    opEqualVerify,
    opOr,
    opXor,
  )
import Alba.Dsl.V1.Common.StackUntyped (FnU)
import DslDemo.TurtleVm.Bch2025.TurtleVmUtilsUntyped (condOpLeaf, is)

turtleOpBitwiseLogic :: FnU
turtleOpBitwiseLogic =
  condOpLeaf
    [ -- 0x83: OP_INVERT, disabled op
      (is 0x84, opAnd),
      (is 0x85, opOr),
      (is 0x86, opXor),
      (is 0x87, opEqual),
      (is 0x88, opEqualVerify)
    ]
