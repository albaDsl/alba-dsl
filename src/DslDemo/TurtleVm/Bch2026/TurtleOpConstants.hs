-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleOpConstants (turtleOpConstants) where

import Alba.Dsl.V1.Bch2025.OpsUntyped
  ( op1,
    op10,
    op11,
    op12,
    op13,
    op14,
    op15,
    op16,
    op1Negate,
    op2,
    op3,
    op4,
    op5,
    op6,
    op7,
    op8,
    op9,
  )
import Alba.Dsl.V1.Common.StackUntyped (FNU)
import DslDemo.TurtleVm.Bch2026.TurtleVmUtilsUntyped (condOp, condOpLeaf, inRange, is)

turtleOpConstants :: FNU
turtleOpConstants =
  condOp
    [ ( inRange 0x4f 0x59,
        condOpLeaf
          [ (is 0x4F, op1Negate),
            -- 0x50: OP_RESERVED
            (is 0x51, op1),
            (is 0x52, op2),
            (is 0x53, op3),
            (is 0x54, op4),
            (is 0x55, op5),
            (is 0x56, op6),
            (is 0x57, op7),
            (is 0x58, op8)
          ]
      ),
      ( inRange 0x59 0x61,
        condOpLeaf
          [ (is 0x59, op9),
            (is 0x5A, op10),
            (is 0x5B, op11),
            (is 0x5C, op12),
            (is 0x5D, op13),
            (is 0x5E, op14),
            (is 0x5F, op15),
            (is 0x60, op16)
          ]
      )
    ]
