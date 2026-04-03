-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2025.TurtleOpStack (turtleOpStack) where

import Alba.Dsl.V1.Bch2025.OpsUntyped
  ( op2Drop,
    op2Dup,
    op2Over,
    op2Rot,
    op2Swap,
    op3Dup,
    opDepth,
    opDrop,
    opDup,
    opIfDup,
    opNip,
    opOver,
    opPick,
    opRoll,
    opRot,
    opSwap,
    opTuck,
  )
import Alba.Dsl.V1.Common.StackUntyped (FnU)
import DslDemo.TurtleVm.Bch2025.TurtleVmUtilsUntyped
  ( condOp,
    condOpLeaf,
    inRange,
    is,
  )

turtleOpStack :: FnU
turtleOpStack =
  condOp
    [ ( inRange 0x6b 0x75,
        condOpLeaf
          [ -- 0x6b = OP_TOALTSTACK, not supported by turtleVm
            -- 0x6c = OP_FROMALTSTACK, not supported by turtleVm
            (is 0x6d, op2Drop),
            (is 0x6e, op2Dup),
            (is 0x6f, op3Dup),
            (is 0x70, op2Over),
            (is 0x71, op2Rot),
            (is 0x72, op2Swap),
            (is 0x73, opIfDup),
            (is 0x74, opDepth)
          ]
      ),
      ( inRange 0x75 0x7e,
        condOpLeaf
          [ (is 0x75, opDrop),
            (is 0x76, opDup),
            (is 0x77, opNip),
            (is 0x78, opOver),
            (is 0x79, opPick),
            (is 0x7a, opRoll),
            (is 0x7b, opRot),
            (is 0x7c, opSwap),
            (is 0x7d, opTuck)
          ]
      )
    ]
