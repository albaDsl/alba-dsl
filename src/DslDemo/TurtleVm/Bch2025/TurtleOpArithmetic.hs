-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2025.TurtleOpArithmetic (turtleOpArithmetic) where

import Alba.Dsl.V1.Bch2025.OpsUntyped
  ( op0NotEqual,
    op1Add,
    op1Sub,
    opAbs,
    opAdd,
    opBoolAnd,
    opBoolOr,
    opDiv,
    opGreaterThan,
    opGreaterThanOrEqual,
    opLessThan,
    opLessThanOrEqual,
    opMax,
    opMin,
    opMod,
    opMul,
    opNegate,
    opNot,
    opNumEqual,
    opNumEqualVerify,
    opNumNotEqual,
    opSub,
    opWithin,
  )
import Alba.Dsl.V1.Common.StackUntyped (FNU)
import DslDemo.TurtleVm.Bch2025.TurtleVmUtilsUntyped
  ( condOp,
    condOpLeaf,
    inRange,
    is,
  )

turtleOpArithmetic :: FNU
turtleOpArithmetic =
  condOp
    [ ( inRange 0x8b 0x98,
        condOpLeaf
          [ (is 0x8b, op1Add),
            (is 0x8c, op1Sub),
            -- 0x8d: OP_2MUL, disabled op
            -- 0x8e: OP_2DIV, disabled op
            (is 0x8f, opNegate),
            (is 0x90, opAbs),
            (is 0x91, opNot),
            (is 0x92, op0NotEqual),
            (is 0x93, opAdd),
            (is 0x94, opSub),
            (is 0x95, opMul),
            (is 0x96, opDiv),
            (is 0x97, opMod)
          ]
      ),
      ( inRange 0x98 0xa6,
        condOpLeaf
          [ -- 0x98: OP_LSHIFT, disabled op
            -- 0x99: OP_RSHIFT, disabled op
            (is 0x9a, opBoolAnd),
            (is 0x9b, opBoolOr),
            (is 0x9c, opNumEqual),
            (is 0x9d, opNumEqualVerify),
            (is 0x9e, opNumNotEqual),
            (is 0x9f, opLessThan),
            (is 0xa0, opGreaterThan),
            (is 0xa1, opLessThanOrEqual),
            (is 0xa2, opGreaterThanOrEqual),
            (is 0xa3, opMin),
            (is 0xa4, opMax),
            (is 0xa5, opWithin)
          ]
      )
    ]
