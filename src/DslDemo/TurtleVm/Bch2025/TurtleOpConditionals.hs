-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2025.TurtleOpConditionals
  ( turtleOpConditionals,
  )
where

import Alba.Dsl.V1.Bch2025.OpsUntyped
  ( opFalse,
    opIf,
    opNop,
    opNot,
    opReturn,
    opVerify,
  )
import Alba.Dsl.V1.Common.Lang (begin, (#))
import Alba.Dsl.V1.Common.StackUntyped (FnU, fromTyped)
import DslDemo.TurtleVm.Bch2025.TurtleVmCondStack (condStackExecuteP)
import DslDemo.TurtleVm.Bch2025.TurtleVmState
  ( dropCondStack,
    getCondStack,
    putCondStack,
    toggleCondStack,
  )
import DslDemo.TurtleVm.Bch2025.TurtleVmUtilsUntyped (condOpLeaf, is)

turtleOpConditionals :: Int -> FnU
turtleOpConditionals maxCsDepth =
  condOpLeaf
    [ (is 0x61, opNop),
      -- 0x62: OP_VER_OP_EVAL, disabled op
      (is 0x63, ifOp id),
      (is 0x64, ifOp opNot),
      -- 0x65: OP_VERIF_OP_BEGIN, disabled op
      -- 0x66: OP_VERNOTIF_OP_UNTIL, disabled op
      (is 0x67, ft toggleCondStack),
      (is 0x68, ft dropCondStack),
      (is 0x69, opVerify),
      (is 0x6a, opReturn)
    ]
  where
    ft = fromTyped

    ifOp :: FnU -> FnU
    ifOp op =
      begin
        # ft getCondStack
        # ft (condStackExecuteP maxCsDepth)
        # opIf
          (op # ft (putCondStack maxCsDepth))
          (opFalse # ft (putCondStack maxCsDepth))
