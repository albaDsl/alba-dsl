-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleVm (turtleVm) where

import Alba.Dsl.V1.Bch2025.OpsUntyped (op0, opDrop, opIf)
import Alba.Dsl.V1.Bch2026 qualified as TY
import Alba.Dsl.V1.Bch2026.OpsUntyped (opInvoke, opUntil)
import Alba.Dsl.V1.Common.Lang (begin, (#))
import Alba.Dsl.V1.Common.StackUntyped (FNU, fromTyped)
import DslDemo.TurtleVm.Bch2025.TurtleVmState (getOpBytes)
import DslDemo.TurtleVm.Bch2025.TurtleVmUtilsUntyped (inRange)
import DslDemo.TurtleVm.Bch2026.TurtleVmCondStack (executeP)
import DslDemo.TurtleVm.Bch2026.TurtleVmDispatchTable (initOpDispatch)
import DslDemo.TurtleVm.Bch2026.TurtleVmState
  ( getOpAndCondStack,
    initState,
    isEndOfProgram,
  )
import DslDemo.TurtleVm.Bch2026.TurtleVmUtilsUntyped (condOp, is)

turtleVm :: Int -> FNU
turtleVm maxCsDepth =
  begin
    # ft (initOpDispatch maxCsDepth)
    # ft initState
    # opUntil loop
  where
    loop :: FNU
    loop =
      begin
        # (ft getOpAndCondStack # ft executeP)
        # opIf handleOp opDrop
        # ft isEndOfProgram

ft :: TY.FNA s alt s' alt' -> FNU
ft = fromTyped

handleOp :: FNU
handleOp =
  condOp
    [ (is 0x00, opDrop # op0),
      (inRange 0x01 0x4c, ft getOpBytes),
      (inRange 0x4f 0xd4, opInvoke)
    ]
