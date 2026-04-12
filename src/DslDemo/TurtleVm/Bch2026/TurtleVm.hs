-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleVm
  ( turtleVm,
    turtleVmInit,
    turtleVmEval,
  )
where

import Alba.Dsl.V1.Bch2025.LangUntyped (int)
import Alba.Dsl.V1.Bch2025.OpsUntyped
  ( op0,
    opDrop,
    opIf,
    opSplit,
    opSwap,
    opUnless,
  )
import Alba.Dsl.V1.Bch2026 qualified as TY
import Alba.Dsl.V1.Bch2026.OpsUntyped (opInvoke, opUntil)
import Alba.Dsl.V1.Common.Lang (begin, (.))
import Alba.Dsl.V1.Common.StackUntyped (FnU, fromTyped)
import DslDemo.TurtleVm.Bch2025.TurtleVmUtils (toSigned)
import DslDemo.TurtleVm.Bch2025.TurtleVmUtilsUntyped (inRange)
import DslDemo.TurtleVm.Bch2026.TurtleVmCondStack (executeP)
import DslDemo.TurtleVm.Bch2026.TurtleVmDispatchTable (initOpDispatch)
import DslDemo.TurtleVm.Bch2026.TurtleVmState
  ( getOpAndCondStack,
    initState,
    isEndOfProgram,
  )
import DslDemo.TurtleVm.Bch2026.TurtleVmUtils
  ( fromSigned,
    isOpDataOp,
    isSingleByteOp,
  )
import DslDemo.TurtleVm.Bch2026.TurtleVmUtilsUntyped (condOp, is, unsupportedOp)
import Prelude (Int)

turtleVm :: Int -> FnU
turtleVm maxCsDepth = turtleVmInit maxCsDepth . turtleVmEval

turtleVmInit :: Int -> FnU
turtleVmInit maxCsDepth = ft (initOpDispatch maxCsDepth)

turtleVmEval :: FnU
turtleVmEval = ft initState . opUntil loop
  where
    loop :: FnU
    loop =
      begin
        . (ft getOpAndCondStack . ft executeP)
        . opIf handleOp opDrop
        . ft isEndOfProgram

ft :: TY.FnA s alt s' alt' -> FnU
ft = fromTyped

handleOp :: FnU
handleOp =
  begin
    . ft isSingleByteOp
    . opIf
      ( begin
          . ft toSigned
          . condOp
            [ (is 0x00, opDrop . op0),
              (inRange 0x4f 0xd4, ft fromSigned . opInvoke)
            ]
      )
      ( begin
          . (int 1 . opSplit . opSwap)
          . ft isOpDataOp
          . opUnless unsupportedOp
      )
