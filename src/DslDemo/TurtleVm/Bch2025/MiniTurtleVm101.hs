-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2025.MiniTurtleVm101
  ( miniTurtleVm101,
    turtleOpDefine,
    turtleOpInvoke,
  )
where

import Alba.Dsl.V1.Bch2025 (Fn, FnA, Stack (..), begin, (.))
import Alba.Dsl.V1.Bch2025 qualified as TY
import Alba.Dsl.V1.Bch2025.LangUntyped (repeatProg)
import Alba.Dsl.V1.Bch2025.OpsUntyped (op1, op1Add, opDrop, opIf, opMul)
import Alba.Dsl.V1.Common.CompilerUtils (aop)
import Alba.Dsl.V1.Common.StackUntyped (FnU, fromTyped, (∘))
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (OP_DEFINE, OP_INVOKE))
import DslDemo.TurtleVm.Bch2025.Maybe (ifJust)
import DslDemo.TurtleVm.Bch2025.TurtleVmStateSimple
  ( getOp,
    getOpBytes,
    initStateWithDefaultOpDefine,
    invokeFunction,
    putFunction,
  )
import DslDemo.TurtleVm.Bch2025.TurtleVmUtilsUntyped (condOpLeaf, is)
import Prelude ()

miniTurtleVm101 :: FnU
miniTurtleVm101 = ft initStateWithDefaultOpDefine ∘ repeatProg 12 handleOp

handleOp :: FnU
handleOp =
  begin
    ∘ ft getOp
    ∘ ft (ifJust (toSigned . TY.opTrue) (TY.op0 . TY.opFalse))
    ∘ opIf handleOp' opDrop
  where
    toSigned = TY.bytes [0] . TY.opCat . TY.opBin2Num

handleOp' :: FnU
handleOp' =
  condOpLeaf
    [ (is 0x02, ft (getOpBytes 2)), -- OP_DATA_02
      (is 0x51, op1),
      (is 0x8b, op1Add),
      (is 0x89, ft putFunction), -- OP_DEFINE
      (is 0x8a, ft invokeFunction), -- OP_INVOKE
      (is 0x95, opMul)
    ]

ft :: TY.FnA s alt s' alt' -> FnU
ft = fromTyped

turtleOpDefine :: Fn (s :> TY.TBytes) s
turtleOpDefine = aop OP_DEFINE

turtleOpInvoke :: FnA s alt s' alt' -> FnA s alt s' alt'
turtleOpInvoke _prog = aop OP_INVOKE
