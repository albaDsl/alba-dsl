-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.MiniTurtleVm101
  ( miniTurtleVm101,
    turtleOpDefine,
    turtleOpInvoke,
  )
where

import Alba.Dsl.V1.Bch2025 (FN, FNA, S (..), type (>))
import Alba.Dsl.V1.Bch2025 qualified as TY
import Alba.Dsl.V1.Bch2025.LangUntyped (repeatProg)
import Alba.Dsl.V1.Bch2025.OpsUntyped (op1, op1Add, opDrop, opIf, opMul)
import Alba.Dsl.V1.Common.CompilerUtils (aop)
import Alba.Dsl.V1.Common.Lang (begin, (#))
import Alba.Dsl.V1.Common.StackUntyped (FNU, fromTyped)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (OP_DEFINE, OP_INVOKE))
import DslDemo.TurtleVm.Maybe (ifJust)
import DslDemo.TurtleVm.TurtleVmStateSimple
  ( getOp,
    getOpBytes,
    initStateWithDefaultOpDefine,
    invokeFunction,
    putFunction,
  )
import DslDemo.TurtleVm.TurtleVmUtilsUntyped (condOpLeaf, is)

miniTurtleVm101 :: FNU
miniTurtleVm101 = ft initStateWithDefaultOpDefine # repeatProg 12 handleOp

handleOp :: FNU
handleOp =
  begin
    # ft getOp
    # ft (ifJust (toSigned # TY.opTrue) (TY.op0 # TY.opFalse))
    # opIf handleOp' opDrop
  where
    toSigned = TY.bytes [0] # TY.opCat # TY.opBin2Num

handleOp' :: FNU
handleOp' =
  condOpLeaf
    [ (is 0x02, ft (getOpBytes 2)), -- OP_DATA_02
      (is 0x51, op1),
      (is 0x8b, op1Add),
      (is 0x89, ft putFunction), -- OP_DEFINE
      (is 0x8a, ft invokeFunction), -- OP_INVOKE
      (is 0x95, opMul)
    ]

ft :: TY.FNA s alt s' alt' -> FNU
ft = fromTyped

turtleOpDefine :: FN (s > TY.TBytes) s
turtleOpDefine (S c fs) = S (aop c OP_DEFINE) fs

turtleOpInvoke :: FNA s alt s' alt' -> FNA s alt s' alt'
turtleOpInvoke _prog (S c fs) = S (aop c OP_INVOKE) fs
