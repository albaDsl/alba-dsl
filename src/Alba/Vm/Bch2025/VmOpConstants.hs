-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2025.VmOpConstants (evalOpConstants) where

import Alba.Vm.Bch2025.Utils (ir0, nc0, op0)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement (bytesToStackElement)
import Alba.Vm.Common.VmState (VmState (..))

{- ORMOLU_DISABLE -}
evalOpConstants :: OpcodeL2 -> VmState -> Maybe (Either ScriptError VmState)
evalOpConstants op st =
  case op of
    OP_1NEGATE -> op0Const (-1)
    OP_0 ->       op0Const 0
    OP_1 ->       op0Const 1
    OP_2 ->       op0Const 2
    OP_3 ->       op0Const 3
    OP_4 ->       op0Const 4
    OP_5 ->       op0Const 5
    OP_6 ->       op0Const 6
    OP_7 ->       op0Const 7
    OP_8 ->       op0Const 8
    OP_9 ->       op0Const 9
    OP_10 ->      op0Const 10
    OP_11 ->      op0Const 11
    OP_12 ->      op0Const 12
    OP_13 ->      op0Const 13
    OP_14 ->      op0Const 14
    OP_15 ->      op0Const 15
    OP_16 ->      op0Const 16

    OP_DATA _opcodeL1 bytes -> op0 st (bytesToStackElement st.params bytes)

    _ -> Nothing
  where
    op0Const :: Integer -> Maybe (Either ScriptError VmState)
    op0Const x = op0 st ((ir0 st.params . nc0) x)
{- ORMOLU_ENABLE -}
