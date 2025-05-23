-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.OpClasses (isConditionalOp, isPushOp, isDisabledOp) where

import Alba.Vm.Common.OpcodeL1 qualified as L1
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))

isConditionalOp :: OpcodeL2 -> Bool
isConditionalOp OP_IF = True
isConditionalOp OP_NOTIF = True
isConditionalOp (OP_UNUSED L1.OP_VERIF) = True
isConditionalOp (OP_UNUSED L1.OP_VERNOTIF) = True
isConditionalOp OP_ELSE = True
isConditionalOp OP_ENDIF = True
isConditionalOp _ = False

isPushOp :: OpcodeL2 -> Bool
isPushOp OP_0 = True
isPushOp (OP_DATA _ _) = True
isPushOp OP_1NEGATE = True
isPushOp (OP_UNUSED L1.OP_RESERVED) = True
isPushOp OP_1 = True
isPushOp OP_2 = True
isPushOp OP_3 = True
isPushOp OP_4 = True
isPushOp OP_5 = True
isPushOp OP_6 = True
isPushOp OP_7 = True
isPushOp OP_8 = True
isPushOp OP_9 = True
isPushOp OP_10 = True
isPushOp OP_11 = True
isPushOp OP_12 = True
isPushOp OP_13 = True
isPushOp OP_14 = True
isPushOp OP_15 = True
isPushOp OP_16 = True
isPushOp _ = False

isDisabledOp :: OpcodeL2 -> Bool
isDisabledOp (OP_UNUSED L1.OP_INVERT) = True
isDisabledOp (OP_UNUSED L1.OP_2MUL) = True
isDisabledOp (OP_UNUSED L1.OP_2DIV) = True
isDisabledOp (OP_UNUSED L1.OP_LSHIFT) = True
isDisabledOp (OP_UNUSED L1.OP_RSHIFT) = True
isDisabledOp _ = False
