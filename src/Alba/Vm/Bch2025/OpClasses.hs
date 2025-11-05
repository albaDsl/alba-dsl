-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2025.OpClasses where

import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))

isDisabledOp :: OpcodeL2 -> Bool
isDisabledOp OP_INVERT = True
isDisabledOp OP_LSHIFTNUM = True -- L1.OP_2MUL_OP_LSHIFTNUM
isDisabledOp OP_RSHIFTNUM = True -- L1.OP_2DIV_OP_RSHIFTNUM
isDisabledOp OP_LSHIFTBIN = True -- L1.OP_LSHIFT_OP_LSHIFTBIN
isDisabledOp OP_RSHIFTBIN = True -- L1.OP_LSHIFT_OP_RSHIFTBIN
isDisabledOp _ = False
