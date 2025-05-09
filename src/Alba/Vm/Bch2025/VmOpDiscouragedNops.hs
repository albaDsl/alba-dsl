-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2025.VmOpDiscouragedNops (evalOpDiscouragedNops) where

import Alba.Vm.Common.OpcodeL1 qualified as L1
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.VmParams (VmParams (..))
import Alba.Vm.Common.VmState (VmState (..))

evalOpDiscouragedNops ::
  OpcodeL2 ->
  VmState ->
  Maybe (Either ScriptError VmState)
evalOpDiscouragedNops op st =
  case op of
    OP_UNUSED L1.OP_NOP1 -> nopOrFail
    OP_UNUSED L1.OP_NOP4 -> nopOrFail
    OP_UNUSED L1.OP_NOP5 -> nopOrFail
    OP_UNUSED L1.OP_NOP6 -> nopOrFail
    OP_UNUSED L1.OP_NOP7 -> nopOrFail
    OP_UNUSED L1.OP_NOP8 -> nopOrFail
    OP_UNUSED L1.OP_NOP9 -> nopOrFail
    OP_UNUSED L1.OP_NOP10 -> nopOrFail
    _ -> Nothing
  where
    nopOrFail =
      Just $
        if not st.params.discourageNops
          then Right st
          else Left SeDiscourageUpgradableNops
