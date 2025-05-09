-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2025.VmOpBitwiseLogic (evalOpBitwiseLogic) where

import Alba.Vm.Bch2025.Utils (ba2, bor2, nc2, op2, op2v, verifyEqual)
import Alba.Vm.Bch2025.Utils qualified as VU
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.VmState (VmState (..))
import Data.Bits (xor, (.&.), (.|.))

{- ORMOLU_DISABLE -}
evalOpBitwiseLogic :: OpcodeL2 -> VmState -> Maybe (Either ScriptError VmState)
evalOpBitwiseLogic op st =
  case op of
    OP_AND ->         op2  st (bw2 (.&.))
    OP_OR ->          op2  st (bw2 (.|.))
    OP_XOR ->         op2  st (bw2 xor)
    OP_EQUAL ->       op2  st ((bor2 . ba2 . nc2) (==))
    OP_EQUALVERIFY -> op2v st (ba2 $ verifyEqual SeEqualVerify)
    _ -> Nothing
  where
    bw2 = VU.bw2 st.params
{- ORMOLU_ENABLE -}
