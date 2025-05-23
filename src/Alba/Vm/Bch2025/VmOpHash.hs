-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2025.VmOpHash (evalOpHash) where

import Alba.Vm.Bch2025.Utils (ba1, nc1, op1h)
import Alba.Vm.Bch2025.Utils qualified as VU
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.Tx (hash160, hash256, ripemd160, sha1, sha256)
import Alba.Vm.Common.VmState (VmState (..))

{- ORMOLU_DISABLE -}
evalOpHash :: OpcodeL2 -> VmState -> Maybe (Either ScriptError VmState)
evalOpHash op st =
  case op of
    OP_RIPEMD160 -> op1h st ((br1 . ba1 . nc1) ripemd160) False
    OP_SHA1 ->      op1h st ((br1 . ba1 . nc1) sha1) False
    OP_SHA256 ->    op1h st ((br1 . ba1 . nc1) sha256) False
    OP_HASH160 ->   op1h st ((br1 . ba1 . nc1) hash160) True
    OP_HASH256 ->   op1h st ((br1 . ba1 . nc1) hash256) True
    _ -> Nothing
  where
    br1 = VU.br1 st.params
{- ORMOLU_ENABLE -}
