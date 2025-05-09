-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2025.VmOps (evalVmOp) where

import Alba.Vm.Bch2025.TxContext (TxContext)
import Alba.Vm.Bch2025.VmOpArithmetic (evalOpArithmetic)
import Alba.Vm.Bch2025.VmOpBitwiseLogic (evalOpBitwiseLogic)
import Alba.Vm.Bch2025.VmOpBytes (evalOpBytes)
import Alba.Vm.Bch2025.VmOpConstants (evalOpConstants)
import Alba.Vm.Bch2025.VmOpDiscouragedNops (evalOpDiscouragedNops)
import Alba.Vm.Bch2025.VmOpEqualityAndConditionals (evalOpConditionals)
import Alba.Vm.Bch2025.VmOpHash (evalOpHash)
import Alba.Vm.Bch2025.VmOpIntrospection (evalOpIntrospection)
import Alba.Vm.Bch2025.VmOpLocktime (evalOpLocktime)
import Alba.Vm.Bch2025.VmOpMultiSig (evalOpMultiSig)
import Alba.Vm.Bch2025.VmOpSig (evalOpSig)
import Alba.Vm.Bch2025.VmOpStack (evalOpStack)
import Alba.Vm.Bch2025.VmOpTokenIntrospection (evalOpTokenIntrospection)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.VmState (CodeL1, VmState (..))
import Control.Applicative ((<|>))

evalVmOp ::
  OpcodeL2 ->
  CodeL1 ->
  TxContext ->
  VmState ->
  Maybe (Either ScriptError VmState)
evalVmOp op code txContext state =
  evalOpConstants op state
    <|> evalOpConditionals op state
    <|> evalOpStack op state
    <|> evalOpBytes op state
    <|> evalOpBitwiseLogic op state
    <|> evalOpArithmetic op state
    <|> evalOpHash op state
    <|> evalOpSig op code txContext state
    <|> evalOpMultiSig op code txContext state
    <|> evalOpLocktime op txContext state
    <|> evalOpIntrospection op txContext state
    <|> evalOpTokenIntrospection op txContext state
    <|> evalOpDiscouragedNops op state
