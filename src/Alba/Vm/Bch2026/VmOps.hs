-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2026.VmOps (evalVmOp) where

import Alba.Vm.Bch2025.TxContext (TxContext)
import Alba.Vm.Bch2025.VmOps qualified as Bch2025
import Alba.Vm.Bch2026.VmOpEval (evalOpEval)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2)
import Alba.Vm.Common.ScriptError (ScriptError)
import Alba.Vm.Common.VmState (VmState)
import Control.Applicative ((<|>))

evalVmOp ::
  OpcodeL2 ->
  TxContext ->
  VmState ->
  Maybe (Either ScriptError VmState)
evalVmOp op txContext state =
  Bch2025.evalVmOp op txContext state
    <|> evalOpEval op state
