-- Copyright (c) 2026 albaDsl

module Alba.Vm.BchSpec.VmOps (evalVmOp) where

import Alba.Vm.Bch2026.VmOps qualified as Bch2026
import Alba.Vm.BchSpec.VmOpEval (evalOpEval)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2)
import Alba.Vm.Common.ScriptError (ScriptError)
import Alba.Vm.Common.TxContext (TxContext)
import Alba.Vm.Common.VmState (VmState)
import Control.Applicative ((<|>))

evalVmOp ::
  OpcodeL2 ->
  TxContext ->
  VmState ->
  Maybe (Either ScriptError VmState)
evalVmOp op txContext state =
  Bch2026.evalVmOp op txContext state
    <|> evalOpEval op state
