-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2025.Vm (evaluateScript, startState, verifyScript) where

import Alba.Vm.Bch2025.VmOps (evalVmOp)
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.TxContext (TxContext)
import Alba.Vm.Common.Vm qualified as VmCommon
import Alba.Vm.Common.VmParams (VmParams (..))
import Alba.Vm.Common.VmState (CodeL1, VerifyScriptResult (..), VmState (..))

evaluateScript ::
  TxContext ->
  VmState ->
  Either (ScriptError, Maybe VmState) VmState
evaluateScript = VmCommon.evaluateScript evalVmOp

verifyScript ::
  CodeL1 ->
  TxContext ->
  VmParams ->
  Either (ScriptError, VerifyScriptResult) VerifyScriptResult
verifyScript = VmCommon.verifyScript evalVmOp

startState :: VmParams -> VmState
startState = VmCommon.startState
