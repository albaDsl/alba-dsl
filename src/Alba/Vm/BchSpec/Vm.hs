-- Copyright (c) 2026 albaDsl

module Alba.Vm.BchSpec.Vm (evaluateScript, startState, verifyScript) where

import Alba.Vm.BchSpec.OpClasses (isDisabledOp)
import Alba.Vm.BchSpec.VmOps (evalVmOp)
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.TxContext (TxContext)
import Alba.Vm.Common.Vm qualified as CV
import Alba.Vm.Common.VmParams (VmParams (..))
import Alba.Vm.Common.VmState (CodeL1, VerifyScriptResult (..), VmState (..))

evaluateScript ::
  TxContext ->
  VmState ->
  Either (ScriptError, Maybe VmState) VmState
evaluateScript = CV.evaluateScript (CV.Deps evalVmOp isDisabledOp)

verifyScript ::
  CodeL1 ->
  TxContext ->
  VmParams ->
  Either (ScriptError, VerifyScriptResult) VerifyScriptResult
verifyScript = CV.verifyScript (CV.Deps evalVmOp isDisabledOp)

startState :: VmParams -> VmState
startState = CV.startState
