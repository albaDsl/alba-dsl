-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2025.Vm (evaluateScript, startState, verifyScript) where

import Alba.Vm.Bch2025.VmOps (evalVmOp)
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.TxContext (TxContext)
import Alba.Vm.Common.Vm qualified as CV
import Alba.Vm.Common.VmParams (VmParams (..))
import Alba.Vm.Common.VmState (CodeL1, VerifyScriptResult (..), VmState (..))

evaluateScript ::
  TxContext ->
  VmState ->
  Either (ScriptError, Maybe VmState) VmState
evaluateScript = CV.evaluateScript (CV.Deps evalVmOp)

verifyScript ::
  CodeL1 ->
  TxContext ->
  VmParams ->
  Either (ScriptError, VerifyScriptResult) VerifyScriptResult
verifyScript = CV.verifyScript (CV.Deps evalVmOp)

startState :: VmParams -> VmState
startState = CV.startState
