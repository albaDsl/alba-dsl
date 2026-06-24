-- Copyright (c) 2025 albaDsl

module Alba.Vm.BchSpec.VmOpEval (evalOpEval) where

import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement (stackElementToBytes)
import Alba.Vm.Common.VmStack (CondStackElement (..), condStackPush)
import Alba.Vm.Common.VmState (VmState (..))
import Data.Sequence (Seq ((:|>)))

evalOpEval ::
  OpcodeL2 ->
  VmState ->
  Maybe (Either ScriptError VmState)
evalOpEval op st@(VmState {code, signedCode, s, exec}) =
  case op of
    OP_EVAL -> Just $ do
      (s' :|> code') <- pure s
      let code'' = stackElementToBytes code'
          entry = Eval {callerCode = code, callerSignedCode = signedCode}
          exec' = condStackPush exec entry
      pure st {code = code'', signedCode = code'', s = s', exec = exec'}
    _ -> Nothing
