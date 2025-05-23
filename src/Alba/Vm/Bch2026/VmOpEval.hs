-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2026.VmOpEval (evalOpEval) where

import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement (stackElementToBytes)
import Alba.Vm.Common.VmStack (CondStackElement (..))
import Alba.Vm.Common.VmState (VmState (..))
import Data.Sequence (Seq ((:|>)), (|>))

evalOpEval ::
  OpcodeL2 ->
  VmState ->
  Maybe (Either ScriptError VmState)
evalOpEval op st@(VmState {code, signedCode, s, exec}) =
  case op of
    OP_EVAL -> Just $ do
      (s' :|> lambda) <- pure s
      let lambda' = stackElementToBytes lambda
          exec' = exec |> Eval {cseCode = code, cseSignedCode = signedCode}
      pure st {code = lambda', signedCode = lambda', s = s', exec = exec'}
    _ -> Nothing
