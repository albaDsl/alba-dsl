-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2026.VmOpLoops (evalOpLoops) where

import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement (stackElementToBool')
import Alba.Vm.Common.VmStack
  ( CondStackElement (..),
    condStackExecuteP,
    condStackPush,
    condStackUncons,
  )
import Alba.Vm.Common.VmState (VmState (..))
import Data.Sequence (Seq ((:|>)))

evalOpLoops ::
  OpcodeL2 ->
  VmState ->
  Maybe (Either ScriptError VmState)
evalOpLoops op st@(VmState {code, s, exec}) =
  case op of
    OP_BEGIN -> Just $ do
      let exec' = condStackPush exec (Loop {loopStart = code})
      pure st {exec = exec'}
    OP_UNTIL -> Just $ do
      case condStackUncons exec of
        (Just (Loop {loopStart}), exec') -> do
          if condStackExecuteP exec
            then do
              (s' :|> x1) <- pure s
              if not (stackElementToBool' x1)
                then pure st {code = loopStart, s = s'}
                else pure st {s = s', exec = exec'}
            else pure st {exec = exec'}
        (Just _, _) -> Left SeUnbalancedConditional
        (Nothing, _) -> Left SeUnbalancedConditional
    _ -> Nothing
