-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2025.VmOpEqualityAndConditionals (evalOpConditionals) where

import Alba.Vm.Bch2025.Utils (boa1, condStackExecuteP, op1v)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement (stackElementToBool')
import Alba.Vm.Common.VmStack (CondStackElement (..))
import Alba.Vm.Common.VmState (VmState (..))
import Control.Monad (unless)
import Data.Sequence (Seq ((:|>)), (|>))

{- ORMOLU_DISABLE -}
evalOpConditionals ::
  OpcodeL2 ->
  VmState ->
  Maybe (Either ScriptError VmState)
evalOpConditionals op st@(VmState {s, exec, params}) =
  case op of
    OP_IF -> Just $ do
      if condStackExecuteP exec
        then do
          (s' :|> x1) <- pure s
          Right $ st {s = s', exec = exec |> Exec (stackElementToBool' x1)}
        else Right $ st {exec = exec |> Exec False}
    OP_NOTIF -> Just $ do
      if condStackExecuteP exec
        then do
          (s' :|> x1) <- pure s
          Right $
            st {s = s', exec = exec |> Exec ((not . stackElementToBool') x1)}
        else Right $ st {exec = exec |> Exec False}
    OP_ELSE   -> Just $ toggleTop >>= \exec' -> Right $ st {exec = exec'}
    OP_ENDIF  -> Just $ dropTop >>= \exec' -> Right $ st {exec = exec'}
    OP_NOP    -> Just $ Right st
    OP_RETURN -> Just $ Left SeOpReturn
    OP_VERIFY -> op1v st (boa1 params (`unless` Left SeVerify))
    _ -> Nothing
  where
    toggleTop = case exec of
      (rest :|> Exec top) -> Right $ rest |> Exec (not top)
      _ -> Left SeUnbalancedConditional

    dropTop = case exec of
      (rest :|> Exec _top) -> Right rest
      _ -> Left SeUnbalancedConditional
{- ORMOLU_ENABLE -}
