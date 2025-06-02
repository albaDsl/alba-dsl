-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2025.VmOpEqualityAndConditionals (evalOpConditionals) where

import Alba.Misc.Utils (maybeToEither)
import Alba.Vm.Bch2025.Utils (boa1, op1v)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement (stackElementToBool')
import Alba.Vm.Common.VmStack
  ( CondStackElement (..),
    condStackExecDrop,
    condStackExecToggle,
    condStackExecuteP,
    condStackPush,
  )
import Alba.Vm.Common.VmState (VmState (..))
import Control.Monad (unless)
import Data.Sequence (Seq ((:|>)))

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
          let entry = Exec (stackElementToBool' x1)
          Right $ st {s = s', exec = condStackPush exec entry }
        else Right $ st {exec = condStackPush exec (Exec False)}
    OP_NOTIF -> Just $ do
      if condStackExecuteP exec
        then do
          (s' :|> x1) <- pure s
          let entry = Exec ((not . stackElementToBool') x1)
          Right $ st {s = s', exec = condStackPush exec entry}
        else Right $ st {exec = condStackPush exec (Exec False)}
    OP_ELSE   -> Just $ toggleTop >>= \exec' -> Right $ st {exec = exec'}
    OP_ENDIF  -> Just $ dropTop >>= \exec' -> Right $ st {exec = exec'}
    OP_NOP    -> Just $ Right st
    OP_RETURN -> Just $ Left SeOpReturn
    OP_VERIFY -> op1v st (boa1 params (`unless` Left SeVerify))
    _ -> Nothing
  where
    toggleTop = maybeToEither SeUnbalancedConditional (condStackExecToggle exec)

    dropTop = maybeToEither SeUnbalancedConditional (condStackExecDrop exec)
{- ORMOLU_ENABLE -}
