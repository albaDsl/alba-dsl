-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2026.VmOpDefineInvoke (evalOpDefineInvoke) where

import Alba.Vm.Common.OpcodeL1 (CodeL1)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement (stackElementToBytes, stackElementToInteger)
import Alba.Vm.Common.VmParams (VmParams (..))
import Alba.Vm.Common.VmStack (CondStackElement (..), condStackPush)
import Alba.Vm.Common.VmState (VmState (..))
import Data.Map qualified as M
import Data.Sequence (Seq ((:|>)))

evalOpDefineInvoke :: OpcodeL2 -> VmState -> Maybe (Either ScriptError VmState)
evalOpDefineInvoke op st@(VmState {code, signedCode, exec, functions, s}) =
  case op of
    OP_DEFINE -> Just $ do
      (s' :|> body :|> index) <- pure s
      index' <- stackElementToInteger st.params index
      let body' = stackElementToBytes body
      verifyFunctionIndexRange st.params index'
      functions' <- insertFunctionOrFail index' body' functions
      pure st {s = s', functions = functions'}
    OP_INVOKE -> Just $ do
      (s' :|> index) <- pure s
      index' <- stackElementToInteger st.params index
      verifyFunctionIndexRange st.params index'
      body <- lookupFunctionOrFail index' functions
      let entry = Eval {callerCode = code, callerSignedCode = signedCode}
          exec' = condStackPush exec entry
      pure st {code = body, signedCode = body, s = s', exec = exec'}
    _ -> Nothing

verifyFunctionIndexRange :: VmParams -> Integer -> Either ScriptError ()
verifyFunctionIndexRange vmParams index =
  if index < 0 || index > fromIntegral vmParams.maxFunctionIdentifier
    then Left SeInvalidFunctionIdentifier
    else Right ()

insertFunctionOrFail ::
  Integer ->
  CodeL1 ->
  M.Map Int CodeL1 ->
  Either ScriptError (M.Map Int CodeL1)
insertFunctionOrFail index body functions =
  let index' = fromIntegral index
   in case M.lookup index' functions of
        Nothing -> Right $ M.insert index' body functions
        Just _ -> Left SeFunctionOverwriteDisallowed

lookupFunctionOrFail ::
  Integer ->
  M.Map Int CodeL1 ->
  Either ScriptError CodeL1
lookupFunctionOrFail index functions =
  let index' = fromIntegral index
   in case M.lookup index' functions of
        Just body -> Right body
        Nothing -> Left SeInvokedUndefinedFunction
