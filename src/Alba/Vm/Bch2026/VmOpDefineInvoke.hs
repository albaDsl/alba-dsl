-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2026.VmOpDefineInvoke (evalOpDefineInvoke) where

import Alba.Vm.Common.BasicTypes (Bytes)
import Alba.Vm.Common.OpcodeL1 (CodeL1)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement (StackElement (..), stackElementToBytes)
import Alba.Vm.Common.VmLimits (addBytesPushed)
import Alba.Vm.Common.VmParams (VmParams (..))
import Alba.Vm.Common.VmStack (CondStackElement (..), condStackPush)
import Alba.Vm.Common.VmState (VmState (..))
import Data.ByteString qualified as B
import Data.Map qualified as M
import Data.Sequence (Seq ((:|>)))

evalOpDefineInvoke :: OpcodeL2 -> VmState -> Maybe (Either ScriptError VmState)
evalOpDefineInvoke op st@(VmState {code, signedCode, exec, functions, s}) =
  case op of
    OP_DEFINE -> Just $ do
      (s' :|> body :|> name) <- pure s
      let name' = stackElementToBytes name
          body' = stackElementToBytes body
      verifyFunctionIdentifierSize st.params name'
      functions' <- insertFunctionOrFail name' body' functions
      pure $ addBytesPushed body.byteSize (st {s = s', functions = functions'})
    OP_INVOKE -> Just $ do
      (s' :|> name) <- pure s
      let name' = stackElementToBytes name
      verifyFunctionIdentifierSize st.params name'
      body <- lookupFunctionOrFail name' functions
      let entry = Eval {callerCode = code, callerSignedCode = signedCode}
          exec' = condStackPush exec entry
      pure st {code = body, signedCode = body, s = s', exec = exec'}
    _ -> Nothing

verifyFunctionIdentifierSize :: VmParams -> Bytes -> Either ScriptError ()
verifyFunctionIdentifierSize vmParams name =
  if B.length name <= vmParams.maxFunctionIdentifierLength
    then Right ()
    else Left SeInvalidFunctionIdentifier

insertFunctionOrFail ::
  Bytes ->
  CodeL1 ->
  M.Map Bytes CodeL1 ->
  Either ScriptError (M.Map Bytes CodeL1)
insertFunctionOrFail name body functions =
  case M.lookup name functions of
    Nothing -> Right $ M.insert name body functions
    Just _ -> Left SeFunctionOverwriteDisallowed

lookupFunctionOrFail ::
  Bytes ->
  M.Map Bytes CodeL1 ->
  Either ScriptError CodeL1
lookupFunctionOrFail name functions =
  case M.lookup name functions of
    Just body -> Right body
    Nothing -> Left SeInvokedUndefinedFunction
