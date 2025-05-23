-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Alba.Vm.Bch2025.VmOpSig (evalOpSig) where

import Alba.Vm.Bch2025.SigCheckUtils (checkSig, verifySignature)
import Alba.Vm.Bch2025.SigEncoding
  ( checkDataSignatureEncoding,
    checkPubKeyEncoding,
  )
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement
  ( StackElement (..),
    boolToStackElement,
    stackElementToBool,
    stackElementToBytes,
  )
import Alba.Vm.Common.Tx (sha256)
import Alba.Vm.Common.TxContext (TxContext)
import Alba.Vm.Common.VmLimits (addBytesPushed, addHashIterations, addSigCheck)
import Alba.Vm.Common.VmState (VmState (..))
import Data.ByteString qualified as B
import Data.Sequence (Seq ((:|>)))
import Prelude hiding (seq)

evalOpSig ::
  OpcodeL2 ->
  TxContext ->
  VmState ->
  Maybe (Either ScriptError VmState)
evalOpSig op txContext st@VmState {..} =
  case op of
    OP_CHECKSIG -> Just $ opCheckSig txContext st
    OP_CHECKSIGVERIFY -> Just $ verifyOp opCheckSig SeCheckSigVerify
    OP_CHECKDATASIG -> Just $ opCheckDataSig txContext st
    OP_CHECKDATASIGVERIFY ->
      Just $ verifyOp opCheckDataSig SeCheckDataSigVerify
    OP_CODESEPARATOR -> Just $ Right $ st {signedCode = code}
    _ -> Nothing
  where
    verifyOp f err = do
      st'@VmState {s = s' :|> res} <- f txContext st
      res' <- stackElementToBool params res
      if res' then Right $ st' {s = s'} else Left err

opCheckSig :: TxContext -> VmState -> Either ScriptError VmState
opCheckSig txContext VmState {..} = do
  (s' :|> sig :|> pubKey) <- pure s
  let sig' = stackElementToBytes sig
      pubKey' = stackElementToBytes pubKey
  checkDataSignatureEncoding sig'
  checkPubKeyEncoding pubKey'
  let st' = case checkSig txContext signedCode sig' pubKey' of
        Just (res, imgSize) ->
          let res' = boolToStackElement res
           in ( addBytesPushed res'.byteSize
                  . addSigCheck 1
                  . addHashIterations imgSize True
              )
                (VmState {s = s' :|> res', ..})
        Nothing ->
          let res = boolToStackElement False
           in addBytesPushed res.byteSize (VmState {s = s' :|> res, ..})
  Right st'

opCheckDataSig :: TxContext -> VmState -> Either ScriptError VmState
opCheckDataSig _ VmState {..} = do
  (s' :|> sig :|> msg :|> pubKey) <- pure s
  let sig' = stackElementToBytes sig
      msg' = stackElementToBytes msg
      pubKey' = stackElementToBytes pubKey
  checkDataSignatureEncoding sig'
  checkPubKeyEncoding pubKey'
  let hash = sha256 msg'
      imgSize = B.length msg'
      res = verifySignature sig' pubKey' hash
      res' = boolToStackElement res
  Right $
    ( addBytesPushed res'.byteSize
        . addSigCheck (if B.null sig' then 0 else 1)
        . addHashIterations imgSize False
    )
      (VmState {s = s' :|> res', ..})
