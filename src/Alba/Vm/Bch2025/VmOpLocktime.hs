-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2025.VmOpLocktime (evalOpLocktime) where

import Alba.Vm.Bch2025.Utils (ia1p, op1vk)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.Tx (Tx (..), TxIn (..))
import Alba.Vm.Common.TxContext (TxContext, txContextInputIndex, txContextTx)
import Alba.Vm.Common.VmParams (VmParams (..))
import Alba.Vm.Common.VmState (VmState (..))
import Control.Monad (unless)
import Data.Bits ((.&.), (.|.))
import Prelude hiding (seq)

evalOpLocktime ::
  OpcodeL2 ->
  TxContext ->
  VmState ->
  Maybe (Either ScriptError VmState)
evalOpLocktime op txContext st =
  case op of
    OP_CHECKLOCKTIMEVERIFY -> op1vk st (ia1p st.params 5 verifyLockTime)
    OP_CHECKSEQUENCEVERIFY -> op1vk st (ia1p st.params 5 verifySequence)
    _ -> Nothing
  where
    verifyLockTime time = do
      verifyPositive time SeNegativeLocktime
      verifyLockTime' st.params time txContext SeUnsatisfiedLocktime

    verifySequence seq = do
      verifyPositive seq SeNegativeLocktime
      unless ((seq .&. st.params.sequenceLocktimeDisableFlag) == 0) (Right ())
      verifySequence' st.params seq txContext SeUnsatisfiedLocktime

    verifyPositive x _err | x >= 0 = Right ()
    verifyPositive _x err = Left err

-- Clarification on logic:
-- https://bitcoin.stackexchange.com/questions/40706/
-- why-is-op-checklocktimeverify-disabled-by-maximum-sequence-number
verifyLockTime' ::
  VmParams ->
  Integer ->
  TxContext ->
  ScriptError ->
  Either ScriptError ()
verifyLockTime' vmParams time txContext err = do
  unless ((txLt < th && time < th) || (txLt >= th && time >= th)) (Left err)
  unless (txLt >= time) (Left err)
  unless (currentInput.sequence /= vmParams.sequenceFinal) (Left err)
  where
    tx = txContextTx txContext
    txLt = fromIntegral tx.lockTime :: Integer
    th = fromIntegral vmParams.lockTimeThreshold :: Integer
    currentInput = tx.inputs !! txContextInputIndex txContext

verifySequence' ::
  VmParams ->
  Integer ->
  TxContext ->
  ScriptError ->
  Either ScriptError ()
verifySequence' vmParams seq txContext err = do
  unless (tx.version >= 2) (Left err)
  unless ((txSeq .&. vmParams.sequenceLocktimeDisableFlag) == 0) (Left err)
  let txSeq' = fromIntegral $ txSeq .&. lockTimeMask :: Integer
      seq' = seq .&. fromIntegral lockTimeMask
  unless
    ((txSeq' < sltf && seq < sltf) || (txSeq' >= sltf && seq >= sltf))
    (Left err)
  unless (seq' <= txSeq') (Left err)
  where
    tx = txContextTx txContext
    idx = txContextInputIndex txContext
    txSeq = fromIntegral (tx.inputs !! idx).sequence
    lockTimeMask =
      vmParams.sequenceLocktimeTypeFlag
        .|. vmParams.sequenceLocktimeMask
    sltf = fromIntegral vmParams.sequenceLocktimeTypeFlag :: Integer
