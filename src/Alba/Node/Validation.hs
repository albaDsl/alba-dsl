-- Copyright (c) 2025 albaDsl

module Alba.Node.Validation
  ( AcceptToMemoryPoolResult,
    VerifyScriptFun,
    Mode (..),
    acceptToMemoryPool,
  )
where

import Alba.Node.Policy (areInputsStandard, isStandardTx)
import Alba.Node.TxVerify (checkTxInputs, contextualCheckTransaction)
import Alba.Node.ValidateTokens (checkTxTokens)
import Alba.Node.ValidationFailure (ValidationFailure (..))
import Alba.Tx.Bch2025 (TxOut (..))
import Alba.Vm.Bch2025
  ( TxContext,
    txContextCoins,
    txContextInputIndex,
  )
import Alba.Vm.Common.OpcodeL1 (CodeL1)
import Alba.Vm.Common.ScriptError (ScriptError)
import Alba.Vm.Common.VmParams (VmParams (..))
import Alba.Vm.Common.VmState (VerifyScriptResult)
import Control.Monad (when)
import Prelude hiding (sum)

type VerifyScriptFun =
  CodeL1 ->
  TxContext ->
  VmParams ->
  Either (ScriptError, VerifyScriptResult) VerifyScriptResult

type AcceptToMemoryPoolResult =
  Either
    ValidationFailure
    (Either (ScriptError, VerifyScriptResult) VerifyScriptResult)

data Mode = Standard | Nonstandard
  deriving (Eq)

-- FIXME: Having a Mode flag to this function is a temporary solution.
acceptToMemoryPool ::
  VerifyScriptFun ->
  TxContext ->
  VmParams ->
  Mode ->
  AcceptToMemoryPoolResult
acceptToMemoryPool verifyScript txContext vmParams mode = do
  let coins = txContextCoins txContext
      idx = txContextInputIndex txContext
      scriptPubKey = (coins !! idx).scriptPubKey
  when (mode == Standard) $ isStandardTx txContext vmParams
  contextualCheckTransaction txContext vmParams
  checkTxInputs txContext
  when (mode == Standard) $ areInputsStandard txContext vmParams
  checkTxTokens txContext vmParams
  Right $ verifyScript scriptPubKey txContext vmParams
