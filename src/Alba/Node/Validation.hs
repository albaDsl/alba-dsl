-- Copyright (c) 2025 albaDsl

module Alba.Node.Validation (acceptToMemoryPool) where

import Alba.Node.ValidateTokens (verifyTxTokens)
import Alba.Node.ValidationFailure (ValidationFailure (..))
import Alba.Tx.Bch2025 (Tx (..), TxIn (..), TxOut (..))
import Alba.Vm.Bch2025
  ( TxContext,
    VmParams (..),
    txContextCoins,
    txContextInputIndex,
    txContextTx,
    verifyScript,
  )
import Alba.Vm.Common.ScriptError (ScriptError)
import Alba.Vm.Common.VmState (VerifyScriptResult)
import Control.Monad (guard, unless)
import Data.Binary (encode)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Word (Word64)
import Prelude hiding (sum)

acceptToMemoryPool ::
  TxContext ->
  VmParams ->
  Either
    ValidationFailure
    (Either (ScriptError, VerifyScriptResult) VerifyScriptResult)
acceptToMemoryPool txContext vmParams = do
  let coins = txContextCoins txContext
      idx = txContextInputIndex txContext
      scriptPubKey = (coins !! idx).scriptPubKey
  verifyTx txContext vmParams
  verifyTxTokens txContext
  Right $ verifyScript scriptPubKey txContext vmParams

verifyTx :: TxContext -> VmParams -> Either ValidationFailure ()
verifyTx txContext vmParams = do
  let tx = txContextTx txContext
      utxos = txContextCoins txContext
      txSize = BL.length (encode tx)
  unless (txSize <= fromIntegral vmParams.maxStandardTxSize) $ Left VfTxOversize
  unless (txSize >= fromIntegral vmParams.minTxSize) $ Left VfTxUndersize
  unless (tx.version == 1 || tx.version == 2) $ Left VfTxVersion
  unless
    ( case vmParams.maxTxInScriptSigSize of
        Just limit -> all (\x -> B.length x.scriptSig <= limit) tx.inputs
        Nothing -> True
    )
    $ Left VfTxScriptSigSize
  inSum <-
    maybe (Left VfAmounts) Right $ moneySum ((\o -> o.value) <$> utxos)
  outSum <-
    maybe (Left VfAmounts) Right $ moneySum ((\o -> o.value) <$> tx.outputs)
  unless (outSum <= inSum) $ Left VfAmounts

moneySum :: [Word64] -> Maybe Word64
moneySum = moneySum' 0
  where
    moneySum' :: Word64 -> [Word64] -> Maybe Word64
    moneySum' sum [] = Just sum
    moneySum' sum (x : xs) = do
      guard (moneyRange sum)
      guard (moneyRange x)
      moneySum' (sum + x) xs

moneyRange :: Word64 -> Bool
moneyRange amount | amount > maxSatoshis = False
  where
    maxSatoshis = 21_000_000_000_000_00
moneyRange _ = True
