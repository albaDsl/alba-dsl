-- Copyright (c) 2025 albaDsl

module Alba.Node.TxVerify (checkTxInputs, contextualCheckTransaction) where

import Alba.Node.ValidationFailure (ValidationFailure (..))
import Alba.Tx.Bch2025 (Tx (..), TxOut (..))
import Alba.Vm.Bch2025
  ( TxContext,
    txContextCoins,
    txContextTx,
  )
import Alba.Vm.Common.VmParams (VmParams (..))
import Control.Monad (guard, unless)
import Data.Binary (encode)
import Data.ByteString.Lazy qualified as BL
import Data.Word (Word64)
import Prelude hiding (sum)

checkTxInputs :: TxContext -> Either ValidationFailure ()
checkTxInputs txContext = do
  let tx = txContextTx txContext
      utxos = txContextCoins txContext
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

contextualCheckTransaction ::
  TxContext ->
  VmParams ->
  Either ValidationFailure ()
contextualCheckTransaction txContext vmParams = do
  let tx = txContextTx txContext
      txSize = BL.length (encode tx)
  unless (txSize >= fromIntegral vmParams.minTxSize) $ Left VfTxUndersize
  unless (tx.version == 1 || tx.version == 2) $ Left VfTxVersion
