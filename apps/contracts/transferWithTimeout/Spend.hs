-- Copyright (c) 2025 albaDsl

module Spend (senderWithdrawTx, recipientWithdrawTx) where

import Alba.Dsl.V1.Bch2025 (CodeL1, outputScript, setScriptSig, signAll)
import Alba.Misc.Haskoin (Address (..))
import Alba.Tx.Bch2025 (OutPoint (..), Tx (..), TxIn (..), TxOut (..))
import ContractApi (recipientWithdraw, senderWithdraw)
import Crypto.Secp256k1 (Ctx, SecKey)
import Numeric.Natural (Natural)

recipientWithdrawTx ::
  Ctx ->
  OutPoint ->
  TxOut ->
  CodeL1 ->
  SecKey ->
  Address ->
  Tx
recipientWithdrawTx ctx outpoint utxo redeemScript secKey recvAddr =
  let tx = txTemplate outpoint recvAddr 0
      sig = signAll ctx tx redeemScript utxo 0 secKey
   in setScriptSig 0 (recipientWithdraw ctx redeemScript sig) tx

senderWithdrawTx ::
  Ctx ->
  OutPoint ->
  TxOut ->
  CodeL1 ->
  SecKey ->
  Natural ->
  Address ->
  Tx
senderWithdrawTx ctx outpoint utxo redeemScript secKey timeout recvAddr =
  let tx = txTemplate outpoint recvAddr timeout
      sig = signAll ctx tx redeemScript utxo 0 secKey
   in setScriptSig 0 (senderWithdraw ctx redeemScript sig) tx

txTemplate :: OutPoint -> Address -> Natural -> Tx
txTemplate outpoint recvAddr timeout =
  Tx
    { version = 2,
      inputs = [TxIn {prevout = outpoint, scriptSig = [], sequence = 0}],
      outputs =
        [ TxOut
            { value = 9_500,
              scriptPubKey = outputScript recvAddr,
              tokenData = Nothing
            }
        ],
      lockTime = fromIntegral timeout
    }
