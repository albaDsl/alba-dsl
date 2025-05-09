-- Copyright (c) 2025 albaDsl

module Spend (refreshTx, withdrawTx, inheritTx) where

import Alba.Dsl.V1.Bch2025
  ( CodeL1,
    outputScript,
    setScriptSig,
    signAll,
    timeSequence,
  )
import Alba.Misc.Haskoin (Address, scriptAddress)
import Alba.Misc.KeyPair (KeyPair (..))
import Alba.Tx.Bch2025 (OutPoint (..), Tx (..), TxIn (..), TxOut (..))
import ContractApi (inherit, refresh, withdraw)
import Crypto.Secp256k1 (Ctx)
import Data.ByteString qualified as B
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Params (fee, inheritDelay, refreshDelay)
import Prelude hiding (seq)

refreshTx :: Ctx -> OutPoint -> TxOut -> CodeL1 -> KeyPair -> Tx
refreshTx ctx outpoint utxo redeemScript key =
  let seq = fromIntegral $ timeSequence refreshDelay
      recvAddr = scriptAddress redeemScript
      tx = txTemplate outpoint seq (utxo.value - fee) (outputScript recvAddr)
      sig = signAll ctx tx redeemScript utxo 0 key.secKey
   in setScriptSig 0 (refresh ctx redeemScript key.pubKey sig) tx

withdrawTx ::
  Ctx -> OutPoint -> TxOut -> CodeL1 -> KeyPair -> Address -> Tx
withdrawTx ctx outpoint utxo redeemScript key recvAddr =
  let seq = 0
      tx = txTemplate outpoint seq (utxo.value - fee) (outputScript recvAddr)
      sig = signAll ctx tx redeemScript utxo 0 key.secKey
   in setScriptSig 0 (withdraw ctx redeemScript key.pubKey sig) tx

inheritTx ::
  Ctx -> OutPoint -> TxOut -> CodeL1 -> KeyPair -> Address -> Tx
inheritTx ctx outpoint utxo redeemScript key recvAddr =
  let seq = fromIntegral $ timeSequence inheritDelay
      tx = txTemplate outpoint seq (utxo.value - fee) (outputScript recvAddr)
      sig = signAll ctx tx redeemScript utxo 0 key.secKey
   in setScriptSig 0 (inherit ctx redeemScript key.pubKey sig) tx

txTemplate :: OutPoint -> Natural -> Word64 -> B.ByteString -> Tx
txTemplate outpoint seq outAmount scriptPubKey =
  Tx
    { version = 2,
      inputs =
        [ TxIn {prevout = outpoint, scriptSig = [], sequence = fromIntegral seq}
        ],
      outputs =
        [ TxOut
            { value = outAmount,
              scriptPubKey = scriptPubKey,
              tokenData = Nothing
            }
        ],
      lockTime = 0
    }
