-- Copyright (c) 2026 albaDsl

module Spend (withdrawTx, withdrawTx') where

import Alba.Dsl.V1.Bch2025.TxDsl (outputScript, setScriptSig)
import Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Dc qualified as Dc
import Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Vc qualified as Vc
import Alba.Dsl.V1.Bch2026.TxDsl (libraryInputs)
import Alba.Dsl.V1.Common.LzssBit (compress)
import Alba.Misc.Bchn (getTx)
import Alba.Misc.Haskoin (Address, Network)
import Alba.Tx.Bch2025 (OutPoint (..), Tx (..), TxId, TxIn (..), TxOut (..))
import ContractApi (withdraw)
import Crypto.Secp256k1 (Ctx)
import Data.Word (Word64)
import Params (spendFee)

-- We are disregarding the funds sitting in the library UTXOs, as if they were
-- used read-only.
withdrawTx :: Ctx -> Network -> TxId -> Address -> IO Tx
withdrawTx ctx net txId recvAddr = do
  deployTx <- either err id <$> getTx net txId
  let contractUtxoIndex = 9 :: Int
      contractUtxo = deployTx.outputs !! contractUtxoIndex
      outpoint = OutPoint txId (fromIntegral contractUtxoIndex)
  pure $ withdrawTx' ctx contractUtxo outpoint recvAddr
  where
    err = error "Couldn't load contract deploy Tx."

withdrawTx' :: Ctx -> TxOut -> OutPoint -> Address -> Tx
withdrawTx' ctx contractUtxo outpoint recvAddr =
  let solution = compress "She sells seashells by the sea"
      tx = txTemplate outpoint (contractUtxo.value - spendFee) recvAddr
   in setScriptSig inputId (withdraw ctx solution) tx
  where
    -- Our contract input follows right after the vc lib.
    inputId = fromIntegral (Dc.numUtxos + Vc.numUtxos)

txTemplate :: OutPoint -> Word64 -> Address -> Tx
txTemplate outpoint outAmount recvAddr =
  Tx
    { version = 2,
      inputs =
        libraryInputs outpoint.txId 0 Dc.numUtxos
          <> libraryInputs outpoint.txId Dc.numUtxos Vc.numUtxos
          <> [TxIn {prevout = outpoint, scriptSig = [], sequence = 0}],
      outputs =
        [ TxOut
            { value = outAmount,
              scriptPubKey = outputScript recvAddr,
              tokenData = Nothing
            }
        ],
      lockTime = 0
    }
