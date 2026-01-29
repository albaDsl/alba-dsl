-- Copyright (c) 2026 albaDsl

module Spend (withdrawTx) where

import Alba.Dsl.V1.Bch2025.TxDsl (outputScript, setScriptSig)
import Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Dc qualified as Dc
import Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Vc qualified as Vc
import Alba.Dsl.V1.Bch2026.TxDsl (libraryInputs)
import Alba.Dsl.V1.Common.Lzss (compress)
import Alba.Misc.Haskoin (Address)
import Alba.Tx.Bch2025 (OutPoint (..), Tx (..), TxId, TxIn (..), TxOut (..))
import ContractApi (withdraw)
import Crypto.Secp256k1 (Ctx)
import Data.Word (Word64)
import Params (fee)

withdrawTx ::
  Ctx -> OutPoint -> Word64 -> TxId -> TxId -> Address -> Tx
withdrawTx ctx outpoint amount dcLibTxId vcLibTxId recvAddr =
  let solution = compress "She sells seashells by the seashore"
      tx = txTemplate outpoint (amount - fee) dcLibTxId vcLibTxId recvAddr
   in setScriptSig inputId (withdraw ctx solution) tx
  where
    -- Our contract input follows right after the vc lib.
    inputId = fromIntegral (Dc.numUtxos + Vc.numUtxos)

txTemplate :: OutPoint -> Word64 -> TxId -> TxId -> Address -> Tx
txTemplate outpoint outAmount dcLibTxId vcLibTxId recvAddr =
  Tx
    { version = 2,
      inputs =
        libraryInputs dcLibTxId 0 Dc.numUtxos
          <> libraryInputs vcLibTxId Dc.numUtxos Vc.numUtxos
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
