-- Copyright (c) 2026 albaDsl

module Spend (withdrawTx) where

import Alba.Dsl.V1.Bch2026 (CodeL1, outputScript, setScriptSig)
import Alba.Misc.Bchn (getTx)
import Alba.Misc.Haskoin (Address, Network)
import Alba.Tx.Bch2025 (OutPoint (..), Tx (..), TxId, TxIn (..), TxOut (..))
import ContractApi (withdraw)
import Crypto.Secp256k1 (Ctx)
import Data.Word (Word64)
import Params (spendFee)

withdrawTx :: Ctx -> Network -> TxId -> CodeL1 -> Address -> IO Tx
withdrawTx ctx net txId redeemScript recvAddr = do
  deployTx <- either err id <$> getTx net txId
  let (utxo, outpoint) = findP2ShUtxoToSpend txId deployTx redeemScript
      solution = 4_000_000_000
      spendTx = txTemplate outpoint (utxo.value - spendFee) recvAddr
   in pure $ setScriptSig 0 (withdraw ctx redeemScript solution) spendTx
  where
    err = error "Couldn't load contract deploy Tx."

-- Hardcoded to refer to output 0 in the given Tx for now.
findP2ShUtxoToSpend :: TxId -> Tx -> CodeL1 -> (TxOut, OutPoint)
findP2ShUtxoToSpend txId tx _ =
  let idx = 0
   in (tx.outputs !! idx, OutPoint txId (fromIntegral idx))

txTemplate :: OutPoint -> Word64 -> Address -> Tx
txTemplate outpoint outAmount recvAddr =
  Tx
    { version = 2,
      inputs =
        [ TxIn {prevout = outpoint, scriptSig = [], sequence = 0xffffffff}
        ],
      outputs =
        [ TxOut
            { value = outAmount,
              scriptPubKey = outputScript recvAddr,
              tokenData = Nothing
            }
        ],
      lockTime = 0
    }
