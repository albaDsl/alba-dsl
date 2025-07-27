-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Spend (withdrawTx) where

import Alba.Dsl.V1.Bch2025
  ( CodeL1,
    FN,
    Optimize (..),
    TInt,
    begin,
    bytes,
    compile,
    int,
    op1Add,
    opMul,
    outputScript,
    setScriptSig,
    (#),
    type (>),
  )
import Alba.Misc.Haskoin (Address)
import Alba.Tx.Bch2025 (OutPoint (..), Tx (..), TxIn (..), TxOut (..))
import ContractApi (withdraw)
import Crypto.Secp256k1 (Ctx)
import Data.ByteString qualified as B
import Data.Word (Word64)
import DslDemo.TurtleVm.Bch2025.MiniTurtleVm101 (turtleOpDefine, turtleOpInvoke)
import Numeric.Natural (Natural)
import Params (fee)
import Prelude hiding (seq)

withdrawTx ::
  Ctx -> OutPoint -> TxOut -> CodeL1 -> Address -> Tx
withdrawTx ctx outpoint utxo redeemScript recvAddr =
  let solution = compile None progSolution
      seq = 0
      tx = txTemplate outpoint seq (utxo.value - fee) (outputScript recvAddr)
   in setScriptSig 0 (withdraw ctx solution redeemScript) tx

-- This solution is already public. There is a nice solution that is a fair
-- amount shorter too.
progSolution :: FN s (s > TInt)
progSolution =
  begin
    # bytes (compile None f)
    # turtleOpDefine
    # int 1
    # op1Add
    # int 1
    # turtleOpInvoke f
    # op1Add
  where
    f = op1Add # opMul

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
