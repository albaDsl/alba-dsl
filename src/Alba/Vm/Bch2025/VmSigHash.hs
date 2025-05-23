-- Copyright (c) 2025 albaDsl
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Alba.Vm.Bch2025.VmSigHash (signatureHash, signatureHash') where

import Alba.Misc.Haskoin
  ( SigHashType,
    anyoneCanPay,
    hasUtxosFlag,
    isSigHashNone,
    isSigHashSingle,
    putVarInt,
  )
import Alba.Vm.Common (CodeL1)
import Alba.Vm.Common.Tx
  ( Hash256 (..),
    Tx (..),
    TxIn (..),
    TxOut (..),
    hash256',
    hash256zero,
    prefixToken,
  )
import Alba.Vm.Common.TxContext
  ( TxContext,
    txContextCoins,
    txContextInputIndex,
    txContextTx,
  )
import Data.Binary (Binary (..))
import Data.Binary.Put (putByteString, putWord32le, putWord64le, runPut)
import Data.ByteString qualified as B
import Data.ByteString.Lazy (toStrict)

signatureHash :: TxContext -> CodeL1 -> SigHashType -> (Hash256, Int)
signatureHash txContext code sigHashType =
  let tx = txContextTx txContext
      utxos = txContextCoins txContext
      inputIndex = txContextInputIndex txContext
   in signatureHash' tx code utxos inputIndex sigHashType

signatureHash' ::
  Tx ->
  CodeL1 ->
  [TxOut] ->
  Int ->
  SigHashType ->
  (Hash256, Int)
signatureHash' tx code utxos idx sigHashType =
  let input = tx.inputs !! idx
      utxo = utxos !! idx
      image =
        toStrict
          ( runPut $ do
              putWord32le tx.version
              put hashPrevouts
              (if hasUtxosFlag sigHashType then put hashUtxos else nothing)
              put hashSequence
              put input.prevout
              ( case utxo.tokenData of
                  Just td -> put prefixToken >> put td
                  Nothing -> nothing
                )
              scriptCode
              putWord64le utxo.value
              putWord32le input.sequence
              put hashOutputs
              putWord32le tx.lockTime
              putWord32le $ fromIntegral sigHashType
          )
   in (hash256' image, B.length image)
  where
    nothing = putByteString B.empty

    hashPrevouts =
      if not (anyoneCanPay sigHashType)
        then hash256' $ toStrict (runPut $ mapM_ (put . (.prevout)) tx.inputs)
        else hash256zero

    hashSequence =
      if not (anyoneCanPay sigHashType)
        && not (isSigHashSingle sigHashType)
        && not (isSigHashNone sigHashType)
        then
          hash256' $
            toStrict (runPut $ mapM_ (putWord32le . (.sequence)) tx.inputs)
        else hash256zero

    hashUtxos = hash256' $ toStrict (runPut $ mapM_ put utxos)

    hashOutputs
      | not (isSigHashSingle sigHashType) && not (isSigHashNone sigHashType) =
          hash256' $ toStrict (runPut $ mapM_ put tx.outputs)
      | isSigHashSingle sigHashType && idx < length tx.outputs =
          hash256' $ toStrict (runPut $ put (tx.outputs !! idx))
      | otherwise = hash256zero

    scriptCode = putVarInt (B.length code) >> putByteString code
