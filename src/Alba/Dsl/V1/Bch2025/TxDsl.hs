-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2025.TxDsl
  ( outputScript,
    setScriptSig,
    signAll,
    timeSequence,
    days,
    hours,
    minutes,
  )
where

import Alba.Dsl.V1.Bch2025.Compile (Optimize (None), compile)
import Alba.Dsl.V1.Bch2025.Contract.Prelude
  ( p2pkhScriptPubKey,
    p2shScriptPubKey,
  )
import Alba.Misc.Haskoin
  ( Address (..),
    TxSignature (..),
    setForkIdFlag,
    sigHashAll,
    signHash,
  )
import Alba.Tx.Bch2025 (Tx, TxOut (..))
import Alba.Vm.Bch2025.VmSigHash (signatureHash')
import Alba.Vm.Common.OpcodeL1 (CodeL1)
import Crypto.Secp256k1 (Ctx, SecKey)
import Data.Binary (encode)
import Data.Bits (setBit, shiftR, (.|.))
import Data.ByteString qualified as B
import Data.ByteString.Lazy (toStrict)
import Data.Generics.Labels ()
import Lens.Micro (ix, over)
import Numeric.Natural (Natural)

outputScript :: Address -> B.ByteString
outputScript (PubKeyAddress h) =
  compile None (p2pkhScriptPubKey (toStrict $ encode h))
outputScript (ScriptAddress h) =
  compile None (p2shScriptPubKey (toStrict $ encode h))

setScriptSig :: Int -> CodeL1 -> Tx -> Tx
setScriptSig idx code =
  over (#inputs . ix idx . #scriptSig) (const code)

signAll ::
  Ctx ->
  Tx ->
  CodeL1 ->
  TxOut ->
  Natural ->
  SecKey ->
  TxSignature
signAll ctx tx code utxo idx secKey =
  let sigHashType = setForkIdFlag sigHashAll
      idx' = fromIntegral idx
      (hash, _) = signatureHash' tx code [utxo] idx' sigHashType
   in TxSignature (signHash ctx secKey hash) sigHashType

timeSequence :: Natural -> Natural
timeSequence seconds
  | seconds <= maxSeconds =
      setBit 0 22 .|. (seconds `shiftR` 9)
  where
    maxSeconds = 33_554_431
timeSequence _ = error "timeSequence"

days :: Natural -> Natural
days d = secsPerDay * d
  where
    secsPerDay = 86400

hours :: Natural -> Natural
hours h = secsPerHour * h
  where
    secsPerHour = 3600

minutes :: Natural -> Natural
minutes m = secsPerMinute * m
  where
    secsPerMinute = 60
