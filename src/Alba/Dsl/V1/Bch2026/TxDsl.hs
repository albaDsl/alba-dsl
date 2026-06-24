-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.TxDsl
  ( outputScript,
    setScriptSig,
    signAll,
    timeSequence,
    days,
    hours,
    minutes,
    libraryToTx,
    libraryInputs,
    simpleWrap,
    simpleWrapChunkSize,
    simpleWrapProg,
  )
where

import Alba.Dsl.V1.Bch2026.Lang (bytes, bytes')
import Alba.Dsl.V1.Bch2026.Ops
  ( opCheckSig,
    opDrop,
    opDup,
    opEqual,
    opEqualVerify,
    opHash160,
    opTrue,
  )
import Alba.Dsl.V1.Common.Compile (Optimize (None), compile)
import Alba.Dsl.V1.Common.Lang ((∘))
import Alba.Dsl.V1.Common.Stack (Fn, Stack (..), TBool, THash160, TPubKey, TSig)
import Alba.Misc.Haskoin
  ( Address (..),
    TxSignature (..),
    setForkIdFlag,
    sigHashAll,
    signHash,
  )
import Alba.Tx.Bch2025.Tx (Tx (..))
import Alba.Tx.Bch2025.TxId (TxId (..))
import Alba.Tx.Bch2025.TxIn (OutPoint (..), TxIn (..))
import Alba.Tx.Bch2025.TxOut (TxOut (..))
import Alba.Vm.Bch2025.VmSigHash (signatureHash')
import Alba.Vm.Common.BasicTypes (Bytes)
import Alba.Vm.Common.OpcodeL1
  ( CodeL1,
    OpcodeL1 (OP_UNASSIGNED_FF),
    opcodeL1ToWord8,
  )
import Crypto.Secp256k1 (Ctx, SecKey)
import Data.Binary (encode)
import Data.Bits (setBit, shiftR, (.|.))
import Data.ByteString qualified as B
import Data.ByteString.Lazy (toStrict)
import Data.Generics.Labels ()
import Data.List (unfoldr)
import Data.Word (Word32, Word64)
import Lens.Micro (ix, over)
import Numeric.Natural (Natural)
import Prelude hiding (drop)

outputScript :: Address -> B.ByteString
outputScript (PubKeyAddress h) =
  compile None (p2pkhScriptPubKey (toStrict $ encode h))
outputScript (ScriptAddress h) =
  compile None (p2shScriptPubKey (toStrict $ encode h))

p2shScriptPubKey :: B.ByteString -> Fn (s :> THash160) (s :> TBool)
p2shScriptPubKey scriptHash = opHash160 ∘ bytes' scriptHash ∘ opEqual

p2pkhScriptPubKey :: B.ByteString -> Fn (s :> TSig :> TPubKey) (s :> TBool)
p2pkhScriptPubKey pubKeyHash =
  (opDup ∘ opHash160 ∘ bytes' pubKeyHash ∘ opEqualVerify ∘ opCheckSig)

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

-- Satoshi amount per UTXO deployed.
amount :: Word64
amount = 2000

libraryToTx :: CodeL1 -> Int -> (CodeL1 -> CodeL1) -> Tx
libraryToTx lib chunkSize wrap =
  let chunks =
        unfoldr
          ( \b ->
              if not (B.null b)
                then Just $ B.splitAt chunkSize b
                else Nothing
          )
          lib
      outputs = toOutput . wrap <$> chunks
   in Tx {version = 2, inputs = [], lockTime = 0, ..}
  where
    toOutput :: Bytes -> TxOut
    toOutput chunk =
      TxOut {value = amount, scriptPubKey = chunk, tokenData = Nothing}

libraryInputs :: TxId -> Word32 -> Word32 -> [TxIn]
libraryInputs txId offset numUtxos = input <$> [0 .. numUtxos - 1]
  where
    input idx =
      TxIn
        { prevout = OutPoint txId (offset + idx),
          scriptSig = [],
          sequence = 0
        }

simpleWrap :: CodeL1 -> CodeL1
simpleWrap chunk = compile None (simpleWrapProg (extendWithNops chunk))
  where
    extendWithNops :: CodeL1 -> CodeL1
    extendWithNops chunk'
      | B.length chunk' == simpleWrapChunkSize = chunk'
      | otherwise =
          let diff = simpleWrapChunkSize - B.length chunk'
              nop = opcodeL1ToWord8 OP_UNASSIGNED_FF
           in chunk' <> B.replicate diff nop

simpleWrapChunkSize :: Int
simpleWrapChunkSize = 201 - headerSize - trailerSize
  where
    headerSize = 2 -- OP_PUSHDATA1 <len>
    trailerSize = 2 -- OP_DROP OP_TRUE

simpleWrapProg :: CodeL1 -> Fn s (s :> TBool)
simpleWrapProg chunk = bytes chunk ∘ opDrop ∘ opTrue
