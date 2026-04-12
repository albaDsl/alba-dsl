-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.TxDsl
  ( libraryToTx,
    libraryInputs,
    simpleWrap,
    simpleWrapChunkSize,
    simpleWrapProg,
  )
where

import Alba.Dsl.V1.Bch2025
  ( Bytes,
    CodeL1,
    Fn,
    Optimize (None),
    TBool,
    bytes,
    compile,
    opDrop,
    opTrue,
    (∘),
    type (>),
  )
import Alba.Tx.Bch2025 (OutPoint (..), Tx (..), TxIn (..), TxOut (..))
import Alba.Tx.Bch2025.TxId (TxId (..))
import Alba.Vm.Common.OpcodeL1 (OpcodeL1 (OP_UNASSIGNED_FF), opcodeL1ToWord8)
import Data.ByteString qualified as B
import Data.List (unfoldr)
import Data.Word (Word32, Word64)
import Prelude hiding (drop)

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

simpleWrapProg :: CodeL1 -> Fn s (s > TBool)
simpleWrapProg chunk = bytes chunk ∘ opDrop ∘ opTrue
