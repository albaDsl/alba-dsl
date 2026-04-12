-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Dc where

import Alba.Dsl.V1.Bch2026
  ( CompilationResult (..),
    Fn,
    LibData (..),
    Optimize (O1),
    TBytes,
    bytes,
    compileLibrary,
    invokeExt,
    libraryToTx,
    simpleWrap,
    simpleWrapChunkSize,
    (.),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.LzssBit qualified as LZ
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop)
import Alba.Tx.Bch2025 (Tx (..), hash256)
import Data.ByteString qualified as B
import Data.Word (Word32)
import Prelude (fromIntegral, length, ($))

lib :: LibData
lib =
  let CompilationResult {..} = compileLibrary O1 "dc" showCase
      size = B.length code
      hash = hash256 code
      deploySize = size
      deployCode = code
   in LibData {..}

deployTx :: Tx
deployTx = libraryToTx lib.code simpleWrapChunkSize simpleWrap

numUtxos :: Word32
numUtxos = fromIntegral $ length deployTx.outputs

showCase :: Fn s s
showCase = bytes "0341421300" . LZ.decompress . drop

decompress :: Fn (s > TBytes) (s > TBytes)
decompress = invokeExt lib "Alba.Dsl.V1.Bch2026.Contract.LzssBit" "decompress"
