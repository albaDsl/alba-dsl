-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Dc where

import Alba.Dsl.V1.Bch2026
  ( FN,
    LibData (..),
    Optimize (O1),
    TBytes,
    bytes,
    compileLibrary,
    invokeExt,
    libraryToTx,
    simpleWrap,
    simpleWrapChunkSize,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Lzss qualified as LZ
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop)
import Alba.Dsl.V1.Common.OpcodeL3 (FunctionId (Standard))
import Alba.Tx.Bch2025 (Tx (..), hash256)
import Data.ByteString qualified as B
import Data.Word (Word32)
import Prelude hiding (drop)

lib :: LibData
lib =
  let (code, functionTable) = compileLibrary O1 "dc" showCase
      size = B.length code
      hash = hash256 code
      deploySize = size
      deployCode = code
   in LibData {..}

deployTx :: Tx
deployTx = libraryToTx lib.code simpleWrapChunkSize simpleWrap

numUtxos :: Word32
numUtxos = fromIntegral $ length deployTx.outputs

showCase :: FN s s
showCase = bytes "0341421300" # LZ.decompress # drop

decompress :: FN (s > TBytes) (s > TBytes)
decompress =
  invokeExt lib (Standard "Alba.Dsl.V1.Bch2026.Contract.Lzss" 80 3 "decompress")
