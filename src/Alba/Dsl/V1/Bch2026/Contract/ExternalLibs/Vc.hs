-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Vc where

import Alba.Dsl.V1.Bch2026
  ( CompilationResult (CompilationResult, code, functionTable),
    Fn,
    Optimize (O1),
    StackEntry,
    TNat,
    begin,
    compileLibrary,
    n2i,
    nat,
    op1Add,
    opSwap,
    (∘),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Integral (Integral (..))
import Alba.Dsl.V1.Bch2026.Contract.Ord (Ord (..))
import Alba.Dsl.V1.Bch2026.Contract.PackFs (PackFs (packFsRec), TPackFs)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, swap)
import Alba.Dsl.V1.Bch2026.Contract.TInt8 (TInt8, int8)
import Alba.Dsl.V1.Bch2026.Contract.TVector (TVector)
import Alba.Dsl.V1.Bch2026.Contract.TVector qualified as V
import Alba.Dsl.V1.Bch2026.ExternalLib (LibData (..), invokeExt)
import Alba.Dsl.V1.Bch2026.Lang (lambda1, lambda2, runEnv)
import Alba.Dsl.V1.Bch2026.TxDsl
  ( libraryToTx,
    simpleWrap,
    simpleWrapChunkSize,
  )
import Alba.Tx.Bch2025.Hash (hash256)
import Alba.Tx.Bch2025.Tx (Tx (..))
import Data.ByteString qualified as B
import Data.Word (Word32)
import DslDemo.MergeSort.MergeSort qualified as MS
import Prelude hiding (drop)

lib :: LibData
lib =
  let CompilationResult {..} = compileLibrary O1 "vc" showCase
      size = B.length code
      deployCode = code
      deploySize = B.length deployCode
      hash = hash256 code
   in LibData {..}

deployTx :: Tx
deployTx = libraryToTx lib.deployCode simpleWrapChunkSize simpleWrap

numUtxos :: Word32
numUtxos = fromIntegral $ Prelude.length deployTx.outputs

showCase :: Fn s s
showCase =
  runEnv
    ( begin
        ∘ (int8Vector ∘ V.head ∘ drop)
        ∘ (int8Vector ∘ V.last ∘ drop)
        ∘ (int8Vector ∘ V.length ∘ drop)
        ∘ (lambda1 (drop ∘ int8 2) ∘ int8Vector ∘ V.map ∘ drop)
        ∘ (lambda2 add ∘ int8 0 ∘ int8Vector ∘ V.foldl ∘ drop)
        ∘ (int8Vector ∘ int8Vector ∘ V.zip ∘ drop)
        ∘ (lambda2 add ∘ int8Vector ∘ int8Vector ∘ V.zipWith ∘ drop)
        ∘ (nat 10 ∘ lambda1 (op1Add ∘ n2Int8) ∘ V.generate)
        ∘ (lambda1 (int8 3 ∘ lessThan) ∘ swap ∘ V.filter ∘ drop)
        ∘ (int8Vector ∘ MS.sort ∘ drop)
    )
  where
    int8Vector :: Fn s (s > TVector TInt8)
    int8Vector = int8 0 ∘ int8 1 ∘ V.empty ∘ V.cons ∘ V.cons

    n2Int8 :: Fn (s > TNat) (s > TInt8)
    n2Int8 = n2i ∘ fromInt

sort :: forall a s. (PackFs a) => Fn (s > TVector a) (s > TVector a)
sort = packFsRec @a ∘ opSwap ∘ sortF

sortF :: (StackEntry a) => Fn (s > TPackFs a > TVector a) (s > TVector a)
sortF = invokeExt lib "DslDemo.MergeSort.MergeSort" "sortF"

length :: forall a s. (PackFs a) => Fn (s > TVector a) (s > TNat)
length = packFsRec @a ∘ swap ∘ lengthF

lengthF :: Fn (s > TPackFs a > TVector a) (s > TNat)
lengthF = invokeExt lib "Alba.Dsl.V1.Bch2026.Contract.TVector" "lengthF"
