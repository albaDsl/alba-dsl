-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.TUnit (TUnit, unit) where

import Alba.Dsl.V1.Bch2025 (Fn, StackEntry, TBytes, bytes, cast, (.), type (>))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqUtils
  ( blobEqEqual,
    blobEqEqualVerify,
    blobEqRecord,
  )
import Prelude ()

data TUnit

instance StackEntry TUnit

instance BlobEq TUnit where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

unit :: Fn s (s > TUnit)
unit = bytes [] . fromRaw

fromRaw :: Fn (s > TBytes) (s > TUnit)
fromRaw = cast
