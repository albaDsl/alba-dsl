-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Unit (TUnit, unit) where

import Alba.Dsl.V1.Bch2025 (Fn, StackEntry, cast, int, (#), type (>))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqUtils
  ( blobEqEqual,
    blobEqEqualVerify,
    blobEqRecord,
  )

data TUnit

instance StackEntry TUnit

instance BlobEq TUnit where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

unit :: Fn s (s > TUnit)
unit = int 0 # cast
