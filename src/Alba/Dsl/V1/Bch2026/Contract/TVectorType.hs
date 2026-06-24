-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.TVectorType (TVector) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Bch2026.Contract.Prelude
import Data.Kind (Type)
import Prelude ()

data TVector (a :: Type)

instance StackEntry (TVector a)

instance (BlobEq a) => BlobEq (TVector a) where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord
