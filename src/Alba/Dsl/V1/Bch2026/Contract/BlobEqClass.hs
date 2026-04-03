-- Copyright (c) 2026 albaDsl
--
-- 'blob' here indicates that we compare for equality using a simple 'opEqual'
-- across the whole stack value. Same for aggregated types. We do not
-- deconstruct them and compare the elements.
--
-- For a type to qualify for the BlobEq typeclass it must satisfy this
-- condition:
--
--    Inhabitants of the type that are considered equal also have the same
--    bytestring representation (i.e. stack representation).
--
-- That we include instances for numeric types (e.g. TInt) in this class means
-- that we consider non-minimally encoded integers to not be part of their
-- respective types.
module Alba.Dsl.V1.Bch2026.Contract.BlobEqClass
  ( BlobEq (..),
    TBlobEqRec,
  )
where

import Alba.Dsl.V1.Bch2026
import Data.Kind (Type)
import Prelude ()

data TBlobEqRec (t :: Type)

instance StackEntry (TBlobEqRec t)

class (StackEntry a) => BlobEq a where
  equal :: (StackEntry a) => Fn (s > a > a) (s > TBool)

  equalVerify :: (StackEntry a) => Fn (s > a > a) s
  equalVerify = equal # opVerify

  blobEqRec :: Fn s (s > TBlobEqRec a)
