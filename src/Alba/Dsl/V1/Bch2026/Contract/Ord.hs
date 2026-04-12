-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Ord where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    StackEntry,
    TBool,
    TInt,
    TNat,
    opGreaterThan,
    opGreaterThanOrEqual,
    opLessThan,
    opLessThanOrEqual,
    opMax,
    opMin,
    opWithin,
    type (>),
  )
-- (StackEntry, Fn, type (>))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqCoreInstances ()
import Data.Kind (Type)
import Prelude (undefined)

data TOrdRec (t :: Type)

instance StackEntry (TOrdRec t)

class (BlobEq a) => Ord a where
  lessThan :: Fn (s > a > a) (s > TBool)
  lessThanOrEqual :: Fn (s > a > a) (s > TBool)
  greaterThan :: Fn (s > a > a) (s > TBool)
  greaterThanOrEqual :: Fn (s > a > a) (s > TBool)
  min :: Fn (s > a > a) (s > a)
  max :: Fn (s > a > a) (s > a)
  within :: Fn (s > a > a > a) (s > TBool)
  blobOrdRec :: Fn s (s > TOrdRec a)

instance Ord TInt where
  lessThan = opLessThan
  lessThanOrEqual = opLessThanOrEqual
  greaterThan = opGreaterThan
  greaterThanOrEqual = opGreaterThanOrEqual
  min = opMin
  max = opMax
  within = opWithin
  blobOrdRec = undefined -- FIXME: implement.

instance Ord TNat where
  lessThan = opLessThan
  lessThanOrEqual = opLessThanOrEqual
  greaterThan = opGreaterThan
  greaterThanOrEqual = opGreaterThanOrEqual
  min = opMin
  max = opMax
  within = opWithin
  blobOrdRec = undefined -- FIXME: implement.
