-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Ord
  ( TOrdRec,
    Ord (..),
    mkOrdM,
    getLessThanOrEqual,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack (..),
    StackEntry,
    TBool,
    TInt,
    TQuotB,
    TNat,
    cast,
    quot2,
    opGreaterThan,
    opGreaterThanOrEqual,
    opLessThan,
    opLessThanOrEqual,
    opMax,
    opMin,
    opWithin,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqCoreInstances ()
import Data.Kind (Type)
import Prelude ()

data TOrdRec (t :: Type)

instance StackEntry (TOrdRec t)

class (BlobEq a) => Ord a where
  lessThan :: Fn (s :> a :> a) (s :> TBool)
  lessThanOrEqual :: Fn (s :> a :> a) (s :> TBool)
  greaterThan :: Fn (s :> a :> a) (s :> TBool)
  greaterThanOrEqual :: Fn (s :> a :> a) (s :> TBool)
  min :: Fn (s :> a :> a) (s :> a)
  max :: Fn (s :> a :> a) (s :> a)
  within :: Fn (s :> a :> a :> a) (s :> TBool)
  ordRec :: Fn s (s :> TOrdRec a)

-- Only holds 'lessThanOrEqual' for now.
mkOrdM :: Fn (s :> TQuotB '[a, a] '[TBool]) (s :> TOrdRec a)
mkOrdM = fromRaw

getLessThanOrEqual :: Fn (s :> TOrdRec a) (s :> TQuotB '[a, a] '[TBool])
getLessThanOrEqual = toRaw

fromRaw :: Fn (s :> TQuotB '[a, a] '[TBool]) (s :> TOrdRec a)
fromRaw = cast

toRaw :: Fn (s :> TOrdRec a) (s :> TQuotB '[a, a] '[TBool])
toRaw = cast

instance Ord TInt where
  lessThan = opLessThan
  lessThanOrEqual = opLessThanOrEqual
  greaterThan = opGreaterThan
  greaterThanOrEqual = opGreaterThanOrEqual
  min = opMin
  max = opMax
  within = opWithin
  ordRec = quot2 (lessThanOrEqual @TInt) . mkOrdM

instance Ord TNat where
  lessThan = opLessThan
  lessThanOrEqual = opLessThanOrEqual
  greaterThan = opGreaterThan
  greaterThanOrEqual = opGreaterThanOrEqual
  min = opMin
  max = opMax
  within = opWithin
  ordRec = quot2 (lessThanOrEqual @TNat) . mkOrdM
