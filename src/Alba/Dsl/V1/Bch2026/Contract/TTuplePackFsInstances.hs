-- Copyright (c) 2026 albaDsl
{-# OPTIONS_GHC -Wno-orphans #-}

module Alba.Dsl.V1.Bch2026.Contract.TTuplePackFsInstances where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack ((:>)),
    begin,
    constant,
    fn,
    nat,
    opDup,
    quot1,
    quot2,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.Misc (pad, unpad)
import Alba.Dsl.V1.Bch2026.Contract.PackFs (PackFs (..), TPackFs, mkPackFsM)
import Alba.Dsl.V1.Bch2026.Contract.PackFsCoreInstances ()
import Alba.Dsl.V1.Bch2026.Contract.PartialApplicationA (apply2)
import Alba.Dsl.V1.Bch2026.Contract.TInt16 (TInt16)
import Alba.Dsl.V1.Bch2026.Contract.TTuple (TTuple)
import Numeric.Natural (Natural)
import Prelude ((+))

-- Size of the size fields: 2 (TTuple) + 1 (pad).
sizeFields :: Natural
sizeFields = 3

-- ## Generic instance.
instance {-# OVERLAPPABLE #-} (PackFs a, PackFs b) => PackFs (TTuple a b) where
  sizeConst = sizeConst @a + sizeConst @b + sizeFields
  size = nat (sizeConst @(TTuple a b))
  pack = size @(TTuple a b) . pad
  unpack = unpad
  packFsRec = tuplePackFs0

tuplePackFs0 ::
  forall s a b.
  (PackFs (TTuple a b)) =>
  Fn s (s :> TPackFs (TTuple a b))
tuplePackFs0 = size @(TTuple a b) . tuplePackFs
  where
    tuplePackFs = fn (opDup . quot2 pad . apply2 . quot1 unpad . mkPackFsM)

-- ## A few specific instances added for performance.
instance PackFs (TTuple TInt16 TInt16) where
  sizeConst = sizeConst @TInt16 + sizeConst @TInt16 + sizeFields
  size = nat (sizeConst @(TTuple TInt16 TInt16))
  pack = size @(TTuple TInt16 TInt16) . pad
  unpack = unpad
  packFsRec = tuplePackFs5

tuplePackFs5 ::
  (PackFs (TTuple TInt16 TInt16)) =>
  Fn s (s :> TPackFs (TTuple TInt16 TInt16))
tuplePackFs5 =
  constant
    ( begin
        . size @(TTuple TInt16 TInt16)
        . quot1 (pack @(TTuple TInt16 TInt16))
        . quot1 (unpack @(TTuple TInt16 TInt16))
        . mkPackFsM
    )
