-- Copyright (c) 2026 albaDsl
{-# OPTIONS_GHC -Wno-orphans #-}

module Alba.Dsl.V1.Bch2026.Contract.TTuplePackFsInstances where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack ((:>)),
    begin,
    constant,
    lambda1,
    nat,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.Misc (pad, unpad)
import Alba.Dsl.V1.Bch2026.Contract.PackFs
  ( PackFs (..),
    TPackFs,
    mkPackFsM,
  )
import Alba.Dsl.V1.Bch2026.Contract.TBytes128 (TBytes128)
import Alba.Dsl.V1.Bch2026.Contract.TInt16 (TInt16)
import Alba.Dsl.V1.Bch2026.Contract.TInt64 (TInt64)
import Alba.Dsl.V1.Bch2026.Contract.TInt8 (TInt8)
import Alba.Dsl.V1.Bch2026.Contract.TTuple (TTuple)
import Numeric.Natural (Natural)
import Prelude ((+))

-- Size of the size fields: 2 (TTuple) + 1 (pad).
sizeFields :: Natural
sizeFields = 3

instance PackFs (TTuple TInt8 TInt8) where
  sizeConst = sizeConst @TInt8 + sizeConst @TInt8 + sizeFields
  size = nat (sizeConst @(TTuple TInt8 TInt8))
  pack = size @(TTuple TInt8 TInt8) . pad
  unpack = unpad
  packFsRec = tuplePackFs1

tuplePackFs1 ::
  (PackFs (TTuple TInt8 TInt8)) =>
  Fn s (s :> TPackFs (TTuple TInt8 TInt8))
tuplePackFs1 =
  constant
    ( begin
        . size @(TTuple TInt8 TInt8)
        . lambda1 (pack @(TTuple TInt8 TInt8))
        . lambda1 (unpack @(TTuple TInt8 TInt8))
        . mkPackFsM
    )

instance PackFs (TTuple TInt64 TInt8) where
  sizeConst = sizeConst @TInt64 + sizeConst @TInt8 + sizeFields
  size = nat (sizeConst @(TTuple TInt64 TInt8))
  pack = size @(TTuple TInt64 TInt8) . pad
  unpack = unpad
  packFsRec = tuplePackFs2

tuplePackFs2 ::
  (PackFs (TTuple TInt64 TInt8)) =>
  Fn s (s :> TPackFs (TTuple TInt64 TInt8))
tuplePackFs2 =
  constant
    ( begin
        . size @(TTuple TInt64 TInt8)
        . lambda1 (pack @(TTuple TInt64 TInt8))
        . lambda1 (unpack @(TTuple TInt64 TInt8))
        . mkPackFsM
    )

instance PackFs (TTuple TInt64 TBytes128) where
  sizeConst = sizeConst @TInt64 + sizeConst @TBytes128 + sizeFields
  size = nat (sizeConst @(TTuple TInt64 TBytes128))
  pack = size @(TTuple TInt64 TBytes128) . pad
  unpack = unpad
  packFsRec = tuplePackFs3

tuplePackFs3 ::
  (PackFs (TTuple TInt64 TBytes128)) =>
  Fn s (s :> TPackFs (TTuple TInt64 TBytes128))
tuplePackFs3 =
  constant
    ( begin
        . size @(TTuple TInt64 TBytes128)
        . lambda1 (pack @(TTuple TInt64 TBytes128))
        . lambda1 (unpack @(TTuple TInt64 TBytes128))
        . mkPackFsM
    )

instance PackFs (TTuple TInt64 TInt64) where
  sizeConst = sizeConst @TInt64 + sizeConst @TInt64 + sizeFields
  size = nat (sizeConst @(TTuple TInt64 TInt64))
  pack = size @(TTuple TInt64 TInt64) . pad
  unpack = unpad
  packFsRec = tuplePackFs4

tuplePackFs4 ::
  (PackFs (TTuple TInt64 TInt64)) =>
  Fn s (s :> TPackFs (TTuple TInt64 TInt64))
tuplePackFs4 =
  constant
    ( begin
        . size @(TTuple TInt64 TInt64)
        . lambda1 (pack @(TTuple TInt64 TInt64))
        . lambda1 (unpack @(TTuple TInt64 TInt64))
        . mkPackFsM
    )

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
        . lambda1 (pack @(TTuple TInt16 TInt16))
        . lambda1 (unpack @(TTuple TInt16 TInt16))
        . mkPackFsM
    )
