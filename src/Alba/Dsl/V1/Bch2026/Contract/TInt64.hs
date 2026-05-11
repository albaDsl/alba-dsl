-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.TInt64 (TInt64, int64) where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack (..),
    StackEntry,
    TInt,
    begin,
    cast,
    castStack,
    constant,
    fn,
    int,
    lambda1,
    lambda2,
    nat,
    op1Add,
    op1Sub,
    opAbs,
    opAdd,
    opBin2Num,
    opDiv,
    opGreaterThan,
    opGreaterThanOrEqual,
    opLessThan,
    opLessThanOrEqual,
    opMax,
    opMin,
    opMod,
    opMul,
    opNegate,
    opNum2Bin,
    opSub,
    opVerify,
    opWithin,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqUtils
  ( blobEqEqual,
    blobEqEqualVerify,
    blobEqRecord,
  )
import Alba.Dsl.V1.Bch2026.Contract.Integral (Integral (..))
import Alba.Dsl.V1.Bch2026.Contract.Ord (Ord (..), mkOrdM)
import Alba.Dsl.V1.Bch2026.Contract.PackFs (PackFs (..), TPackFs, mkPackFsM)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (dup)
import Control.Exception (assert)
import Prelude (Integer, (&&), (-), (<=), (>=), (^))

data TInt64

instance StackEntry TInt64

instance BlobEq TInt64 where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance Ord TInt64 where
  lessThan = toRaw2 . opLessThan
  lessThanOrEqual = toRaw2 . opLessThanOrEqual
  greaterThan = toRaw2 . opGreaterThan
  greaterThanOrEqual = toRaw2 . opGreaterThanOrEqual
  min = toRaw2 . opMin . fromRaw
  max = toRaw2 . opMax . fromRaw
  within = toRaw3 . opWithin
  ordRec = lambda2 (lessThanOrEqual @TInt64) . mkOrdM

instance Integral TInt64 where
  add = toRaw2 . opAdd . fromInt
  add1 = toRaw . op1Add . fromInt
  sub = toRaw2 . opSub . fromInt
  sub1 = toRaw . op1Sub . fromInt
  mul = toRaw2 . opMul . fromInt
  div = toRaw2 . opDiv . fromInt
  mod = toRaw2 . opMod . fromInt
  negate = toRaw . opNegate . fromRaw
  abs = toRaw . opAbs . fromRaw
  fromInt =
    fn (dup . int int64Min . int int64Max . opWithin . opVerify . fromRaw)
  toInt = toRaw

instance PackFs TInt64 where
  sizeConst = 8
  size = nat (sizeConst @TInt64)
  pack = toRaw . size @TInt64 . opNum2Bin
  unpack = opBin2Num . fromRaw
  packFsRec = int64PackFs

int64PackFs :: Fn s (s :> TPackFs TInt64)
int64PackFs =
  constant
    ( begin
        . size @TInt64
        . lambda1 (pack @TInt64)
        . lambda1 (unpack @TInt64)
        . mkPackFsM
    )

int64Max :: Integer
int64Max = 2 ^ (63 :: Integer) - 1

int64Min :: Integer
int64Min = -int64Max

int64 :: Integer -> Fn s (s :> TInt64)
int64 x = assert (x >= int64Min && x <= int64Max) (int x . fromRaw)

fromRaw :: Fn (s :> TInt) (s :> TInt64)
fromRaw = cast

toRaw :: Fn (s :> TInt64) (s :> TInt)
toRaw = cast

toRaw2 :: Fn (s :> TInt64 :> TInt64) (s :> TInt :> TInt)
toRaw2 = castStack

toRaw3 :: Fn (s :> TInt64 :> TInt64 :> TInt64) (s :> TInt :> TInt :> TInt)
toRaw3 = castStack
