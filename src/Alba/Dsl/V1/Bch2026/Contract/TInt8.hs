-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.TInt8 (TInt8, int8) where

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
    quot1,
    quot2,
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

data TInt8

instance StackEntry TInt8

instance BlobEq TInt8 where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance Ord TInt8 where
  lessThan = toRaw2 . opLessThan
  lessThanOrEqual = toRaw2 . opLessThanOrEqual
  greaterThan = toRaw2 . opGreaterThan
  greaterThanOrEqual = toRaw2 . opGreaterThanOrEqual
  min = toRaw2 . opMin . fromRaw
  max = toRaw2 . opMax . fromRaw
  within = toRaw3 . opWithin
  ordRec = quot2 (lessThanOrEqual @TInt8) . mkOrdM

instance Integral TInt8 where
  add = toRaw2 . opAdd . fromInt
  add1 = toRaw . op1Add . fromInt
  sub = toRaw2 . opSub . fromInt
  sub1 = toRaw . op1Sub . fromInt
  mul = toRaw2 . opMul . fromInt
  div = toRaw2 . opDiv . fromInt
  mod = toRaw2 . opMod . fromInt
  negate = toRaw . opNegate . fromRaw
  abs = toRaw . opAbs . fromRaw
  fromInt = fn (dup . int int8Min . int int8Max . opWithin . opVerify . fromRaw)
  toInt = toRaw

instance PackFs TInt8 where
  sizeConst = 1
  size = nat (sizeConst @TInt8)
  pack = toRaw . size @TInt8 . opNum2Bin
  unpack = opBin2Num . fromRaw
  packFsRec = int8PackFs

int8PackFs :: Fn s (s :> TPackFs TInt8)
int8PackFs =
  constant
    ( begin
        . size @TInt8
        . quot1 (pack @TInt8)
        . quot1 (unpack @TInt8)
        . mkPackFsM
    )

int8Max :: Integer
int8Max = 2 ^ (7 :: Integer) - 1

int8Min :: Integer
int8Min = -int8Max

int8 :: Integer -> Fn s (s :> TInt8)
int8 x = assert (x >= int8Min && x <= int8Max) (int x . fromRaw)

fromRaw :: Fn (s :> TInt) (s :> TInt8)
fromRaw = cast

toRaw :: Fn (s :> TInt8) (s :> TInt)
toRaw = cast

toRaw2 :: Fn (s :> TInt8 :> TInt8) (s :> TInt :> TInt)
toRaw2 = castStack

toRaw3 :: Fn (s :> TInt8 :> TInt8 :> TInt8) (s :> TInt :> TInt :> TInt)
toRaw3 = castStack
