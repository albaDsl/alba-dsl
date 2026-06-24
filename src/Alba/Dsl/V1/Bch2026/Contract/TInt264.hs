-- Copyright (c) 2026 albaDsl

-- 264-bit (33 byte) integers. Allows for storage of e.g. 256-bit quantities
-- which a 32 byte integer would not due to the sign bit.

module Alba.Dsl.V1.Bch2026.Contract.TInt264 (TInt264, int264) where

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
    quot1,
    quot2,
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

data TInt264

instance StackEntry TInt264

instance BlobEq TInt264 where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance Ord TInt264 where
  lessThan = toRaw2 . opLessThan
  lessThanOrEqual = toRaw2 . opLessThanOrEqual
  greaterThan = toRaw2 . opGreaterThan
  greaterThanOrEqual = toRaw2 . opGreaterThanOrEqual
  min = toRaw2 . opMin . fromRaw
  max = toRaw2 . opMax . fromRaw
  within = toRaw3 . opWithin
  ordRec = quot2 (lessThanOrEqual @TInt264) . mkOrdM

instance Integral TInt264 where
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
    fn (dup . int int264Min . int int264Max . opWithin . opVerify . fromRaw)
  toInt = toRaw

instance PackFs TInt264 where
  sizeConst = 33
  size = nat (sizeConst @TInt264)
  pack = toRaw . size @TInt264 . opNum2Bin
  unpack = opBin2Num . fromRaw
  packFsRec = int264PackFs

int264PackFs :: Fn s (s :> TPackFs TInt264)
int264PackFs =
  constant
    ( begin
        . size @TInt264
        . quot1 (pack @TInt264)
        . quot1 (unpack @TInt264)
        . mkPackFsM
    )

int264Max :: Integer
int264Max = 2 ^ (263 :: Integer) - 1

int264Min :: Integer
int264Min = -int264Max

int264 :: Integer -> Fn s (s :> TInt264)
int264 x = assert (x >= int264Min && x <= int264Max) (int x . fromRaw)

fromRaw :: Fn (s :> TInt) (s :> TInt264)
fromRaw = cast

toRaw :: Fn (s :> TInt264) (s :> TInt)
toRaw = cast

toRaw2 :: Fn (s :> TInt264 :> TInt264) (s :> TInt :> TInt)
toRaw2 = castStack

toRaw3 :: Fn (s :> TInt264 :> TInt264 :> TInt264) (s :> TInt :> TInt :> TInt)
toRaw3 = castStack
