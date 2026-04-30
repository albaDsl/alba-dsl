-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.TInt16 (TInt16, int16) where

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
import Alba.Dsl.V1.Bch2026.Contract.Ord (Ord (..))
import Alba.Dsl.V1.Bch2026.Contract.PackFs (PackFs (..), TPackFs, mkPackFsM)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (dup)
import Control.Exception (assert)
import Prelude (Integer, undefined, (&&), (-), (<=), (>=), (^))

data TInt16

instance StackEntry TInt16

instance BlobEq TInt16 where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance Ord TInt16 where
  lessThan = toRaw2 . opLessThan
  lessThanOrEqual = toRaw2 . opLessThanOrEqual
  greaterThan = toRaw2 . opGreaterThan
  greaterThanOrEqual = toRaw2 . opGreaterThanOrEqual
  min = toRaw2 . opMin . fromRaw
  max = toRaw2 . opMax . fromRaw
  within = toRaw3 . opWithin
  blobOrdRec = undefined -- FIXME: implement.

instance Integral TInt16 where
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
    fn (dup . int int16Min . int int16Max . opWithin . opVerify . fromRaw)
  toInt = toRaw

instance PackFs TInt16 where
  sizeConst = 2
  size = nat (sizeConst @TInt16)
  pack = toRaw . size @TInt16 . opNum2Bin
  unpack = opBin2Num . fromRaw
  packFsRec = int16PackFs

int16PackFs :: Fn s (s :> TPackFs TInt16)
int16PackFs =
  constant
    ( begin
        . size @TInt16
        . lambda1 (pack @TInt16)
        . lambda1 (unpack @TInt16)
        . mkPackFsM
    )

int16Max :: Integer
int16Max = 2 ^ (15 :: Integer) - 1

int16Min :: Integer
int16Min = -int16Max

int16 :: Integer -> Fn s (s :> TInt16)
int16 x = assert (x >= int16Min && x <= int16Max) (int x . fromRaw)

fromRaw :: Fn (s :> TInt) (s :> TInt16)
fromRaw = cast

toRaw :: Fn (s :> TInt16) (s :> TInt)
toRaw = cast

toRaw2 :: Fn (s :> TInt16 :> TInt16) (s :> TInt :> TInt)
toRaw2 = castStack

toRaw3 :: Fn (s :> TInt16 :> TInt16 :> TInt16) (s :> TInt :> TInt :> TInt)
toRaw3 = castStack
