-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Int8 (TInt8, int8, toInt8) where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    StackEntry,
    StackNum,
    TInt,
    begin,
    cast,
    constant,
    int,
    lambda1,
    nat,
    opBin2Num,
    opNum2Bin,
    opVerify,
    opWithin,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.PackFs (PackFs (..), TPackFs, mkPackFsM)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (dup)
import Control.Exception (assert)

data TInt8

instance StackEntry TInt8

-- We currently allow for standard arithmetic ops. Overflow is caught on
-- attempts to pack the datatype.
instance StackNum TInt8

instance PackFs TInt8 where
  sizeConst = 1
  size = nat (sizeConst @TInt8)
  pack = cast # size @TInt8 # opNum2Bin
  unpack = opBin2Num # cast
  record = int8PackFs

int8PackFs :: Fn s (s > TPackFs TInt8)
int8PackFs =
  constant
    ( begin
        # size @TInt8
        # lambda1 (pack @TInt8)
        # lambda1 (unpack @TInt8)
        # mkPackFsM
    )

int8Max :: Integer
int8Max = 127

int8Min :: Integer
int8Min = -int8Max

int8 :: Integer -> Fn s (s > TInt8)
int8 x = assert (x >= int8Min && x <= int8Max) (int x # cast)

toInt8 :: Fn (s > TInt) (s > TInt8)
toInt8 = dup # int int8Min # int int8Max # opWithin # opVerify # cast
