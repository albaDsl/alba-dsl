-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.TInt64 (TInt64, int64, toInt64) where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    StackEntry,
    StackNum,
    TInt,
    begin,
    cast,
    constant,
    fn,
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
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqUtils
  ( blobEqEqual,
    blobEqEqualVerify,
    blobEqRecord,
  )
import Alba.Dsl.V1.Bch2026.Contract.PackFs (PackFs (..), TPackFs, mkPackFsM)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (dup)
import Control.Exception (assert)

data TInt64

instance StackEntry TInt64

-- We currently allow for standard arithmetic ops. Overflow is caught on
-- attempts to pack the datatype.
instance StackNum TInt64

instance PackFs TInt64 where
  sizeConst = 8
  size = nat (sizeConst @TInt64)
  pack = cast # size @TInt64 # opNum2Bin
  unpack = opBin2Num # cast
  packFsRec = int64PackFs

instance BlobEq TInt64 where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

int64PackFs :: Fn s (s > TPackFs TInt64)
int64PackFs =
  constant
    ( begin
        # size @TInt64
        # lambda1 (pack @TInt64)
        # lambda1 (unpack @TInt64)
        # mkPackFsM
    )

int64Max :: Integer
int64Max = 9223372036854775807

int64Min :: Integer
int64Min = -int64Max

int64 :: Integer -> Fn s (s > TInt64)
int64 x = assert (x >= int64Min && x <= int64Max) (int x # cast)

toInt64 :: Fn (s > TInt) (s > TInt64)
toInt64 = fn (dup # int int64Min # int int64Max # opWithin # opVerify # cast)
