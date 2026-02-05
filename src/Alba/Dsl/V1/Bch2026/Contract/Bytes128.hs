-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Bytes128
  ( TBytes128,
    toBytes128,
    bytes128,
    toBytes,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Bytes,
    FN,
    StackEntry,
    TBytes,
    TInt,
    TNat,
    begin,
    bytes,
    cast,
    constant,
    int,
    lambda1,
    name,
    name2,
    nat,
    ns,
    opBin2Num,
    opCat,
    opLessThanOrEqual,
    opNum2Bin,
    opSize,
    opSplit,
    opSubUnsafe,
    opVerify,
    pick,
    roll,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.PackFs (PackFs (..), TPackFs, mkPackFsM)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, dup, nip)
import Control.Exception (assert)
import Data.ByteString qualified as B
import Numeric.Natural (Natural)
import Prelude (Int, Ord ((<=)), fromIntegral, (&&), (>=))

data TBytes128

instance StackEntry TBytes128

instance PackFs TBytes128 where
  pack = packTBytes128
  unpack = unpackTBytes128
  size = nat (sizeConst @TBytes128)
  sizeConst = fromIntegral packSize
  record = bytes128PackFs

bytes128PackFs :: FN s (s > TPackFs TBytes128)
bytes128PackFs =
  constant
    ( begin
        # size @TBytes128
        # lambda1 (pack @TBytes128)
        # lambda1 (unpack @TBytes128)
        # mkPackFsM
    )

maxPayloadSize :: Int
maxPayloadSize = 128

packSize :: Int
packSize = 130

sizeFieldSize :: Natural
sizeFieldSize = 2

packTBytes128 :: FN (s > TBytes128) (s > TBytes)
packTBytes128 =
  begin
    # ns "b128"
    # name "size" (pick "b128" # toBytes # opSize # nip)
    # (pick "size" # cast # nat sizeFieldSize # opNum2Bin)
    # (roll "b128" # toBytes # int 0 # nat (fromIntegral maxPayloadSize))
    # (roll "size" # opSubUnsafe # opNum2Bin # opCat # opCat)

unpackTBytes128 :: FN (s > TBytes) (s > TBytes128)
unpackTBytes128 =
  begin
    # ns "bytes"
    # name2 "size" "rest" (roll "bytes" # nat 2 # opSplit)
    # (roll "rest" # roll "size" # opBin2Num # i2n # opSplit # drop # cast)

bytes128 :: Bytes -> FN s (s > TBytes128)
bytes128 x =
  assert (B.length x >= 0 && B.length x <= maxPayloadSize) (bytes x # cast)

toBytes128 :: FN (s > TBytes) (s > TBytes128)
toBytes128 =
  begin
    # (dup # opSize # nip # nat (fromIntegral maxPayloadSize))
    # (opLessThanOrEqual # opVerify # cast)

i2n :: FN (s > TInt) (s > TNat)
i2n = cast

toBytes :: FN (s > TBytes128) (s > TBytes)
toBytes = cast
