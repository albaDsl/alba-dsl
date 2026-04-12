-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.TBytes128
  ( TBytes128,
    fromBytes,
    bytes128,
    toBytes,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Bytes,
    Fn,
    StackEntry,
    TBytes,
    TInt,
    TNat,
    begin,
    bytes,
    cast,
    constant,
    fn,
    int,
    lambda1,
    n2i,
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
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqUtils
  ( blobEqEqual,
    blobEqEqualVerify,
    blobEqRecord,
  )
import Alba.Dsl.V1.Bch2026.Contract.PackFs (PackFs (..), TPackFs, mkPackFsM)
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, dup, nip)
import Control.Exception (assert)
import Data.ByteString qualified as B
import Numeric.Natural (Natural)
import Prelude (Int, Ord ((<=)), fromIntegral, (&&), (>=))

data TBytes128

instance StackEntry TBytes128

instance BlobEq TBytes128 where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance PackFs TBytes128 where
  pack = packTBytes128
  unpack = unpackTBytes128
  size = nat (sizeConst @TBytes128)
  sizeConst = fromIntegral packSize
  packFsRec = bytes128PackFs

bytes128PackFs :: Fn s (s > TPackFs TBytes128)
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

packTBytes128 :: Fn (s > TBytes128) (s > TBytes)
packTBytes128 =
  fn
    ( begin
        # ns "b128"
        # name "size" (pick "b128" # toRaw # opSize # nip)
        # (pick "size" # n2i # nat sizeFieldSize # opNum2Bin)
        # (roll "b128" # toRaw # int 0 # nat (fromIntegral maxPayloadSize))
        # (roll "size" # opSubUnsafe # opNum2Bin # opCat # opCat)
    )

unpackTBytes128 :: Fn (s > TBytes) (s > TBytes128)
unpackTBytes128 =
  fn
    ( begin
        # ns "bytes"
        # name2 "size" "rest" (roll "bytes" # nat sizeFieldSize # opSplit)
        # (roll "rest" # roll "size" # opBin2Num # i2n # opSplit # drop)
        # fromRaw
    )
  where
    i2n :: Fn (s > TInt) (s > TNat)
    i2n = cast

bytes128 :: Bytes -> Fn s (s > TBytes128)
bytes128 x =
  assert (B.length x >= 0 && B.length x <= maxPayloadSize) (bytes x # fromRaw)

fromBytes :: Fn (s > TBytes) (s > TBytes128)
fromBytes =
  fn
    ( begin
        # (dup # opSize # nip # nat (fromIntegral maxPayloadSize))
        # (opLessThanOrEqual # opVerify # fromRaw)
    )

toBytes :: Fn (s > TBytes128) (s > TBytes)
toBytes = toRaw

toRaw :: Fn (s > TBytes128) (s > TBytes)
toRaw = cast

fromRaw :: Fn (s > TBytes) (s > TBytes128)
fromRaw = cast
