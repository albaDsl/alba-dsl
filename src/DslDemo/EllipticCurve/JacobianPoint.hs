-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.JacobianPoint
  ( TPointJ,
    makePoint,
    pushPoint,
    makeIdentity,
    isIdentity,
    getXYZ,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack (..),
    StackEntry,
    TBool,
    TInt,
    begin,
    cast,
    constant,
    fn,
    int,
    lambda1,
    nat,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( BlobEq (..),
    PackFs (..),
    TEither,
    TPackFs,
    TTuple,
    TUnit,
    blobEqEqual,
    blobEqEqualVerify,
    blobEqRecord,
    drop,
    errPartialFunction,
    ifLeft,
    isLeft,
    left,
    mkPackFsM,
    pad,
    right,
    tuple,
    unit,
    unpad,
    untuple,
  )
import Prelude (Integer)

data TPointJ

instance StackEntry TPointJ

instance BlobEq TPointJ where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance PackFs TPointJ where
  sizeConst = 105 -- 104 + 1 for the pad tag.
  size = nat (sizeConst @TPointJ)
  pack = size @TPointJ . pad
  unpack = unpad
  packFsRec = int8PackFs

int8PackFs :: Fn s (s :> TPackFs TPointJ)
int8PackFs =
  constant
    ( begin
        . (size @TPointJ . lambda1 (pack @TPointJ) . lambda1 (unpack @TPointJ))
        . mkPackFsM
    )

makePoint :: Fn (s :> TInt :> TInt :> TInt) (s :> TPointJ)
makePoint = fn (tuple . tuple . right . fromRaw)

pushPoint :: Integer -> Integer -> Integer -> Fn s (s :> TPointJ)
pushPoint x y z = int x . int y . int z . makePoint

makeIdentity :: Fn s (s :> TPointJ)
makeIdentity = unit . left . fromRaw

isIdentity :: Fn (s :> TPointJ) (s :> TBool)
isIdentity = toRaw . isLeft

getXYZ :: Fn (s :> TPointJ) (s :> TInt :> TInt :> TInt)
getXYZ = fn (toRaw . ifLeft (drop . errPartialFunction) (untuple . untuple))

fromRaw ::
  Fn
    (s :> TEither TUnit (TTuple TInt (TTuple TInt TInt)))
    (s :> TPointJ)
fromRaw = cast

toRaw ::
  Fn
    (s :> TPointJ)
    (s :> TEither TUnit (TTuple TInt (TTuple TInt TInt)))
toRaw = cast
