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
    begin,
    cast,
    constant,
    fn,
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
import DslDemo.EllipticCurve.Field (TFe, pushFe)
import Numeric.Natural (Natural)
import Prelude ()

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
  packFsRec = pointPackFs

pointPackFs :: Fn s (s :> TPackFs TPointJ)
pointPackFs =
  constant
    ( begin
        . (size @TPointJ . lambda1 (pack @TPointJ) . lambda1 (unpack @TPointJ))
        . mkPackFsM
    )

makePoint :: Fn (s :> TFe :> TFe :> TFe) (s :> TPointJ)
makePoint = fn (tuple . tuple . right . fromRaw)

pushPoint :: Natural -> Natural -> Natural -> Fn s (s :> TPointJ)
pushPoint x y z = pushFe x . pushFe y . pushFe z . makePoint

makeIdentity :: Fn s (s :> TPointJ)
makeIdentity = unit . left . fromRaw

isIdentity :: Fn (s :> TPointJ) (s :> TBool)
isIdentity = toRaw . isLeft

getXYZ :: Fn (s :> TPointJ) (s :> TFe :> TFe :> TFe)
getXYZ = fn (toRaw . ifLeft (drop . errPartialFunction) (untuple . untuple))

fromRaw ::
  Fn
    (s :> TEither TUnit (TTuple TFe (TTuple TFe TFe)))
    (s :> TPointJ)
fromRaw = cast

toRaw ::
  Fn
    (s :> TPointJ)
    (s :> TEither TUnit (TTuple TFe (TTuple TFe TFe)))
toRaw = cast
