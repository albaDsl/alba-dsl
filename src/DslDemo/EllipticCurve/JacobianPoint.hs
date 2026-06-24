-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.JacobianPoint
  ( TPointJ,
    makePoint,
    pushPoint,
    makeIdentity,
    isIdentity,
    getXYZ,
    toJacobian,
    fromJacobian,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack ((:>)),
    StackEntry,
    TBool,
    begin,
    cast,
    constant,
    del,
    fn,
    name3,
    nat,
    ns,
    opIf,
    pick,
    quot1,
    roll,
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
import DslDemo.EllipticCurve.Field (TFe, feCube, feInv, feMul, feSquare, pushFe)
import DslDemo.EllipticCurve.Point (TPoint)
import DslDemo.EllipticCurve.Point qualified as AP
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
        . (size @TPointJ . quot1 (pack @TPointJ) . quot1 (unpack @TPointJ))
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

toJacobian :: Fn (s :> TPoint) (s :> TPointJ)
toJacobian =
  fn
    ( begin
        . (ns #p . pick #p . AP.isIdentity)
        . opIf
          (del #p . makeIdentity)
          (roll #p . AP.getXY . pushFe 1 . makePoint)
    )

fromJacobian :: Fn (s :> TPointJ) (s :> TPoint)
fromJacobian =
  fn
    ( begin
        . (ns #p . pick #p . isIdentity)
        . opIf
          (del #p . AP.makeIdentity)
          ( begin
              . name3 #x #y #z (roll #p . getXYZ)
              . (roll #x . pick #z . feSquare . feInv . feMul)
              . (roll #y . roll #z . feCube . feInv . feMul)
              . AP.makePoint
          )
    )
