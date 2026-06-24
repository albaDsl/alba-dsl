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
    name,
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
    TInt264,
    TPackFs,
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
    rot,
    unit,
    unpad,
  )
import Alba.Dsl.V1.Bch2026.Contract.TTupleInt264
  ( TTupleInt264,
    tupleInt264,
    untupleInt264,
  )
import DslDemo.EllipticCurve.Field
  ( TFe,
    feInv,
    feMul,
    feSquare,
    fromTInt264,
    pushFe,
    toTInt264,
  )
import DslDemo.EllipticCurve.Point (TPoint)
import DslDemo.EllipticCurve.Point qualified as AP
import Numeric.Natural (Natural)
import Prelude ((*), (+))

data TPointJ

instance StackEntry TPointJ

instance BlobEq TPointJ where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance PackFs TPointJ where
  sizeConst = (sizeConst @TInt264) * 3 + 1 + 1 -- tuples, either, and pad size.
  size = nat (sizeConst @TPointJ)
  pack = size @TPointJ . pad
  unpack = unpad
  packFsRec = pointPackFs

type PointRaw = TEither TUnit (TTupleInt264 (TTupleInt264 TFe))

pointPackFs :: Fn s (s :> TPackFs TPointJ)
pointPackFs =
  constant
    ( begin
        . (size @TPointJ . quot1 (pack @TPointJ) . quot1 (unpack @TPointJ))
        . mkPackFsM
    )

makePoint :: Fn (s :> TFe :> TFe :> TFe) (s :> TPointJ)
makePoint =
  fn
    ( (rot . toTInt264 . rot . toTInt264 . rot . tupleInt264)
        . (tupleInt264 . right . fromRaw)
    )

pushPoint :: Natural -> Natural -> Natural -> Fn s (s :> TPointJ)
pushPoint x y z = pushFe x . pushFe y . pushFe z . makePoint

makeIdentity :: Fn s (s :> TPointJ)
makeIdentity = unit . left . fromRaw

isIdentity :: Fn (s :> TPointJ) (s :> TBool)
isIdentity = toRaw . isLeft

getXYZ :: Fn (s :> TPointJ) (s :> TFe :> TFe :> TFe)
getXYZ =
  fn
    ( toRaw
        . ifLeft
          (drop . errPartialFunction)
          ( (untupleInt264 . untupleInt264 . rot . fromTInt264 . rot)
              . (fromTInt264 . rot)
          )
    )

fromRaw :: Fn (s :> PointRaw) (s :> TPointJ)
fromRaw = cast

toRaw :: Fn (s :> TPointJ) (s :> PointRaw)
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
              . (name #zInv (roll #z . feInv))
              . (name #zInv2 (pick #zInv . feSquare))
              . (roll #x . pick #zInv2 . feMul)
              . (roll #y . roll #zInv2 . roll #zInv . feMul . feMul)
              . AP.makePoint
          )
    )
