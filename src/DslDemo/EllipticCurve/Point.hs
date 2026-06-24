-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Point
  ( TPoint,
    makePoint,
    pushPoint,
    makeIdentity,
    isIdentity,
    getX,
    getY,
    getXY,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack (..),
    StackEntry,
    TBool,
    cast,
    constant,
    fn,
    nat,
    quot1,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( BlobEq (..),
    PackFs (..),
    TEither,
    TInt264,
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
    nip,
    pad,
    right,
    swap,
    unit,
    unpad,
  )
import Alba.Dsl.V1.Bch2026.Contract.TTupleInt264
  ( TTupleInt264,
    tupleInt264,
    untupleInt264,
  )
import DslDemo.EllipticCurve.Field (TFe, fromTInt264, pushFe, toTInt264)
import Numeric.Natural (Natural)
import Prelude ((*), (+))

data TPoint

instance StackEntry TPoint

instance BlobEq TPoint where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

instance PackFs TPoint where
  sizeConst = (sizeConst @TInt264) * 2 + 1 + 1 -- tuple, either, and pad size.
  size = nat (sizeConst @TPoint)
  pack = size @TPoint . pad
  unpack = unpad
  packFsRec = constant (size @TPoint . quot1 pack . quot1 unpack . mkPackFsM)

type PointRaw = TEither TUnit (TTupleInt264 TFe)

makePoint :: Fn (s :> TFe :> TFe) (s :> TPoint)
makePoint = fn (swap . toTInt264 . swap . tupleInt264 . right . fromRaw)

pushPoint :: Natural -> Natural -> Fn s (s :> TPoint)
pushPoint x y = pushFe x . pushFe y . makePoint

makeIdentity :: Fn s (s :> TPoint)
makeIdentity = unit . left . fromRaw

isIdentity :: Fn (s :> TPoint) (s :> TBool)
isIdentity = toRaw . isLeft

getXY :: Fn (s :> TPoint) (s :> TFe :> TFe)
getXY =
  fn
    ( toRaw
        . ifLeft
          (drop . errPartialFunction)
          (untupleInt264 . swap . fromTInt264 . swap)
    )

getX :: Fn (s :> TPoint) (s :> TFe)
getX = getXY . drop

getY :: Fn (s :> TPoint) (s :> TFe)
getY = getXY . nip

fromRaw :: Fn (s :> PointRaw) (s :> TPoint)
fromRaw = cast

toRaw :: Fn (s :> TPoint) (s :> PointRaw)
toRaw = cast
