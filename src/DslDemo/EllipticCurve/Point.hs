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
    fn,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( BlobEq (..),
    TEither,
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
    nip,
    right,
    tuple,
    unit,
    untuple,
  )
import DslDemo.EllipticCurve.Field (TFe, pushFe)
import Numeric.Natural (Natural)
import Prelude ()

data TPoint

instance StackEntry TPoint

instance BlobEq TPoint where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

makePoint :: Fn (s :> TFe :> TFe) (s :> TPoint)
makePoint = fn (tuple . right . fromRaw)

pushPoint :: Natural -> Natural -> Fn s (s :> TPoint)
pushPoint x y = pushFe x . pushFe y . makePoint

makeIdentity :: Fn s (s :> TPoint)
makeIdentity = unit . left . fromRaw

isIdentity :: Fn (s :> TPoint) (s :> TBool)
isIdentity = toRaw . isLeft

getXY :: Fn (s :> TPoint) (s :> TFe :> TFe)
getXY = fn (toRaw . ifLeft (drop . errPartialFunction) untuple)

getX :: Fn (s :> TPoint) (s :> TFe)
getX = getXY . drop

getY :: Fn (s :> TPoint) (s :> TFe)
getY = getXY . nip

fromRaw :: Fn (s :> TEither TUnit (TTuple TFe TFe)) (s :> TPoint)
fromRaw = cast

toRaw :: Fn (s :> TPoint) (s :> TEither TUnit (TTuple TFe TFe))
toRaw = cast
