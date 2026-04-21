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
    TInt,
    cast,
    fn,
    int,
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
import Prelude (Integer)

data TPoint

instance StackEntry TPoint

instance BlobEq TPoint where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

makePoint :: Fn (s :> TInt :> TInt) (s :> TPoint)
makePoint = fn (tuple . right . fromRaw)

pushPoint :: Integer -> Integer -> Fn s (s :> TPoint)
pushPoint x y = int x . int y . makePoint

makeIdentity :: Fn s (s :> TPoint)
makeIdentity = unit . left . fromRaw

isIdentity :: Fn (s :> TPoint) (s :> TBool)
isIdentity = toRaw . isLeft

getXY :: Fn (s :> TPoint) (s :> TInt :> TInt)
getXY = fn (toRaw . ifLeft (drop . errPartialFunction) untuple)

getX :: Fn (s :> TPoint) (s :> TInt)
getX = getXY . drop

getY :: Fn (s :> TPoint) (s :> TInt)
getY = getXY . nip

fromRaw :: Fn (s :> TEither TUnit (TTuple TInt TInt)) (s :> TPoint)
fromRaw = cast

toRaw :: Fn (s :> TPoint) (s :> TEither TUnit (TTuple TInt TInt))
toRaw = cast
